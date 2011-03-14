# getPairs: methods for retreiving data pairs from a result set or
# data object


# utility function

# internal function to construct SQL query for getting record pairs
# Args:
#   object: RLBigData object
#
# The returned statement is a prepared statement if weight limits are used
# (i.e. a finite value for at least one of max.weight and min.weight)

getPairsBackend <- function(object, filter.match,
  filter.link=c("nonlink", "possible", "link"), max.weight=Inf,
  min.weight=-Inf, withMatch = TRUE, withClass = FALSE, withWeight = FALSE,
  sort=FALSE, single.rows=FALSE)
{
    if (is(object, "RLResult"))
      object <- object@data
      
    stmtList <- getSQLStatement(object)
    select_list <- stmtList$select_list
    from_clause <- stmtList$from_clause
#    from_clause <- gsub("join", "cross join", stmtList$from_clause)
    where_clause <- stmtList$where_clause

    # get column names either from slot data or data1, depending on the class
    # (a more robust way would be good)
    colN <- switch(class(object),
      RLBigDataDedup = colnames(object@data),
      RLBigDataLinkage = colnames(object@data1),
      stop(paste("Unexpected class of object:", class(object)))
    )

    # convert to database column names and add ids
    dbNames <- make.db.names(object@drv, colN, allow.keywords = FALSE)
    dbNames <- c("row_names", dbNames)

    # concatenate fields of first table, fields of second table
    # in the format t1.field1, t1.field2, ..., t2.field1, t2.field2, ...
    select_list <- paste(sapply(c("t1", "t2"), function(tableName)
        sapply(dbNames, function(fieldName) sprintf("%s.%s", tableName, fieldName))
      ), collapse=", "
    )
    if (withMatch)
    {
      select_list <- paste(select_list, "t1.identity=t2.identity as is_match",
        sep =", ")
    }
    # Include filtering by linkage result.
    # Implementation: The pairs are left joined with tables links and
    # possible_links. Links and possible links will have non-null values in the
    # columns of the corresponding table. A column "class" will be included in
    # the output which can take the values 1 (non-link), 2 (possible) and 3
    # (link), which corresponds to the factor levels "N", "P", "L"

    # The join is necessary for filtering of a distinct classification
    # and for displaying the result
    if (withClass || any(is.na(match(c("link", "nonlink", "possible"),filter.link))))
    {
      from_clause <- paste(from_clause,
        "left join links l on (t1.row_names=l.id1 and t2.row_names=l.id2)",
      	"left join possible_links p on (t1.row_names=p.id1 and t2.row_names=p.id2)"
      )
    }

    if (withClass)
    {
      # This expression evaluates to 1 for non-matches, 2 (=1+1) for possible
      # matches and 3 (=1+2) for matches under the constraint that a pair can
      # not be a possible match and a match at one time.
      # The values can later easily be transformed to factor levels "N", "P", "L"
      select_list <- paste(select_list,
        "1 + (p.id1 is not null) + (l.id1 is not null) * 2 as class",
        sep =", ")
    }

    # Join with table of weights necessary if a weight range is given
    # or weights are to be included in the output
    # For a given weight range, force evaluation of the weight index by
    # using cross join
    if (is.finite(max.weight) || is.finite(min.weight))
    {
      from_clause <- paste("Wdata weights cross join", from_clause)
      where_clause <- paste( "t1.row_names=weights.id1",
        "and t2.row_names=weights.id2 and",
        where_clause)
    } else if (withWeight)
    {
      from_clause <- paste(
        from_clause, "Wdata weights",
        sep = ", ")
      where_clause <- paste( "t1.row_names=weights.id1",
        "and t2.row_names=weights.id2 and",
        where_clause)
    }

    if (withWeight)
    {
      if (!dbExistsTable(object@con, "Wdata"))
        stop(paste("No weights have been calculated for object!"))
      select_list <- paste(select_list,
        "weights.W as Weight", sep=",")

    }

    # add restrictions concerning matching status
    filterMatchFun <- function(filterElem)
    {
      switch(filterElem,
        match = "t1.identity=t2.identity",
        nonmatch = "t1.identity!=t2.identity",
        unknown = "t1.identity is null or t2.identity is null"
      )
    }
    # if no restriction is made (show matches, nonmatches and unknown),
    # do not add any clause
    if (any(is.na(match(c("match", "nonmatch", "unknown"), filter.match))))
    {
      filterMatch <- paste(sapply(filter.match, filterMatchFun), collapse=" or ")
    } else
    {
      filterMatch = "1"
    }

    if (is.finite(max.weight) || is.finite(min.weight))
    {
      weight_clause <- "weights.W between :min and :max"
    } else
    {
      weight_clause <- "1"
    }


    # Add restrictions concerning classification
    # A pair is a link if the left join with the table of links gives null
    # columns (same holds for possible links)
    filterLinkFun <- function(filterElem)
    {
      switch(filterElem,
        link = "l.id1 is not null",
        possible = "p.id1 is not null",
        nonlink = "(l.id1 is null and p.id1 is null)"
      )
    }
    # if no restriction is made (show matches, nonmatches and unknown),
    # do not add any clause
    if (any(is.na(match(c("link", "nonlink", "possible"), filter.link))))
    {
      filterLink <- paste(sapply(filter.link, filterLinkFun), collapse=" or ")
    } else
    {
      filterLink = "1"
    }

    if (sort)
    {
      order_clause = "order by Weight desc"
    } else order_clause=""
    
    # construct statement
    stmt <- sprintf("select %s from %s where %s and (%s) and (%s) and %s %s", select_list,
      from_clause, where_clause, filterMatch, filterLink, weight_clause, order_clause)
#    message(stmt  )
#    print(dbGetPreparedQuery(object@con, paste("explain query plan", stmt),
#      data.frame(min=min.weight, max=max.weight)))

    result <- dbGetPreparedQuery(object@con, stmt, data.frame(min=min.weight, max=max.weight))

#    if(nrow(result)==0)
#      return (NULL)

    cnames <- c("id.1", paste(colN, ".1", sep=""), "id.2",
      paste(colN, ".2", sep=""))

    if (withMatch)
      cnames <- c(cnames, "is_match")
    if (withClass)
      cnames <- c(cnames, "Class")
    if (withWeight)
      cnames <- c(cnames, "Weight")

    colnames(result) <- cnames

    # converion of SQLite coding to more apropriate types
    # double conversion is necessary for cases when the first row has NA
    # as matching status. This causes RSQLite to cast the column to character
    # (see comment for RS_SQLite_fetch in package RSQLite, file src/RS-SQLite.c)
    # and finally to the unintended conversion "0" -> NA / "1" -> NA
    if (withMatch)
      result$is_match <- as.logical(as.numeric(result$is_match))
    if (withClass)
    {
      result$Class <- factor(result$Class, levels=1:3)
      levels(result$Class) <- c("N", "P", "L")
    }


    if(single.rows)
      result
    else
    {

      cnames=c("id",
        colnames(switch(class(object),
          RLBigDataDedup = object@data,
          RLBigDataLinkage = object@data1,
          stop(paste("Unexpected class of object:", class(object)))
        )))
      if (withMatch)
        cnames <- c(cnames, "is_match")
      if (withClass)
        cnames <- c(cnames, "Class")
      if (withWeight)
        cnames <- c(cnames, "Weight")

      if (nrow(result)==0)
        return(data.frame(matrix(nrow=0, ncol=length(cnames),
          dimnames=list(character(0), cnames))))
      # if pairs are to be printed on consecutive lines, some formatting is
      # necassery

      # This function inserts some white space:
      #   1. The second row of every pair has possible the matching status,
      #       classification and weight, the other one blank fields
      #   2. A line of white space seperates record pairs

      # compute number of additional fields (weight etc)
      nAdditional <- as.numeric(withMatch) + as.numeric(withWeight) + as.numeric(withClass)
    	printfun=function(x)
      {
        c(x[1:((length(x)-nAdditional)/2)],rep("", nAdditional),
          x[((length(x)-nAdditional)/2 + 1):length(x)],
          rep("", (length(x)+nAdditional)/2)) # blank line

      }

      # Apply helper function to every line
      m=apply(result,1,printfun)
      # reshape result into a table of suitable format
      m=as.data.frame(matrix(m[TRUE],nrow=ncol(m)*3,ncol=nrow(m)/3,byrow=TRUE))


      colnames(m) <- cnames


      return(m)
  } # end else
}


setGeneric(
  name = "getPairs",
  def = function(object, ...) standardGeneric("getPairs")
)

setMethod(
  f = "getPairs",
  signature = "RLBigData",
  definition = function(object, max.weight = Inf, min.weight = -Inf,
    filter.match = c("match", "unknown", "nonmatch"),
    withWeight = dbExistsTable(object@con, "Wdata"), withMatch = TRUE,
    single.rows = FALSE, sort = TRUE)
  {
  
    # check arguments
    if (!is.logical(single.rows) || is.na(single.rows))
      stop(paste("Illegal value for single.rows:", single.rows))

    if (!is.character(filter.match))
      stop(paste("Illegal class for filter.match:", class(filter.match)))

    if (any(naind <- is.na(match(filter.match, c("match", "unknown", "nonmatch")))))
      stop(paste("Illegal value in filter.match:", filter.match[naind]))


    # call backend function
    getPairsBackend(object, filter.match=filter.match, max.weight = max.weight,
      min.weight = min.weight, withWeight = withWeight, withMatch = withMatch,
      sort = withWeight, single.rows = single.rows)

  }
)



setMethod(
  f = "getPairs",
  signature = "RLResult",
  definition = function(object, filter.match = c("match", "unknown", "nonmatch"),
    filter.link = c("nonlink", "possible", "link"), max.weight = Inf, min.weight = -Inf,
    withMatch = TRUE, withClass=TRUE, withWeight=dbExistsTable(object@data@con, "Wdata"),
    single.rows = FALSE, sort=withWeight)
  {
    # assing data base connection to local variable
    con <- object@data@con
    # insert match result in database and create indeces to speed up lookup
    # dbWrite does not create table if an empty data frame is passed,
    # create empty table in this case
    dbGetQuery(con, "drop table if exists links")
    dbGetQuery(con, "create table links (id1 integer, id2 integer)")
    dbGetQuery(con, "drop table if exists possible_links")
    dbGetQuery(con, "create table possible_links (id1 integer, id2 integer)")
    if (nrow(object@links) > 0)
    {
      dbWriteTable(con, "links", as.data.frame(object@links), append=TRUE,
        row.names=FALSE)
    }
    if (nrow(object@possibleLinks) > 0)
    {
      dbWriteTable(con, "possible_links", as.data.frame(object@possibleLinks), append=TRUE,
        row.names=FALSE)
    }

    dbGetQuery(object@data@con, "create index index_links on links(id1, id2)")
    dbGetQuery(object@data@con, "create index index_possible on possible_links(id1, id2)")

    # call backend function
    res <- getPairsBackend(object, filter.match=filter.match, filter.link = filter.link,
      max.weight = max.weight, min.weight = min.weight, withMatch = withMatch,
      withClass = withClass, withWeight = withWeight,
      sort = withWeight, single.rows = single.rows)

    # clean up temporary tables
    dbGetQuery(object@data@con, "drop table links")
    dbGetQuery(object@data@con, "drop table possible_links")
    
    res
  }
)



# traditional function
setMethod(
  f = "getPairs",
  signature = "RecLinkData",
  definition = function(object, max.weight = Inf, min.weight = -Inf,
         single.rows = FALSE, show = "all", sort = !is.null(object$Wdata))
  {
    # rename object to keep old code
    rpairs <- object
    if (!("RecLinkData" %in% class(rpairs) ||
      "RecLinkResult" %in% class(rpairs)))
      stop("Wrong class for rpairs!")

    if (!is.numeric(max.weight))
      stop(paste("Illegal type for max.weight: ", class(max.weight)))

    if (!is.numeric(min.weight))
      stop(paste("Illegal type for min.weight: ", class(min.weight)))

    if (max.weight <=min.weight)
      stop("max.weight must be greater than min.weight!")

    if (!is.character(show))
      stop(paste("Illegal type for show:", class(show)))

    if (!is.element(show, c("all","links","nonlinks","possible")))
      stop(paste("Illegal value for show:", show))

    if (rpairs$type=="deduplication")
    {
        data1=rpairs$data
        data2=data1
    } else
    {
        data1=rpairs$data1
        data2=rpairs$data2
    }

  	if (!is.null(rpairs$Wdata))
    {
      ind=which(rpairs$Wdata <= max.weight & rpairs$Wdata >= min.weight)
      weights <- rpairs$Wdata
    }
    else
    {
      ind <- 1:nrow(rpairs$pairs)
      weights <- rep(NA, nrow(rpairs$pairs))
    }

  	if (!is.null(rpairs$prediction))
  	{
  		show.ind=switch(show,links=which(rpairs$prediction[ind]=="L"),
  			nonlinks=which(rpairs$prediction[ind]=="N"),
   			possible=which(rpairs$prediction[ind]=="P"),
        FP=which(rpairs$prediction=="L" & rpairs$pairs$is_match==FALSE),
  			FN=which(rpairs$prediction=="N" & rpairs$pairs$is_match==TRUE),
        TRUE)
  		ind=ind[show.ind]
  	} else if (show != "all" && is.null(rpairs$prediction))
  	{
      warning("No prediction vector found, returning all data pairs!")
    }


    pairs=data.frame(id1=rpairs$pairs[ind,1],
                    data1[rpairs$pairs[ind,1],],
                    id2=rpairs$pairs[ind,2],
                    data2[rpairs$pairs[ind,2],],
                    Weight=weights[ind])


  	if (isTRUE(sort))
  	{
      	o=order(pairs$Weight,decreasing=TRUE)
      	pairs=pairs[o,]
    }

  	if (single.rows)
  	{
    	# if no pairs at all meet the restrictions, empty frame
      if (is.na(ind) || length(ind)==0)
      {
        pairs <- pairs[0,]
      }
    	colnames(pairs)=c("id1", paste(colnames(data1),".1",sep=""),
  								   "id2", paste(colnames(data2),".2",sep=""), "Weight")
  		return (pairs)
  	}

  	printfun=function(x)
    {
      c(x[1:((length(x)-1)/2)],c("",x[((length(x)+1)/2):length(x)]),
      rep("", (length(x) + 1) / 2))

    }
    m=apply(pairs,1,printfun)
    m=as.data.frame(matrix(m[TRUE],nrow=ncol(m)*3,ncol=nrow(m)/3,byrow=TRUE))
    colnames(m)=c("id", colnames(data1), "Weight")
  	# if no pairs at all meet the restrictions, empty frame
    if (is.na(ind) || length(ind)==0)
    {
      m <- m[0,]
    }

    return(m)
  }
)


# shortcuts for retreiving pairs with wrong classification

getFalsePos <- function(object, single.rows=FALSE)
{
  getPairs(object, filter.link = "link", filter.match = "nonmatch",
    single.rows = single.rows)
}

getFalseNeg <- function(object, single.rows=FALSE)
{
  getPairs(object, filter.link = "nonlink", filter.match = "match",
    single.rows = single.rows)
}

getFalse <- function(object, single.rows=FALSE)
{
  rbind(
    getFalsePos(object, single.rows = single.rows),
    getFalseNeg(object, single.rows = single.rows)
  )
}
