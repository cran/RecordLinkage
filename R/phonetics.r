# phonetic.r: functions for phonetic coding

soundex <- function(str)
{
  # check type
  if (typeof(str) != "character" && !is(str, "factor"))
     stop(sprintf("Illegal data type: %s", typeof(str)))
  if (is(str, "factor"))
    str=as.character(str)
   out <- .C(".soundex_sym", as.character(str), ans=character(length(str)),length(str),
             PACKAGE="RecordLinkage")
   if (any(is.na(str)))
    out$ans[is.na(str)]=NA
   dim(out$ans)=dim(str)
   dimnames(out$ans)=dimnames(str)
   return(out$ans)
}
