# tools.r: varios utility functions

unorderedPairs <- function (x) 
{
    if (length(x)==1)
    {
      if (!is.numeric(x) || x < 2)
        stop("x must be a vector or a number >= 2")
        return (array(unlist(lapply(1:(x-1),
          function (k) rbind(k,(k+1):x))),dim=c(2,x*(x-1)/2)))
    }
    if (!is.vector(x))
      stop ("x must be a vector or a number >= 2")
    n=length(x)
    return (array(unlist(lapply(1:(n-1),
    function (k) rbind(x[k],x[(k+1):n]))),dim=c(2,n*(n-1)/2)))
}

isFALSE <- function(x) identical(x,FALSE)

delete.NULLs  <-  function(x)
    x[unlist(lapply(x, length) != 0)]

resample <- function(x, size, ...)
     if(length(x) <= 1) { if(!missing(size) && size == 0) x[FALSE] else x
     } else sample(x, size, ...)

