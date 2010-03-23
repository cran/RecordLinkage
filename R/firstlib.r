.First.lib <- function(lib,pkg)
{
   library.dynam("RecordLinkage",pkg,lib)
   cat("RecordLinkage library\n")
   cat("[c] IMBEI Mainz\n")
}
