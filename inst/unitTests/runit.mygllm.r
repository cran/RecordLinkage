
# test EM implementation with example from package gllm by David Duffy

test.mygllm <- function()
{
  #
  # An example of latent class analysis: 2 latent classes
  #
  # Data matrix
  #
  y<-c( 3,   6,   2,   11,   1,   1,   3,    4,
        1,   8,   0,   16,   0,   3,   2,   15,
       10,  29,  14,   81,   3,  28,  15,   80,
       16,  56,  21,  173,  11,  61,  28,  298)
  #
  # Scatter matrix
  #
  s<-  c(1:32,1:32)
  #
  # Design matrix
  #
  i<-rep(1,64)
  x<-as.integer(gl(2,32,64))-1
  a<-as.integer(gl(2,16,64))-1
  b<-as.integer(gl(2,8 ,64))-1
  c<-as.integer(gl(2,4 ,64))-1
  d<-as.integer(gl(2,2 ,64))-1
  e<-as.integer(gl(2,1 ,64))-1
  X<-cbind(i,x,a,b,c,d,e,x*cbind(a,b,c,d,e))
  
  result <- mygllm(y,s,X,maxit=1000,tol=0.00001)
  
  expected <- read.table("result.gllm.txt")
  checkEqualsNumeric(result, expected[[1]], tol=0.01)  
}