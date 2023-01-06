makeCacheMatrix<-function(len=numberic, minimum=numeric, maximum=numerix){
  mat<-null
  set <- function(){
      mat<-matrix(runif(n=len*len, min=minimum, max=maximum), nrow=len)
  }
  get<-function()mat
  cacheInverse<- function{
    inver<-mat$inv()
    if(!isnull(inver)){
      message('getting cached inverse')
      return inver
    }
  }
}
cacheSolve<-function(mat=matrix){
  inver <-null 
  if (!isnull(cacheInverse(mat)){
    inver<-cacheInverse(mat)
  }
  else {
    inver<-mat$inv()
  }
}