## The following code returns the inverse f a matrix and caches it,
#if the inverse of a particular matrix is already cached then the 
#cached value is returned.


## This function creates a special "matrix" object that can cache 
#its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-matrix(data=NA,nrow = nrow(x),ncol = ncol(x))
  set<-function(y){
    x<<-matrix(data=y,nrow = nrow(y),ncol = ncol(y))
    inv<<-matrix(data=NA,nrow = nrow(y),ncol = ncol(y))
  }
  get<-function() return(x)
  setinv<-function(invers){inv<<-invers}
  getinv<-function() inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. If the inverse has already 
#been calculated (and the matrix has not changed), then the 
#cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv<-x$getinv()
  if(!is.null(m)){
    print("Returning the cached value")
    return(inv)
  }
  orig<-x$get()
  inv<-solve(orig)
  x$setinv(inv)
  inv
}
