## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## returns a list containing a function to set the matrix
##to get the matrix, set the inverse of the matrix and get
##inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) inv<<-inverse
  getinverse<-function() inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}
## Write a short comment describing this function

##Below function will get the inverse of the matrix only if it has not
##not been computed before otherwise it will return the cached value
cacheSolve <- function(x, ...) {
    inv<-x$getinverse()
    if(!is.null(inv)){
      message("getting cached data")
      return(inv)
    }
    mat<-x$get()
    inv<-solve(mat)
    x$setinverse(inv)
    inv
}
