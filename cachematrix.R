## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix object from the given input matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function () x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## This function returns the pre-calculated inverse, or calculates it if needed

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) message("Getting cached inverse")
  else {
    inv <- solve(x$get())
    x$setinv(inv)
  }
  inv
}
