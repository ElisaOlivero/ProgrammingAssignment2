## This file contains two functions that create a special 
#  object that stores a numeric matrix and caches its inverse.
#  The functions assume that the matrix provided as input
#  is always invertible.
#  Example provided for testing:
#  X<-replicate(10, rnorm(10)) 
#  y<-makeCacheMatrix(X)
#  cacheSolve(y)
#  cacheSolve(y)

## This function creates a special matrix object that caches
#  its inverse
makeCacheMatrix <- function(X = matrix()) {
  X_inv <- NULL
  set <- function(y) {
    X <<- y
    X_inv <<- NULL
  }
  get <- function() X
  setinv <- function(solve) X_inv <<- solve
  getinv <- function() X_inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special matrix returned 
#  by makeCacheMatric. If the inverse has already been calculated (and 
#  the matrix has not changed) it retrieve the inverse from cache.
cacheSolve <- function(X, ...) {
  ## Return a matrix that is the inverse of X
  X_inv <- X$getinv()
  if(!is.null(X_inv)) {
    message("getting cached data")
    return(X_inv)
  }
  data <- X$get()
  X_inv <- solve(data, ...)
  X$setinv(X_inv)
  X_inv
}
