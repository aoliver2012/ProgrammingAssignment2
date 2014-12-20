################################################# 
## cachematrix.R
## Extends the default matrix function to include
## a cached inverse of matrix. Inverse must be
## calculated explicitly the first time the 
## cacheMatrix is created / set.
#################################################

## makeCacheMatrix will create the cached matrix object when
## passed a matrix, or an empty matrix otherwise. Inverse
## must be created explicitly via cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x<<-y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve will check the inverse of the matrix contained in the
## the passed cached matrix object and, if NULL, will calculate and
## set it's inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
