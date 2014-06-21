## Based on example R script, I have two functions written here. 
## The purpose of the functions is to compute the inverseerse of a square matrix
## For example, if X is a square inverseertible matrix, then solve(X) returns its inverseerse


## This function creates a special "matrix" object that can cache its inverseerse.

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverseerse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverseerse has already been calculated (and the matrix has not changed)
## then the cachesolve should retrieve the inverseerse from the cache.

cacheSolve <- function(x, ...) {
  
  inverse <- x$getinverse()
  
  ## check if inverse exists in cache, then return it
  if(!is.null(inverse)) {
    message("Cached data used")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}