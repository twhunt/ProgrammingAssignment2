## Adds functionality to cache the result of a matrix inverse computation.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## This object holds a matrix, and its inverse.
## The matrix is set upon object instantiation, while its inverse is not.
## returns list of functions with names: setMatrix, getInverse, computeInverse, getMatrix
## setMatrix(y) sets the internal matrix to y, and its cached inverse to NULL.
## getInverse() returns the cached inverse, which may be null
## computeInverse() computes the inverse, and stores it in the cache so that subsequent calls to getInverse() will return the inverse, and not NULL.
## getMatrix() returns the matrix held by this object. This function is not required by the assignment, but its inclusion is arguably good practice.
makeCacheMatrix <- function(x = matrix()) {
  
  cachedInverse <- NULL
  
  setMatrix = function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  
  computeInverse = function() {
    cachedInverse <<- solve(x)
  }
  
  getMatrix = function() {
    x
  }
  
  getInverse = function() {
    cachedInverse
  }
  
  list(setMatrix = setMatrix, getInverse = getInverse, computeInverse = computeInverse, getMatrix = getMatrix)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## x is a special invertible matrix of mode makeCacheMatrix
        ## Return the inverse of 'x'
  
  if (is.null(x$getInverse())) {
    x$computeInverse()
  }
  
  x$getInverse()
}
