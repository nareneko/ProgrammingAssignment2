## Caches a matrix, calculates its inverse and caches it

## Takes a matrix and can get/set a matrix, get/set its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  getMatrix <- function() x
  getInverse <- function() inverse
  setMatrix <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  setInverse <- function(inv) inverse <<- inv
  
  list(getMatrix = getMatrix, getInverse = getInverse,
       setMatrix = setMatrix, setInverse = setInverse)
}


## Checks to see if inverse has been calculated, if not, calculates inverse of matrix, sets it and returns the inverse
cacheSolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x'
  if(is.null(x$getInverse())) {
    message('calculating inverse....')
    x$setInverse(solve(x$getMatrix()))
  }
  x$getInverse()
}
