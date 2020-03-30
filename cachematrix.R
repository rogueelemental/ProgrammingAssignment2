## makeCacheMatrix and cacheSolve are a pair of functions that operate together.
## Their purpose is to calculate and cache an inverse of a matrix, with the
##    added benefit of first checking whether that calculation has already
##    been completed and stored in the cache, to save on computation time.

## makeCacheMatrix returns a matrix object that contains 4 functions,
##    stored in a 2x2 matrix, to be used by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setInverse <- function(inv) inverse <<- inv
      getInverse <- function() inverse
      ## temporarily creates list of functions
      temp <- list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
      ## returns the list of functions as a 2x2 matrix
      matrix(temp, 2, 2)
}


## cacheSolve first to see if the inverse matrix has already been calculated,
## and either returns the cached result, or calculates a fresh inverse.

cacheSolve <- function(x, ...) {
      ## Calls getInverse(), which is at (2,2) on the special matrix
      inverse <- x[[2,2]]
      ## Check for cached data
      if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      ## Calls get(), which is at (2, 1) on the special matrix
      workingmatrix <- x[[2,1]]
      ## Calculates inverse of workingmatrix
      inverse <- solve(workingmatrix)
      ## calls setInverse(), which is at (1, 2) on the special matrix
      x[[1, 2]]
      inverse
}
