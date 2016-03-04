## Functions for creating and accessing a cache of 
## a matrix and its inverse

## Return a list of functions for setting and reading a
## cache of matrix 'x' and its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  set_inverse <- function(solve) inverse <<- solve
  get_inverse <- function() inverse
  return(list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse))
}


## Return a matrix that is the inverse of 'x'.
## If the inverse is cached, and the matrix hasn't changed,
## return the cached result. Otherwise, compute the inverse
## and cache the result
cacheSolve <- function(x, ...) {
  inverse <- x$get_inverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$set_inverse(inverse)
  return(inverse)
}
