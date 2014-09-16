## A pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix<- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  
  # list of elements that this function returns
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## The following function calculates the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) { # if allready in memory
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) # first calculation of inverse
  x$setinv(m)
  m
}
