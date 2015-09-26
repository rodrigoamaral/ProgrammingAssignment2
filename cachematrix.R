## The following functions creates a cacheable matrix object
## and a function that calculate its inverse or getting
## a previous cached result if needed

## makeCacheMatrix creates a matrix object that can
## cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
  s <<- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set=set,
       get=get,
       setsolve=setsolve,
       getsolve=getsolve)
}


## cacheSolve returns the inverse of a matrix 'x' created with
## makeCacheMatrix, using its cache if available

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if (!is.null(s)) {
    message("getting cache data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
