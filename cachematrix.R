## These functions are used to calculted the inverted matrix and save the 
## calculation in a cache that will keep its value as long as the matrix 
## is not modified

## This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(inv) m <<- inv
  getInv <- function() m
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Computes the inverse of the special matrix returned by makeCacheMatrix. 
## If the inverse is in the cache and the matrix hasn't been modified returns 
## the cached inverse.
## This function assumes that the matrix is invertible by using solve(x)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #else
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}
