## Put comments here that give an overall description of what your
## functions do

## Returns a structure holding:
##  - an input matrix (variable "x")
##  - a externaly computed inverse matrix (variable "inv")
##  - a set of functions to store and retrieve the input matrix
##    (set,get) and its inverse (setinverse, getinverse)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns the inverse of the matrix stored in the structure
## from makeCacheMatrix. Compute and caches the inverse if not
## available, otherwise just return the cached value.

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
