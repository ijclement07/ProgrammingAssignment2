## Script cachematrix.R
##
## This script defines a matrix class that stores the inverse of
## a given matrix in the cache, once it is computed. The script
## also defines a function to retrieve the matrix inverse, or compute
## it if it is not yet stored in the cache.

## Function makeCacheMatrix(x)
## Takes a given matrix x and stores its inverse in the cache.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) m <<- inv
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Function cacheSolve(x)
## Takes a given matrix object x (as defined by makeCacheMatrix),
## and returns its inverse stored in the cache. If the cache is
## empty, then the inverse is computed and is then stored in the
## cache.
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  mat <- x$get()
  inv <- solve(mat)
  x$setInverse(inv)
  inv
}

