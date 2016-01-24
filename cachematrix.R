## This pair of functions attempt to eliminate unnecessary costly solve functions
## by caching the inverse the first time it is calculated and retrieving cached
## inverse should the inverse be required again for the same matrix

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function solves the inverse of the matrix returned from makeCacheMatrix 
## If the inverse has already been calculated and the matrix has not changed then
## cachesolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  ## if the inverse is cached - retrieves it and skips the re-calc
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## as cached inverse does not exist, calculates it and caches
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
