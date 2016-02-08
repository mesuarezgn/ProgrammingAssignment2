## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## That is purpose of these fuctions. 

makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a special "matrix" object that can cache its inverse.
  
  minverse <- NULL
  set <- function(y) {
      x <<- y
      minverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) minverse <<- inverse
  getinverse <- function() minverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve will retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  minverse <- x$getinverse()
  if (!is.null(minverse)) {
    message("getting cached data")
    return(minverse)
  }
  data <- x$get()
  minverse <- solve(data, ...)
  x$setinverse(minverse)
  minverse
}
