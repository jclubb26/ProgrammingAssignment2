## These functions calculate the inverse of a matrix and saves to the cache.
## Therefore, using lexical scoping, next time the user seeks to calculate
## the inverse of the matrix, the previously calculated result in the cache
## will be used - this saves time rather than calculating it again.

## This function creates a special matrix that can cache its inverse. It is
## really a list containing a function to set the value of the matrix, get
## the value of the matric, set the value of the inverse and get the value of
## the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the matrix that is returned by 
## makeCacheMatrix (above). If this has already been calculated it will
## retrieve the inverse from the cache. If not it calcautes the inverse
## matrix and sets the value.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %*% data
  x$setinverse(m)
  m      
  
}
