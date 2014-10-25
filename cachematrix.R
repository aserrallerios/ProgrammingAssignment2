## This method returns a list of methods used to cache the matrix inverse.
## A new matrix can be set and get. The matrix inverse can be set and get.
## Setting a new matrix clear the cached inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This method takes a cache with a matrix.
## If the inverse of the matrix is not cached, this method calculates it and 
## stores it in the cache. Then returns it.
## Otherwise, it just returns the cached inverse of the matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
