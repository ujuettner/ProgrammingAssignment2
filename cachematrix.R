# Enables caching the calculated inverse of a given matrix.
# Example usage:
# > m <- matrix(c(1, 0, 0, 0, 2, 0, 0, 0, 1), nrow = 3, ncol = 3)
# > mc <- makeCacheMatrix(m)
# > cacheSolve(mc)
#      [,1] [,2] [,3]
# [1,]    1  0.0    0
# [2,]    0  0.5    0
# [3,]    0  0.0    1
# > cacheSolve(mc)
# getting cached data
#      [,1] [,2] [,3]
# [1,]    1  0.0    0
# [2,]    0  0.5    0
# [3,]    0  0.0    1

# makeCacheMatrix:
# Creates an object thst stores a given matrix and its inverse (once it is set)
# in memory.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(m) {
    x <<- m
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# cacheSolve:
# Returns a cached inverse of a given matrix created by makeCacheMatrix().
# In case of a cache miss, the inverse is calculated and put into the cache.
cacheSolve <- function(x, ...) {
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