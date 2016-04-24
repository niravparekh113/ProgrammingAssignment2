# 1. makeCacheMatrix: This function creates a special "matrix" object 
#    that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse_vector <- NULL
  set <- function(y) {
    x <<- y
    inverse_vector <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inverse_vector <<- inverse
  getInverse <- function() inverse_vector
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# 2. cacheSolve: This function computes the inverse of the special "matrix" returned 
#    by makeCacheMatrix above. If the inverse has already been calculated 
#    (and the matrix has not changed), then the cachesolve should retrieve the inverse 
#    from the cache.

cacheSolve <- function(x, ...) {
  inverse_vector <- x$getInverse()
  if (!is.null(inverse_vector)) {
    message("getting cached data")
    return(inverse_vector)
  }
  mat <- x$get()
  inverse_vector <- solve(mat, ...)
  x$setInverse(inverse_vector)
  inverse_vector
}

