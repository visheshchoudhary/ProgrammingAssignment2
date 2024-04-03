makeCacheMatrix <- function(x = matrix()) {
  ## Create a special matrix object that caches its inverse
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL  # Clear the cache when the matrix changes
  }
  
  get <- function() {
    x
  }
  
  cacheInverse <- function() {
    if (!is.null(inverse)) {
      message("Getting cached inverse")
      return(inverse)
    } else {
      message("Calculating inverse and caching")
      inverse <<- solve(x)
      return(inverse)
    }
  }
  
  list(set = set,
       get = get,
       cacheInverse = cacheInverse)
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  if (!is.list(x) || !all(c("set", "get", "cacheInverse") %in% names(x))) {
    stop("Input is not a cached matrix")
  }
  
  mat <- x$get()
  
  if (!is.null(mat) && !is.null(solve(mat, ...))) {
    if (!is.null(x$cacheInverse())) {
      message("Retrieving cached inverse")
      return(x$cacheInverse())
    } else {
      inv <- x$cacheInverse()
      return(inv)
    }
  } else {
    stop("Matrix is not invertible")
  }
}
