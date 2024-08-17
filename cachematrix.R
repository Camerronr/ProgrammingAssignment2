## This function creates a special "matrix" object
## that can cache its inverse

## Based on the example, I replaced m with cm and mean with solve

makeCacheMatrix <- function(x = matrix()) {
      cm <- NULL
      set <- function(y) {
            x <<- y
            cm <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) cm <<- solve
      getsolve <- function() cm
      list(set = set, get = get,
            setsolve = setsolve,
            getsolve = getsolve)
}


##  This function computes the inverse of the special "matrix" returned 
##  by makeCacheMatrix above, from the cache if already calculated

cacheSolve <- function(x, ...) {
      cm <- x$getsolve()
      if(!is.null(cm)) {
            message("getting cached data")
            return(cm)
      }
      data <- x$get()
      cm <- solve(data, ...)
      x$setsolve(cm)
      cm
}
