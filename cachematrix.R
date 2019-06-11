## The following functions cache the inverse of a matrix


## The following function creates a special "matrix"
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	j <- NULL
    set <- function (y) {
            x <<- y
            j <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) j <<- inverse
    getinverse <- function() j
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The following function computes the inverse of the
## special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
   j <- x$getinverse()
   if (!is.null(j)) {
           message("getting cached data")
           return(j)
   }
   data <- x$get()
   j <- solve(data, ...)
   x$setinverse(j)
   j
}
