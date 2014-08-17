## The pair of functions, makeCacheMatrix and cacheSolve, work to cache the 
## inverse of a given matrix, only recomputing the inverse if the matrix has 
## been changed.

## makeCacheMatrix creates a special "matrix" object that can cache 
## its inverse. The output is a list of 4 functions:
##      1. set the matrix
##      2. get the matrix
##      3. set the inverse of the matrix
##      4. get the inverse of the matrix

makeCacheMatrix <- function(A = matrix()) {
        inv <- NULL
        set <- function(B) {
                A <<- B
                inv <<- NULL
        }
        get <- function() A
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}