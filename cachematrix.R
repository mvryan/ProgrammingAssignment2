## Author: Michael Ryan
## January 6th, 2019
## Programing Assignment 2
## Lexical Scoping

## Two Functions that allow you to store a matrix that will cache results of the solve function.
## This is intended to help speed up processing time.

## Create a new Cache Matrix. This matrix stores the inverse to save computation time.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Return the inverse of the CacheMatrix x. Either by applying solve or the cached value.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
