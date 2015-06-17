## Functions to create a special object that will store a numeric matrix
## and cache its inverse

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## It takes in an invertible matrix, x, and returns a list of functions.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL # initialize the matrix inverse
    set <- function(y) { # set the value of the matrix in the cache
        x <<- y
        s <<- NULL
    }
    get <- function() x # get the value of the matrix

    setsolve <- function(solve) s <<- solve # set the value of the inverse
    getsolve <- function() s # get the value of the inverse
    list(set = set, get = get, # build a list of functions to be returned
         setsolve = setsolve,
         getsolve = getsolve)
}


## Computes the inverse of the special "matrix" returned from makeCacheMatrix.

cacheSolve <- function(x, ...) {
    s <- x$getsolve() # retrieve matrix inverse
    if(!is.null(s)) { # if inverse exists, uses the cached inverse
        message("retrieving matrix inverse from cache")
        return(s) ## return cached inverse
    }
    data <- x$get() ## get the matrix from cache
    s <- solve(data) ## calculate inverse
    x$setsolve(s) ## cache the value of the inverse
    s ## Return a matrix that is the inverse of 'x'
}
