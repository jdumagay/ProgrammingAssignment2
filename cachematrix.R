## makeCacheMatrix and cacheSolve alows for the creation of a matrix object
## that can cache its inverse matrix

## makeCacheMatrix takes a matrix whose inverse can be validly computed with
## R's solve function and returns a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    set_inverse <- function(inversed) inverse <<- inversed
    get_inverse <- function() inverse
    list(set = set, 
         get = get, 
         set_inverse = set_inverse, 
         get_inverse = get_inverse)
}


## cacheSolve takes a special matrix object created with the makeCacheMatrix
## function, computes its inverse, and places the computed inverse in cache

cacheSolve <- function(x, ...) {
    m <- x$get_inverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$set_inverse(m)
    m
}
