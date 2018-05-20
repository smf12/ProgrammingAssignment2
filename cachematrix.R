## Put comments here that give an overall description of what your
## functions do

## This function creates a list containing 4 functions:
## set_mat that sets the matrix data
## get_mat that gets the matrix data
## set_inv that sets the inverse matrix values
## get_inv that  retrieves the inverse matrix data

makeCacheMatrix <- function(x = matrix()) {
    ## returns a list containing 4 functions
    inv <- NULL
    set_mat <- function(n) {
        x <<- n
        inv <<- NULL
    }
    get_mat <- function() x
    set_inv <- function(inverse) inv <<- inverse
    get_inv <- function() inv
    list(set_mat = set_mat, get_mat = get_mat, set_inv = set_inv, get_inv = get_inv)
}


## This function retrieves the inverse of a square matrix. It first
## tries to retrieve the previously- calulated inverse matrix from 
## cache. If that does not exist, it calculates the inverse matrix
## via the solve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$get_inv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    d <- x$get_mat()
    inv <- solve(d, ...)
    x$set_inv(inv)
    inv
}
