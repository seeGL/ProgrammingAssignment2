# Coursera - R Programming course
# Programming Assignment 2
#
# Author: seeGL
# Date:  2015-01-25


# This code provides two functions that together provide a way to store and
# cache a matrix and its inverse so that the inverse can be retrieved without
# the need to reclaculate it each time.  The first function initializes the
# matrix and the second returns the inverse.

## makeCacheMatrix
# Take a matrix 'x' and return a list of functions to manipulate the matrix.
# $set - sets the matrix and clears the cached inverse
# $get - returns the matrix
# $setinv - sets the inverse of the matrix to the argument supplied
# $getinv - returns the cached matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(newinv) inv <<- newinv
    # return a list of the four functions that operate on the matrix
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve
# This function returns the previously calculated inverse of the matrix
# if it has already been calculated.  If the inverse has not already been
# calculated then it is first calculated then stored and returned.

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
