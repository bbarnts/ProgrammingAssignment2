# The functions in this file provide a means of caching the inversion 
# of a matrix to reduce CPU load. This way the inversion of a matrix
# will only be calculated once, then retrieved from memory on subsequent
# calls.


# Creates an object that can store and retrieve both the original matrix
# and its inversion. This funciton is the argument to cacheSolve.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <-function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


# This function returns the inverse of the special matrix object returned
# by makeCacheMatrix. If x already has a stored value for inverse, that value 
# is returned. If not (this is the first time cacheSolve has been called on x), 
# then the inverse of x is calculated and stored in the x object.
cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of x
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("Getting cached matrix.")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}

