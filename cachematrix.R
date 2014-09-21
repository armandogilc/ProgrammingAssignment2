## Put comments here that give an overall description of what your
## functions do
# return funtions list

makeCacheMatrix <- function(x = matrix()) {
    # for cache inverse matrix
    inv <- NULL

    # Set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # Get the matrix
    get <- function() x

    # Set the inverse
    setinv <- function(inverse) inv <<- inverse
    # Get the inverse
    getinv <- function() inv

    # Return the matrix 
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# Compute the inverse of the matrix. If the inverse is already
cacheSolve <- function(x, ...) {
    inv <- x$getinv()

    # return calculated inverse
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    data <- x$get()
    inv <- solve(data, ...)

    # Cache the inverse
    x$setinv(inv)

    inv
}
