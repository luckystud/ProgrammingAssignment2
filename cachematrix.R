#' Matrix cache functions

#' My first attempt to use standard R documentation
#' @describeIn makeCacheMatrix Construct a special matrix that caches it's inverse
#' @param x A matrix
#' @return list of functions
# 
# Description: Construct a special matrix that caches the inverse
#     matrix.  Assumes that the input (x) is a square matrix.
#     Return a list of functions defined within the makeCacheMatrix
#     environment a la Lexical Scoping

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


#' @describeIn cacheSolve Return an inverse matrix constructed
#'  from makeCacheMatrix()
#' @param x The inverse matrix constructed from makeCacheMatrix
#' @return The inverse matrix
#
# Description: Return an inverse matrix.  If a inverse was
#     calculated already, return the cached matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
