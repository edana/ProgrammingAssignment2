## This code will create a special matrix that can save in cache 
## its inverse to be able to use it several times

## Creates new functions to get and set the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setSolve <- function(sol) i <<- sol
    getSolve <- function() i
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


##  Performs the calculation of the inverse of the matrix 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getSolve()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    mat <- x$get()
    i <- solve(mat, ...)
    x$setSolve(i)
    i
}
