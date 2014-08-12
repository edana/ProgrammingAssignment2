## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getSolve()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    mat <- x$get()
    i <- solve(mat)
    x$setSolve(i)
    i
}
