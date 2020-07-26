## makeCacheMatrix is used to calculate the inverse
## of matrices in general

## This function consists of set, get, set_inverse
## and get_inverse

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setinv <- function(inverse) {
                inv <<- inverse}
        getinv <- function() {
                inver <- ginv(x)
                inver %*% x
                }
        list(set = set, get = get, setinv = setinv,
             getinv = getinv)
}

## cacheSolve is used to get cache data

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv 
        ## Return a matrix that is the inverse of 'x'
}
