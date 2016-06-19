## The following functions cache the inverse of a matrix

## This function creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        put <- function(y) {
                x <<- y
                inv <<- NULL
        }
        take <- function() x
        put_inv <- function(inverse) inv <<- inverse
        take_inv <- function() inv
        list(put = put, take = take, p_i = put_inv, t_i = take_inv)
}

## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the 
## matrix has not changed), then this function 
## should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        inv <- x$t_i()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$take()
        inv <- solve(data, ...)
        x$p_i(inv)
        inv
}