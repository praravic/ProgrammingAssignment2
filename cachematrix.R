##The below functions compute and cache the inverse of an invertible matrix

## makeCacheMatrix function creates creates a special "matrix" object 
## that can cache its inverse and has a list of 4 accessor functions

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(myinv) i<<- myinv
        getinv <- function() i
        list(set = set, get = get, 
             setinv = setinv, getinv = getinv)
}


## cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix.
## If inverse is already computed and the input matrix has not changed, 
## then this function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        solve_i <- x$getinv()
        if (!is.null(solve_i)) {
                message("Obtaining cached data")
                return(solve_i)
        }
        data <- x$get()
        solve_i <- solve(data, ...)
        x$setinv(solve_i)
        solve_i
}
