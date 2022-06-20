## As matrix inversion has a high computational cost, caching the inverse of a 
## matrix instead of computing it from scratch can be an efficient bypass. 
## The following pair of functions creates a special object 'inv' to act as a 
## cache to store the matrix inversion 

## This function is defined to create a matrix type object to cache the inverse 
## of the provided matrix. 'inv' will hold the value of the calculated 
## inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
              x <<- y
              inv <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) inv <<- solve(x)
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## this function computes the inverse of the matrix. If it finds that the 
## inverse is already in the cache and the matrix hasn't changes, then it will 
## retrieve the info from the cache instead of recomputing

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
