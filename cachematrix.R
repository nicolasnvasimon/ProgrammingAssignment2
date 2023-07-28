## Put comments here that give an overall description of what your
## functions do

# Assignement week 3: Matrix Inverse
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly
# (there are also alternatives to matrix inversion that we will not discuss here).
# 
# The assignment is to write a pair of functions that cache the inverse of a matrix.
# Write the following functions:
# 1/ makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# 
# 2/ cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache.

## Write a short comment describing this function
# The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        matrixInverse <- NULL
        
        set <- function(y) {
                x <<- y
                matrixInverse <<- NULL
        }
        get <- function() x
        
        setInversion <- function(inverse) matrixInverse <<- inverse
        getInversion <- function() matrixInverse
        
        list(set = set, get = get,
             setInversion = setInversion,
             getInversion = getInversion)
}


## Write a short comment describing this function
# The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        matrixInverse <- x$getInversion()
        
        if(!is.null(matrixInverse)) {
                message("getting cached data")
                return(matrixInverse)
        }
        
        data <- x$get()
        matrixInverse <- solve(data, ...)
        x$setInversion(matrixInverse)
        matrixInverse
}
