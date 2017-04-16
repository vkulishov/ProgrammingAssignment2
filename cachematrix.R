## This is solution to Programming Assignment 2 of `R Programming` Coursera class.
## Included functions provide functionality to inverse matrix and cache the result.

## Usage example:
## m <- matrix(1:4, nrow = 2, ncol = 2)
## mc <- makeCacheMatrix(m)
## all.equal(solve(m), cacheSolve(mc)) // returns TRUE
## all.equal(solve(m), cacheSolve(mc)) // prints 'getting cached data' and returns TRUE

## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        
        ## Gets original matrix.
        get <- function() x
        
        ## Sets matrix value and resets inverse cached value to NULL.
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        ## Gets cached value of inverse matrix.
        getInverse <- function() inverse
        
        ## Sets cached inverse matrix value.
        setInverse <- function(i) {
                inverse <<- i
        }
        
        list(get = get, getInverse = getInverse, 
             set = set, setInverse = setInverse)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache;
## otherwise inverse matrix is computed and put into cache
cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if (!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        matrix <- x$get()
        inverse <- solve(matrix, ...)
        x$setInverse(inverse)
        inverse
}
