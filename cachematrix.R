## Below two functions functions cache the inverse of a matrix:
## 1. makeCacheMatrix creates a special "matrix" object hat can cache its inverse
## 2. cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix; it optimizes the 
## operation by retrieving the inverse from the cache, if the inverse has already been calculated and the 
## matrix has not changed

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse 
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function() inv <<- solve(x) #calculate the inverse
    getInverse <- function() inv
    list(set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache 
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
