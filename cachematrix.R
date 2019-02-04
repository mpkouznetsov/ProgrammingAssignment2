## makeCacheMatrix creates "CacheMatrix", a list containing functions to
## set and get the value of a matrix
## set and get the values of its inverse
## 
## cacheSolve caclulates the inverse of a matrix stored within a "CacheMatrix
## returned by makeCacheMatrix. It caches the inverse, so if called
## multiple times with the same argument it will only call `solve` the first time.

## Creates "CacheMatrix", a holder object for a matrix and potentially its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse_matrix) inv <<- inverse_matrix
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns the inverse of the matrix stored in the argument which is a CacheMatrix
## of the kind returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
