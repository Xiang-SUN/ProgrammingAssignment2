## A pair of functions that cache the inverse of a matrix:

## this function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    # Initial
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # Get initial matrix
    get <- function() x
    
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    
    #Return Value
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## this function can cache the result of its inverse

cacheSolve <- function(x, ...) {
   
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    
    ## Return a matrix that is the inverse of 'x'
    inv
}
