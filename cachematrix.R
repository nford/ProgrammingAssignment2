## With these two functions we enable a matrix to cache its own inverse for performance reasons

## Return a matrix that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
    
    inverse <- NULL
    
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setInverse <- function(newInverse) {
        inverse <<- newInverse    
    }
    
    getInverse <- function() {
        inverse
    }
    
    list(get=get, set=set, getInverse=getInverse, setInverse=setInverse)
    
}


## Return the cached inverse of the given matrix if available, otherwise invert and cache

cacheSolve <- function(x, ...) {
    
    inverse <- x$getInverse()
    
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    data <- x$get()
    
    inverse <- solve(data)
    
    x$setInverse(inverse)
    
    inverse

}
