## With these two functions we enable a matrix to cache its own inverse for performance reasons
## Special thanks to rdpeng for his outstanding stub files.

## Function takes a matrix and returns a special "matrix" object that can get and set its inverse by cache

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


## Function takes a special "matrix" object returned from makeCacheMatrix, 
## returns the inverse of the matrix either by fetching from cache if available or calculating it anew
## if the inverse was newly calculated it is cached for future use.

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
