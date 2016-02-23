## These functions create and manipulate a special 'matrix' object
## that can store its computed inverse and utilise it when required

## This function creates a special "matrix" object (list of functions)
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # 'set' function overwrites the currently stored matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x # returns the currently stored matrix
    setinverse <- function(inv) m <<- inv # stores the inverse of current matrix
    getinverse <- function() m # returns the inverse of current matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the input special "matrix" object
## or returns the cached result if already computed.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) { # if the inverse was already calculated...
        message("getting cached data")
        return(m) # ... return cached inverse
    }
    # ... otherwise calculate the inverse
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m) # store the inverse for future use!
    m # return inverse
}
