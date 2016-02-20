## These functions create and manipulate a special 'matrix' object
## that can store its computed inverse and utilise it when required

## This function creates a special "matrix" object (list of functions)
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) m <<- inv
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the input special "matrix" object
## or returns the cached result if already computed.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, diag(nrow = nrow(data), ncol = ncol(data)), ...)
    x$setinverse(m)
    m
}
