## Coursera R programming Assignment #2
## A pair of functions that cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
# this function creates a special matrix object that can cache its inverse
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) s <<- solve
    getinverse <- function() s
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
# This function retrieves the matrix inverse, otherwise it computes the matrix
# inverse from the makeCacheMatrix function 
    s <- x$getinverse()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    
    data <- x$get()
    s <- solve(data, ...)
    x$setinverse(s)
    s
}
