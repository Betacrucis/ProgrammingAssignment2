## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This pair of functions creates a cache for an inverse of a matrix 
## and then solves the inverse of a matrix

## usage: cacheSolve(makeCacheMatrix(x))
## where x is an invertable matrix
## makeCacheMatrix(x) can also be saved to a variable and called directly by cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Write a short comment describing this function
## companion function with makeCacheMatrix where the inverse of amatrix is computed

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
