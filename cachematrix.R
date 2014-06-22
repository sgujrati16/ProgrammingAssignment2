## @author: Sumeet Gujrati
## 06/20/2014
## Course: R Programming
## Assignment 2

## Matrix inversion is a costly computation and there may be some benefit to caching the inverse 
## of a matrix rather than compute it repeatedly. Following pair of functions cache the inverse of 
## a matrix.


## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(mat = matrix()) {
    inv <- NULL
    set <- function(y) {
        mat <<- y
        inv <<- NULL
    }
    get <- function() mat
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## retrieves the inverse from the cache.
cacheSolve <- function(mat, ...) {
    inv <- mat$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- mat$get()
    inv <- solve(data)
    mat$setinverse(inv)
    inv
}
