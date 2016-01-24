## This file create a function to cache the inverse of matrices. It was created for Coursera's
## R Programming class and is based on the example code for the assignment 
##(https://www.coursera.org/learn/r-programming/peer/tNy8H/programming-assignment-2-lexical-scoping)

## This function creates a special matrix object capable of holding a cache of its inverse once the inverse is calculated

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}



## This function calculates the inverse of a CacheMatrix. If the inverse has already been calculated, it returns
## the cached value. If the inverse has not yet been calculated, it calculates the inverse and stores the value.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
  }
