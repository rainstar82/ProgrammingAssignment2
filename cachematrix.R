## 24/04/2014 Anahita - R  - Programming Assignment 2

## --------------------------------------------------------------------------

##Matrix inversion is usually a costly computation and there may be
## some benefit to caching the data for the inverse of a matrix rather than 
## computing it repeatedly. Here we write a pair of functions that 
## cache the inverse of a matrix.

## ---------------------------------------------------------------------------

## The makecacheMatrix is simply a special Matrix which is a list containing a 
## function to:
## set the value of a matrix
## get the value of a matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## ---------------------------------------------------------------------------

## The following function inverse the special "matrix" created with the above 
## function. However, it first checks to see if the inverse  has already been
## created. If so, it gets the inverse from the cache & skips the computation.
## Otherwise, it goes ahead and and sets the inverse in the cache 
## via the setsolve function.

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting the cached data")
                return(s) 
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}

## --------------------------------------------------------------------------
