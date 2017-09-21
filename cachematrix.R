## Create a cache matrix object that can be used
## repeatadly while only needing to calculate the
## inverse of the matrix once.


## makeCacheMatrix <- create a cache object for
## the invertable matrix.

makeCacheMatrix <- function(x = matrix()) {
        inverseCache <- NULL
        set <- function(y) {
                x <<- y
                inverseCache <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inverseCache <<- inverse
        getinverse <- function() inverseCache
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve <- returns the inverse matrix of "x". Used with
## makeCacheMatrix, this will check the cache to see if the inverse
## is already computed. If so, it will pull the inverse from cache.
## Otherwise it will calculate the inverse of x.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}


## Tested formula for the following:
##
##
## m <- matrix(c(1,2,3,4), nrow = 2, ncol = 2)
## m
## 1    3
## 2    4
##
##
## cache <- makeCacheMatrix(m)
## cacheSolve(cache)
##
##  -2   1.5
##   1  -.05