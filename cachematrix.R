## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Two functions have been written to help in caching the inverse of a matrix.

## makeCacheMatrix creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve computes the inverse of the special "matrix" returned by 
## the above function makeCacheMatrix. If the inverse has already been calculated, 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data.")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinv(i)
    i
}

##Sample Output
##> x <- matrix(1:4,2,2)
## > x
##       [,1] [,2]
## [1,]    1    3
## [2,]    2    4

## > x_inv <- makeCacheMatrix(x)
## > x_inv$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4

## First run - no cache
## > cacheSolve(x_inv)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## Second run - Retrieving cache
## > cacheSolve(x_inv)
## getting cached data.
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## >


