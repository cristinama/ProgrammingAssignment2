## Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly 


## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        # set the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # get the value of the matrix
        get <- function() x
        # set the value of the inverse matrix 
        setinverse <- function(solve) inv <<- solve
        # get the value of the inverse matrix
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
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


## Sample run:
## > x = rbind(c(4, 3), c(5, 2))
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1]  [,2]
## [1,]    4    3
## [2,]    5    2

## No cache in the first run
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] -0.2857143  0.4285714
## [2,]  0.7142857 -0.5714286

## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.
##           [,1]      [,2]
## [1,] -0.2857143  0.4285714
## [2,]  0.7142857 -0.5714286

