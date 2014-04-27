## Functions that cache the inverse of a matrix 

## Cache the solve method result which returns the inverse of a matrix
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


## Return the inverse of a matrix either from cache or by calling the solve method if the result is not cached
## Save inverse value in cache after calling the solve method
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
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

##Example of the use of the Function 
##> mdat <- matrix(c(1,2,3,4), nrow = 2, ncol = 2)
##> mat <- makeCacheMatrix(mdat)
##> cacheSolve(mat)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cacheSolve(mat)
##getting cached data
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5