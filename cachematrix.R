## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverseCache <- NULL
        set <- function(y) {
                x <<- y
                inverseCache <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) inverseCache <<- solve
        getInverse <- function() inverseCache
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseCache <- x$getInverse()
        if(!is.null(inverseCache)) {
                message("getting cached data...")
                return(inverseCache)
        }
        data <- x$get()
        inverseCache <- solve(data, ...)
        x$setInverse(inverseCache)
        inverseCache
}

data <- matrix( c(5, 1, 0,
                  3,-1, 2,
                  4, 0,-1), nrow=3, byrow=TRUE)

data

datainv <- makeCacheMatrix(data)

cacheSolve(datainv) #inverse returned after computation

cacheSolve(datainv) #inverse returned from cache