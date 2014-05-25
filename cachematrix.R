## This assignment consists of two functions that are used to 
## 1) create a special "matrix" object that can cache its inverse, and
## 2) compute the inverse of the special "matrix" (or retrieve the already cached inverse)

## The first function, makeCacheMatrix creates a special "matrix" object which can cache its
## inverse. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)        
}

## The second function, cacheSolve, gets the cached inverse of the matrix, or, if it hasn't
## already been cached, creates the inverse of the matrix and caches it. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}