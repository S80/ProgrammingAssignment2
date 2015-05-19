## These two functions are designed to work together in order to: 
## 1.) Store a matrix, and store the inverse in cache memory.
## 2.) Retrieve the inverse of a matrix if it has been previously calculated using the "cacheSolve" function
## or otherwise calculate the inverse.

## This function creates a matrix object and may be used to cache the inverse of that matrix if calculated with "cacheSolve"

makeCacheMatrix <- function(x = matrix()) {
## Args:
##      x: an R object. Default class = matrix
#
## Returns:
##  A list of length four functions:
## "get" = retrieves the matrix stored in "makeCacheMatrix".
## "set" = changes the matrix stored in "makeCacheMatrix" and caches it.
## "getinverse" = retrieves the inverse of "x" if present, if not returns "NULL".
## "setinverse" = stores the inverse of an R object "x".
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function()x
        setinverse <- function(solve)m <<- solve
        getinverse <- function()m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function either:
## 1.) Retrieves the inverse of "x" from the "makeCacheMatrix" function or,
## 2.) Calculates the inverse of "x".
##     Regardless, the computed inverse of a given matrix will be cached.

cacheSolve <- function(x, ...) {
        ## Args:
        ##      x: an R object. Default class = matrix
        #
        ## Returns:
        ##      The inverse of a matrix "x"
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
