## Put comments here that give an overall description of what your
## functions do
## Create two functions to cache the inverse of a matrix

## Write a short comment describing this function
## makeCacheMatrix creates a matrix that can cache the inverse of itself for the input

makeCacheMatrix <- function(x = matrix()) {
        inverseValue <- NULL
        set <- function(y) {
                x <<- y
                inverseValue <<- NULL
        }
        get <- function() x
        setInverseValue <- function(inverse) inverseValue <<- inverse
        getInverseValue <- function() inverseValue
        list(set = set, get = get, setInverseValue = setInverseValue, getInverseValue = getInverseValue)
}


## Write a short comment describing this function
## cacheSolve returns the inverse of the matrix returned by makeCacheMatrix,
## if the inverse was already calculated then cacheSolve will get the inverse
## from cache

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
        inverseValue <- x$getInverseValue()
        if(!is.null(inverseValue)) {
                message("getting cached result")
                return(inverseValue)
        }
        data <- x$get()
        inverseValue <- solve(data, ...)
        x$setInverseValue(inverseValue) 
        inverseValue
}
