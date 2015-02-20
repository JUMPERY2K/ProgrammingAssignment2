# makeCacheMatrix is a function that returns a list of functions
# Its purpose is to store a matrix, and a cached value of the inverse of the
# matrix. Contains the following functions:
# * setMatrix          set the value of a matrix
# * getMatrix          get the value of a matrix
# * cacheInverse       get the cached value (inverse of the matrix)
# * getInverse         get the cached value (inverse of the matrix)

# the "x = matrix()" part works in the argument list of the function,
# but it seems to be creating a variable "x" that is not reachable from
# the global environment, but is avaiable in the environment of the
# makeCacheMatrix function
makeCacheMatrix <- function(x = matrix()) {
        
        # holds the cached value or NULL if nothing is cached
        # initially nothing is cached so set it to NULL
        cache <- NULL
        
        # store a matrix
        setMatrix <- function(newValue) {
                x <<- newValue
                # since the matrix is assigned a new value, flush the cache
                cache <<- NULL
        }
        
        # returns the stored matrix
        getMatrix <- function() {
                x
        }
        
        ## cache the given argument
        cacheInverse <- function(solve) {
                cache <<- solve
        }
        
        ## get the cached value
        getInverse <- function() {
                cache
        }
        
        # return a list. Each named element of the list is a function
        list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse, getInverse = getInverse)
}


## The following function calculates the inverse of a "special" matrix created with
## makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## get the cached value
        inverse <- x$getIverse()
        ## if a cached value exists return it
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        ## otherwise get the matrix, calculate the inverse and store it in 
        ## the cache
        data <- x$getMatrix()
        inverse <- solve(data)
        x$cacheInverse(inverse)
        
        # return the inverse
        inverse
}

