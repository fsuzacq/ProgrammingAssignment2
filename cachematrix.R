## Two functions that combined allow the user to work
## with Invertable matrices, and cache the result of inverting a Matrix
## to avoid computing it again.

## Creates a cached matrix, that holds the result of the inverted matrix
## Returns a list containing 4 functions: one to set matrix data, one to retrieve it,
## other to set the inverse of the setted matrix, and other to retrieve the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
    ## Holds the value of the inverted matrix
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## Returns the inverse of a matrix, fetching it from cache if it is already solved
## or calculating it and storing it if it was not calculated before.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## Is not on cache
    data <- x$get()
    i <- solve(data, ...)
    
    ## Store result on cache for future calculations
    x$setInverse(i)
    
    i
}
