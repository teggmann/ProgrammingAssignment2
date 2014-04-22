## Put comments here that give an overall description of what your
## functions do:
##  This function gets a matrix as an argument.
##  It stores it in x and its inverse in m

## Write a short comment describing this function
##  Defines 4 functions:
##      set: store the matrix gotten as arguement
##      get: return the matrix
##      setInverse: store the inverse of the matrix
##      getInverse: returns the inverse of the matrixto set and get the stored matrix and setInverse and getInvers
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(
        set = set, 
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## Write a short comment describing this function
##  this function checks if the inverse of the matrix received as agrument has
##  been calculated. If this is the case, it will return the stored inversed matrix
##  otherwise, it will calculate the inverse of the matrix, stores it and returns it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
