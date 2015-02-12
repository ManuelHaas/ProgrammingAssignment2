## The following two functions are used to create a special object 
## that stores a matrix and cache's its inverse

## makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(i) inverse <<- i
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve calculates the inverse of the special "matrix"
## created with the above function. 
## But first it checks to see if the inverse has already been 
## calculated. If so, it gets the inverse from the cache and 
## skips the computation. Otherwise, it calculates the inverse
## of the matrix and sets it in the cache via the setinverse
## function.

cacheSolve <- function(x, ...) {
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
