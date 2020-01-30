## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##Object to cache matrix inverse is created.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Write a short comment describing this function
##Computing the inverse of the matrix returned by makeCacheMatrix and then returning the inverse matrix
##It checks if the inverse was computed, and if no, computes it
cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
