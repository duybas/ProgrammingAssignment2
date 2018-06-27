## Put comments here that give an overall description of what your
## functions do

## In this script `makeCacheMatrix` creates a list of functions that are
## 1 - set a new matrix
## 2 - get the matrix that is set
## 3 - set the inverse of the matrix
## 4 - get the inverse of the matrix that is set

makeCacheMatrix <- function(x = matrix()) {
    matinv <- NULL
    set <- function(y) {
        x <<- y
        matinv <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) matinv <<- inv
    getinverse <- function() matinv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Assuming that the matrix supplied is always invertible 
## `cacheSolve` function calculates the inverse of the matrix.
## The first action is to check whether the inverse is already
## cached by calling `getinverse`. If not, it assigns the matrix
## to `data` by calling `get`, calculates its inverse, and
## assigns this value to the cache by calling `setinverse`

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
