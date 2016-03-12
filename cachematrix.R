## Coursera R programming Assignment 2: a pair of functions that cache the inverse of a matrix. Input must be a square matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL ## m is the stored inverse
    set <- function(y) {
        x <<- y ## where x is the original input into makeCasheMatrix and y replaces x 
        m <<- NULL ##This allows that y's inverse can be stored in place of x's
    }
    get <- function() x ##print the matrix made by makeCacheMatrix
    setinverse <- function(solve) m <<- solve ##solve is variable, not a function here
    getinverse <- function() m ##will show the inverse matrix if stored
    list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse() ##Checking if inverse is already cached
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get() ##Calculating inverse of matrix. Only runs if not in cache.
    m <- solve(data)
    x$setinverse(m)
    m
}
