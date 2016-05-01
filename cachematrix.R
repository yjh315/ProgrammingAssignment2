## Cache the inverse of a matrix to skip computation 
## when we try to caculate the inverse again.

## function(1) : makeCacheMatrix : 
## creats a special matrix object (a list) containing 4 sub-functions.
## 1. set : set the value of the matrix
## 2. get : get the value of the matrix
## 3. setsolve : set the value of the inverse.
## 4. getsolve : get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## function(2) : cacheSolve : 
## This fuction get the special matrix object above (function1) as input.
## if the value of inverse of the matrix is found, it will just return the value,
## otherwise, this function will calculate the value of the inverse,
## save it to the cache and return it.

cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
    ## Return a matrix that is the inverse of 'x'
}
