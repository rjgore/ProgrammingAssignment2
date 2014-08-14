## These functions are to store the inverse of a square full rank
## matrix so that it will not rquire repeated computation since inverting
## a matrix is a time consuming process.

## this function creates a list that contains four funtions that do:
## 1) set the value of the full rank square matrix
## 2) get the value of the matix
## 3) set the value of the inverse of the matrix
## 4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
       inv <- NULL
       set <- function(y) {
            x <<- y
            inv <<- NULL
       }
       get <- function() x
       setinv <- function(source) inv <<- source
       getinv <- function() inv
       list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function calculates the inverse if it has not already been
## calculated.  It checks to see if the inverse is already cached and
## gets the inverse from the cache and skips the inversion calculation. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       inv <- x$getinv()
       if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
       }
       data <- x$get()
       inv <- solve(data, ...)
       x$setinv(inv)
       inv
}

