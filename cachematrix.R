## The two functions below make it possible to pull the inverse of a matrix from 
## the cache if it has already been calculated. Thus multiple computations of 
## the inverse of the same matrix can be avoided.

## The function 'makeCacheMatrix' creates a special "matrix" 
## which returns a list containing 4 functions that
## 1. set the values of the matrix
## 2. get the values of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL;
     ## assign values of matrix
     set <- function(y) {
          x<<- y
          inv <<- NULL
     }
     ## print assigned matrix
     get <- function() x
     
     ## assign inverse of matrix
     setinv <- function(inverse) inv <<- inverse
     
     ## print assigned inverse of matrix
     getinv <- function() inv
     list (set = set, get=get,
           setinv=setinv,
           getinv=getinv)
}

## The function 'cacheSolve' gives back the inverse of 
## "matrix" created with the above function 'makeCacheMatrix',
## either by pulling it from the cache or computing it.

cacheSolve <- function(x, ...) {
     ## print assigned inverse (real inverse or NULL)
     inv <- x$getinv()
     
     ## in case inv!=NULL print value of inv
     if(!is.null(inv)) {
          message("Getting cached data")
          return(inv)
     }
     
     ## in case inv=NULL calculate and print inverse of x
     data <- x$get()
     inv <- solve(data, ...)
     x$setinv(inv)
     inv
}
