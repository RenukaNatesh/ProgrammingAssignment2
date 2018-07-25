## Created by Renuka Natesh
## This file contains two functions which will compute inverse of matrix 
## and cache it.

## This function will create an object of a matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function( y ) {
      x <<- y
      inv <<- NULL
    }
  
    get <- function() x
    setinv <- function( i ) inv <<- i
    getinv <- function() inv
  
    list(set = set, get = get,setinv = setinv, getinv = getinv)
}


## this function returns the inverse of a matrix. If it was cached it 
## will return from cache, else it will compute and cache it.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)){
      message("getting cached inverse")
      return(inv)
    }
  
    inv <- solve(x$get())
    x$setinv(inv)
    inv        
}
