## These functions solve for the inverse of a matrix and 
##cache the solution so that it does not need to be recalculated 

## This function takes a matrix and sets up a cache that can be used
## to store the value. Returns a list that allows cacheSolve
## to either cacluate the inverse or return the cached value

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }



## This function takes the list output from the makeCacheMatrix function
## and either solves for the inverse of the matrix or 
##returns the cached value

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
  }

