## These functions are based upon the example shown
## by R. Deng in the preparations for the assignment

## This first function calculates - given a matrix x -
## the inverse matrix of x

makeCacheMatrix <- function(x = matrix()) {
  ## ensure that the matrix m is empty
  m <- NULL ## will contain the result
  set <- function(y) {
        x <<- y
        m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve 
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## The second matrix checks is the inverse matrix of x
## had been calculated before. If so, the cached inverse matrix
## is returned. Otherwise, it is being calculated

CacheInverse <- function(x,...) {
  m <-x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)    
  }
  data <- x$get()
  m <- solve(data)
  x$setsolve(m)
  m
}




