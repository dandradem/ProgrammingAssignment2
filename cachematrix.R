## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The makeCacheMatrix function creates a special matrix, which is a list
## containing containing functions to:
### Set the value of the matrix
### Get the value of the matrix
### Set the value of the inverse
### Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
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


## Write a short comment describing this function

## The cacheSolve function calculates the inverse of the special matrix created
## by makeCacheMatrix. It first check if the inverse has been calculated for
## getting it from the cache. If it hasn't been calculated, it calculates the
## inverse of the data and sets its value in the cache via the setsolve function.
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m        ## Return a matrix that is the inverse of 'x'
}
