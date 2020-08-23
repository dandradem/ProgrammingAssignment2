## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { #set the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x #get the value of the matrix
  setsolve <- function(solve) m <<- solve #set the value of the inverse
  getsolve <- function() m #get the value of the inverse
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) { #check if the inverse has already been calculated
    message("getting cached data")
    return(m)
  }
  data <- x$get() #if it hasn't been calculated yet, it calculates the inverse from data
  m <- solve(data, ...)
  x$setsolve(m)
  m        ## Return a matrix that is the inverse of 'x'
}
