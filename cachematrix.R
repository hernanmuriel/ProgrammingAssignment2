## Put comments here that give an overall description of what your
## functions do
##
## Created by Hernán Muriel on 2020-09-28

## Write a short comment describing this function
## The function, makeCacheMatrix creates a special "matrix",
## which is really a list containing a function to
##   1. set the value of the matrix
##   2. get the value of the matrix
##   3. set the value of the solve
##   4. get the value of the solve
makeCacheMatrix <- function(x = matrix()) {
  ## begins by setting the solve to NULL as a placeholder for
  ## a future value
  s <- NULL
  
  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  ## get the value of the matriz
  get <- function() x
  
  ## set the value of the solve
  setsolve <- function(solve) s <<- solve
  
  ## get the value of the solve
  getsolve <- function() s
  
  ## returns the 'special matrix' containing all of the functions
  ## just defined
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function
## These part or code check if you have the solve of the matrix of
## interest. If these exist then you don't need calculate and you
## can use the cache variable
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
