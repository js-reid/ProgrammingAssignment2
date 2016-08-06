## makeCacheMatrix & cacheSolve
## John Reid - August 6th, 2016
##
## These functions are written for Week 3, Programming Assignment 2
## of the Coursera class "R Programming"
##

## makeCacheMatrix creates a vector containing a function to 
## set/get the value of the matrix, set/get the value of 
## the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve calculates the inverse of the special vector
## created by makeCacheMatrix. It first checks to see if the 
## inverse has already been calculated--if so, it pulls it
## from the cache

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached value")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
