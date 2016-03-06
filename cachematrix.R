## Put comments here that give an overall description of what your
## functions do

## cachematrix.R (originally from github.com/rdpeng/ProgrammingAssignment2)
## Author: FrankLa
## Last modified: Mar 06, 2016
## added details for makeCacheMatrix and cacheSolve 

## Write a short comment describing this function
## --> This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(invs) inv <<- invs
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function
## --> This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}