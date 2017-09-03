## Put comments here that give an overall description of what your
## functions do

## Below function creates object named "matrix" which is used to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL ## initialise invs
  
  ## define set function 
  set <- function(y){
    x <<- y
    invs <<- NULL ## reset invs to null 
  }
  
  ## define get function which returns matrix param
  get <- function() x
  
  setInverse <- function(solveMatrix) invs <<- solveMatrix
  
  getInverse <- function() invs
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## Below function gives the mattrix inverse of the "matrix" object returned by the makeCacheMatrix fuction.
cacheSolve <- function(x, ...) {
  ## Compute matrix inverse of x
  inv <- x$getInverse()
  if(!is.null(inv)){
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}
