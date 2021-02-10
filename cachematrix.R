## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inverse property
  inv <- NULL
  
  ## Method to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Method to set the inverse of the matrix
  get <- function() {x}
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  ## Return a list of the methods
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## Get the matrix from our object
  mat <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  inv <- solve(mat, ...)
  
  ## Set the inverse to the object
  x$setInverse(inv)
  
  ## Return the matrix
  inv
}

source(makeCacheMatrix.R)

## Getting normal matrix
pmatrix <- makeCacheMatrix(matrix(1:16, nrow = 4, ncol = 4))
pmatrix$get()
##Getting the inverse
pmatrix$getInverse()

## Getting normal matrix
pmatrix <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
pmatrix$get()

##Getting the inverse
pmatrix$getInverse()
cacheSolve(pmatrix)
pmatrix$getInverse()
