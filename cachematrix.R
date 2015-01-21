## R Programming Course by Coursera
## Programming Assingment 2 - Caching the Inverse of a Matrix

## There are two functions Put comments here that give an overall description of what your

## 1. makeCacheMatrix - This function takes an invertible matrix as input and prepares the cacheable matrix object
## This function creates a special "matrix', which is really a list containing functions to
## set the matrix
## get the matrix
## set the inverse of the matrix
## get the inverse of the matrix

## 2. cacheSolve - This function calculates the inverse of the matrix, if it is not already available in the cache.
## If the inverse is already in the cache, the inverse is returned from the cache.
## It uses the solve(x) function to calculate the inverse of the Matrix


## Function which create the 'matrix' 
makeCacheMatrix <- function(x = matrix()) {
  ## invMatrix is to store the inverse of the input matrix
  invMatrix <- NULL
  
  ## Set Method for the 'matrix' which takes an invertible matrix
  set <- function(y) {
    ## initalize the values
    x <<- y
    invMatrix <<- NULL
  }
  
  ## Get method for the 'matrix' 
  get <- function() x
  
  ## Set method to set the inverse matrix
  setInverse <- function(invm) invMatrix <<- invm
  
  ## Get method to return the available inverse value 
  getInverse <- function() invMatrix
  
  ## return a list with the set, get methods for the special 'vector' and the mean of the 'vector'
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Function which caches the inverse of the matrix
cacheSolve <- function(x, ...) {
  ## first get the inverse
  invM <- x$getInverse()
  
  ## If not null, return the cached value
  if(!is.null(invM)) {
    message("Getting inverse of the matrix from cached data")
    return(invM)
  }
  
  ## Since at this point, inverse is null, calculate the new inverse
  newMatrix <- x$get()
  invM <- solve(newMatrix, ...)
  
  ## Store the inverse for future use
  x$setInverse(invM)
  
  ## return the calculated inverse matrix
  return(invM)
}

## End