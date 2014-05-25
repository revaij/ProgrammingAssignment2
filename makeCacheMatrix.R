makeCacheMatrix <- function(x = numeric()) {
  #This function creates a special "matrix" object that can cache its inverse.

  m <- NULL
  # Set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  
  #solve computes the inverse of a square matrix
  setinverse <- function(solve) m <<- solve
  # get the inverse
  getinverse <- function() m
  
  # A list with set,get,setinverse and getinverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
