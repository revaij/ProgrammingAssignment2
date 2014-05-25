## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #This function computes the inverse of the special "matrix" returned by 
  #makeCacheMatrix. If the inverse has already been calculated 
  #(and the matrix has not changed), 
  #this function should retrieve the inverse from the cache.
  
  m <- x$getinverse()
  
  #Check to see if the inverse has already been calculated
  if(!is.null(m)) {
    # get the inverse
    message("getting cached data")
    #skip the function
    return(m)
  }
  
  # it calculates the inverse of the data and 
  #sets the value of the inverse in the cache via the setinverse function.
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
        
        
}
