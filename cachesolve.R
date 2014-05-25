cachesolve <- function(x, ...) {
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
