## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # This will hold the cached inverse
  
  # 1. Function to set the matrix value
  set <- function(y) {
    x <<- y      # Use <<- to assign to the 'x' in the parent environment
    inv <<- NULL # Reset the cache if the matrix changes
  }
  
  # 2. Function to get the matrix value
  get <- function(){
    x
  }
  
  # 3. Function to set the inverse (called by cacheSolve)
  setInverse <- function(inverse){
    inv <<- inverse
  } 
  
  # 4. Function to get the cached inverse
  getInverse <- function() {
    inv
  }
  # Return the list of functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  # Attempt to get the inverse from the cache
  inv <- x$getInverse()
  
  # If the cache isn't empty, return the cached data
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise, get the matrix, calculate the inverse, and cache it
  data <- x$get()
  inv <- solve(data, ...) # Using the solve() function in R
  x$setInverse(inv)
  
  inv # Return the inverse
}
