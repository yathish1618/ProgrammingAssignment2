## Makes caching to avoid repeated computation of inverse of the same matrix

## creates functions that sets or gets matrix and further sets or gets the inverses of the matrix

makeCacheMatrix <- function(x = matrix()) {
  #initialise inverse with NULL
  inverse <- NULL
  
  #set the matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  #fetch the matrix
  get <- function() x
  
  #store inverse in cache of this function
  setinverse <- function(inv) inverse <<- inv
  
  #fetch the stored inverse
  getinverse <- function() inverse
  
  #return all the functions defined above
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## computes inverse and stores it in cache. Retrieves inverse from cache if it's already available in the cache

cacheSolve <- function(x, ...) {
  
  #fetch the inverse
  inverse <- x$getinverse()
  
  #check if inverse is NULL or if it's stored already in the cache. If it's stored simply return that cached inverse
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  #get the matrix
  data <- x$get()
  
  #calculate the inverse
  inverse <- solve(data, ...)
  
  #set and store the inverse in cache
  x$setinverse(inverse)
  
  #return inverse
  inverse
}
