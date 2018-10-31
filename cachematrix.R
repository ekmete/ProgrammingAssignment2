## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly 
## Following functions cache the inverse of a matrix.
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix. 


## makeCacheMatrix creates a special "matrix", which is containing functions to
## set and get the value of the matrix and its inverse.
## Argument x is optional.

makeCacheMatrix <- function(x = matrix()) {
  
  inv<-NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverseM) inv <<- inverseM
  
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve calculates the inverse of the special "matrix" created with the above function. 
## Gets the inverse value of the matrix.
## Checks to see if the inverse has already been calculated.
## If so, gets the inverse from the cache and skips the computation. 
## Otherwise, calculates the inverse of the matrix
## Finally, sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    
    message("getting cached data")
    
    return(inv)
  }
  
  data <- x$get()
  
  inv <- solve(data, ...)
  
  x$setinverse(inv)
  
  inv
}