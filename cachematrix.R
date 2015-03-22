## The following two functions are used to create a special object 
## that stores a matrix and cache's its inverse. 


## makeCacheMatrix creates the special object that
## stores the matrix and cache's its inverse(but does not calculate it)
## This special object is a list containing this functions:
##
## set: assing the matrix values
## get: return the matrix values
## setinv: cache's matrix inverse
## getinv: return matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL ## initializes the variable "inv" where the inverse will be stored

  set <- function(y) {
      x <<- y
      inv <<- NULL ## When the values of the matrix are changed,   
  }                ## the cache will be deleted
  
  get <- function() x
  
  setinv <- function(inverse) inv <<- inverse
  
  getinv <- function() inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## cacheSolve returns the inverse of a matrix created with makeCacheMatrix

cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  
  ## if the inverse is stored in the cache, cacheSolve will retrieve it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## if not cacheSolve calculates the inverse and stores it in the cache 
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  
  ## returns the inverse
  inv
}
