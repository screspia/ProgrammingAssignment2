## Put comments here that give an overall description of what your
## functions do

## Creates a matrix that can be inverted
## In arguments: x (a matrix)
## Out arguments: a matrix that can get/set values and get/set the inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Setting the variables
  
  invert <- NULL
  set <- function(y) {
    
    ## Using the new operator introduced for this task
    x <<- y
    invert <<- NULL
  }
  
  ## Declaring the get, setInverse and getInverse functions.
  get <- function() x
  setInverse <- function(inverse) invert <<- inverse
  getInverse <- function() invert
  
  ## Returning a list of functions for the matrix
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Calculates the inverse of the matrix

## In arguments: x(a matrix)
## Out arguments: The inverse of x(a matrix)

cacheSolve <- function(x, ...) {
        
  invert <- x$getInverse()
  
  ## Returning the cached matrix message if it has already been calculated
  if(!is.null(invert)) {
    message("Already inverted")
    return(invert)
  }
  
  ## Calculates the inverted matrix
  data <- x$get()
  invert <- solve(data)
  
  ## Inverts
  x$setInverse(invert)
  
  ## Returns the inverted matrix
  invert
}
