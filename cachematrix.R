## These two complimentary functions impliment a custom Matrix object which calculates
## and stores a cached inverse of the data matrix. Should the cacheSolve function be
## called twice without the stored matrix having been changed between calls then the
## cached inverse is returned, else the inverse is recaculated and returned.

## makeCacheMatrix (x= matrix())
## Creates/returns a CacheMatrix object to be used by cacheSolve(x,...)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  set <- function(y) {
    x <<- y       ##Assign new matrix data to x
    inv <<- NULL  ##Set the cached inverse to null as the cached result in now invalid
  }
  getInverse <- function() inv
  setInverse <- function(invMatrix) inv <<- invMatrix
  #Return CacheMatrix object
  list(get=get, set=set, getInverse = getInverse, setInverse=setInverse)
}


## cacheSolve (x, ...)
## Returns the inverse of x (a CacheMatrix object); if the matrix is unchanged since 
## the previous function call a cached result is returned, alternatively if the matrix
## has been altered the inverse is recalculated and then returned.
## NOTE: The supplied matrix is assumed to be invertable as prescribed.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("Returning cached inverse")
    return(inv)
  } else {
    theMatrix <- x$get()
    inv <- solve(theMatrix, ...)
    x$setInverse(inv)
    ## Return a matrix that is the inverse of 'x'
    return(inv)
  }
  message("Error") ##This line should never execute
}
