## My functions are built analogous to the example. The first one creates a list of four functions. 
## It caches the inverse of a given matrix. 
##This could be used by the second functions to avoid time-consuming computations.

## This function creates a list of four functions. 
## (1) set the value of the matrix x.
## (2) get the value of the matrix x.
## (3) setInv sets the inverse of the given matrix x.
## (4) getInv gets the inverse of the given matrix x.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x  
  setInv <- function(solve) inv <<- solve  
  getInv <- function() inv  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function looks up whether the first function has already calculated the inverse of the matrix x.
## If so, it returns it. If not solves it by getting the data from the first function an solves the matrix by its own.

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(as.matrix(data), ...)
  x$setInv(inv)
  inv
}
