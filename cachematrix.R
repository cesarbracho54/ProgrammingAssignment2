## There are two functions writen below. The first function creates a special matrix.
## The second function calculates and caches the inverse of the special matrix.

## This function creates a special matrix. It returns a list of functions
## that will be used to cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverted <- function(solve) m <<- solve
  getinverted <- function() m
  
  list(set = set, get = get, 
       setinverted = setinverted, 
       getinverted = getinverted)  
}   

#######################################################################

## The cacheSolve function calculates the inverse of a matrix and caches it.
## if the inverse has already been calculated, then it returns the result from
## the cache

cacheSolve <- function(x, ...) {
  m <- x$getinverted ()
  if(!is.null(m)) {
    message("getting cache data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverted(m)
  m

}
