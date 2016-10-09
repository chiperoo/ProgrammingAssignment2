# There are two functions in this file: makeCacheMatrix() and cacheSolve().
# makeCacheMatrix() creates an R object that stores a matrix and its inverse.
#
# cacheSolve() requires an argument that is returned by makeCacheMatrix() in
# order to retrieve the matrix inverse from the cached value stored in
# the makeCacheMatrix() object's environment 


# This function creates a list for a few defined sub functions. This list
# has the following elements:
#  * set
#  * get
#  * setMatrixInverse
#  * getMatrixInverse
# There are also 2 data objects: x and m.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMatrixInverse <- function(matrix) m <<- matrix
  getMatrixInverse <- function() m
  list(set = set, get = get,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}

# This function calls functions from the R object in the parent environment.
# The input is of type makeCacheMatrix()
cacheSolve <- function(x, ...) {
  m <- x$getMatrixInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setMatrixInverse(m)
  m
}
