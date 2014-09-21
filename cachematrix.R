## Functions for creating and using inverted matrices which caching ability

## Create cacheable matrix for input
makeCacheMatrix <- function(original.matrix = matrix()) {
  
  if (!is.matrix(original.matrix)) {
    stop("Please set matrix.")
  }
  
  inverted.matrix <- NULL
  
  set <- function(y) {
    original.matrix <<- y
    inverted.matrix <<- NULL
  }
  
  # Functions for getting and setting cached inv. matrix value
  get <- function() original.matrix

  # use solve on inverted matrix
  set.inverse <- function(solve) inverted.matrix <<- solve
  get.inverse <- function() inverted.matrix
  
  list(
    set = set, 
    get = get,
    set.inverse = set.inverse,
    get.inverse = get.inverse)  
}

## get inverse of the cacheable matrix returned by makeCacheMatrix()
cacheSolve <- function(cacheable.matrix, ...) {
  inverted.matrix <- cacheable.matrix$get.inverse()
  
  # if matrix is null, get inverted matrix
  if(!is.null(inverted.matrix)) {
    message("Getting inverse matrix")
    return(inverted.matrix)
  }
  
  # no cached matrix available, make
  matrix.to.inverse <- cacheable.matrix$get()
  inverted.matrix <- solve(matrix.to.inverse)
  cacheable.matrix$set.inverse(inverted.matrix)
  inverted.matrix
  
}
