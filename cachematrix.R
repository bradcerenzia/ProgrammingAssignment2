## Functions for creating and using inverted matrices which caching ability


## Create cacheable matrix for input
makeCacheMatrix <- function(original.matrix = matrix()) {
  
  if (!is.matrix(original.matrix)) {
    stop("Please give a matrix")
  }
  
  inverted.matrix <- NULL
  
  set <- function(y) {
    original.matrix <<- y
    inverted.matrix <<- NULL
  }
  
  # Functions for getting and setting cached inv. matrix value
  get <- function() original.matrix
  # Inversing the matrix using build in solve() function in R
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
  
  if(!is.null(inverted.matrix)) {
    message("Getting cached inverse matrix")
    return(inverted.matrix)
  }
  
  # no cached matrix available, make
  matrix.to.inverse <- cacheable.matrix$get()
  inverted.matrix <- solve(matrix.to.inverse)
  cacheable.matrix$set.inverse(inverted.matrix)
  inverted.matrix
  
}
