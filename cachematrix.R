## This function creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # store in cache matrix named cm
  # initialize to NULL
  mat <- NULL
  set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  get <- function() x
  setMatrix <- function(inverse) mat <<- inverse
  getInverse <- function() cache
  
  list(set = set, get = get,
       setMatrix = setMatrix,
       getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix

cacheSolve <- function(x=matrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
  mat <- x$getmatrix()
  if(!is.null(mat)){
    message("getting cached data")
    return(mat)
  }
  
  matrix <- x$get()
  mat <- solve(matrix, ...)
  x$setmatrix(mat)
  mat
}