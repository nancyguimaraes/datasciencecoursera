## The functions below are used to cache the inverse of a matrix to avoid costly computation.
## cacheSolve returns the inverse of a matrix that is created using makeCacheMatrix.
## Example of usage:
## myMatrixObj1 <- makeCacheMatrix(data.frame(c1=1:2,c2=3:4))
## inv_matrix1 <- cacheSolve(myMatrixObj1)


## Creates a matrix object that can cache and retrieve a matrix,
## and can cache and retrieve the inverse of a matrix.

makeCacheMatrix <- function(m = matrix()) {
  im <- NULL
  setmatrix <- function(y) {
    m <<- y
    im <<- NULL
  }
  getmatrix <- function() {
    m
  }
  setinvmatrix <- function(matrix) {
    im <<- matrix
  }
  getinvmatrix <- function() {
    im
  }
  list(setmatrix=setmatrix, getmatrix=getmatrix, 
       setinvmatrix=setinvmatrix, getinvmatrix=getinvmatrix)
}


## Computes the inverse of the matrix returned by makeCacheMatrix above.
## It verifies if the inverse of a matrix is already calculated.
## If yes, then retrieve the inverse from the cache.
## If not, then compute the inverse and cache it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  im <- x$getinvmatrix()

  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }

  data <- x$getmatrix()
  im <- solve(data)
  x$setinvmatrix(im)
  im
}
