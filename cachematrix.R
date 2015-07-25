## Two functions, makeCacheMatrix and cacheSolve, are used to calculate, store, and return the inverse of a matrix.
## If the matrix has been solved, a cached solution is returned.

## makeCacheMatrix contains 4 functions: set, get, setsolution, and getsolution.
## Before defining these functions, the matrix solution object, mat.solution, is reset to NULL.
## set takes an argument y, which is then assigned to x from a different environmnet.
## get simply returns the value of x.
## setsolution computes the inverse of the matrix using solve, and assigns that to the object mat.solution.
## getsolution returns the value of mat.solution.
## The four functions contained in makeCacheMatrix are stored in a list.

makeCacheMatrix <- function(x = matrix()) {
  mat.solution <- NULL
  set <- function(y) {
    x <<- y
    mat.solution <<- NULL
  }
  get <- function() x
  setsolution <- function(solve)mat.solution <<- solve
  getsolution <- function() mat.solution
  list(set = set, get = get,
       setsolution = setsolution,
       getsolution = getsolution)
}


## The cacheSolve function returns a matrix that is the inverse of 'x', either solving it, 
## or returning a cached solution

cacheSolve <- function(x, ...) {
  mat.solution <- x$getsolution()
  if(!is.null(mat.solution)) {
    message("getting cached data")
    return(mat.solution)
  }
  data <- x$get()
  mat.solution <- solve(data, ...)
  x$setsolution(mat.solution)
  mat.solution
}
