
## The first function makeCacheMatrix, containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse matrix
##get the value of the inverse matrix

makeCacheMatrix <- function ( m = matrix()) {
  inv <- NULL
  set <- function (m2){
    m <<- m2
    inv <- NULL
  }
  get <- function() m
  setsolve <- function(inverse) inv <<- inverse
  getsolve <- function() inv
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

##The following function calculates the inverse of the special matrix created with the above function. 

cacheSolve <- function(x, ...) {
  inv <- x$getsolve()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix1 <- x$get()
  inv <- solve(matrix1, ...)
  x$setsolve(inv)
  inv
}

