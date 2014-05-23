## This function creates a special "matrix" object that 
## can cache its inverse.

## Computing the inverse of a square matrix can be done with 
## the solve function in R. For example, if X is a square 
## invertible matrix, then solve(X) returns its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # set default value
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getsolve()
  ## check if already inversed, if so return m
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## if not then do the inverse matrix computation
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m 
  
}
