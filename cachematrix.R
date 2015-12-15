## Avoid recalculating matrix inverse by creating a function list
## that is able to store the results of a previous inverse calculation

## makeCacheMatrix function creates a list of functions used to store
## inverse matrix calculation.  Input is a matrix and output is a list of matrix
## functions.

##set function will cache the matrix
##get will retrieve the matrix
##setinverse will cache the inverse matrix
##getinverse will retrieve the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve requires the matrix list created by makeCacheMatrix
## as input.  Will retrieve inverse matrix result if already calculated
## otherwise will calculate, cache and report the inverse.

## warning, this function does not test if matrix is invertable


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
