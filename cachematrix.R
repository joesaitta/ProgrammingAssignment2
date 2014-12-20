## Functions to calculate the inverse of a square matrix, or return the
## cached inverse if the calculation has already been performed

## Function: makeCacheMatrix
## Purpose: make a 'matrix' object that caches its inverse
makeCacheMatrix <- function(x = matrix()) {

## Initialize m
  m <- NULL
  set <- function(y) {

## Assign x and m variables to a different environment
## allowing the calculated inverse to be cached
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m

## Return a special matrix that sets and gets the 
## matrix and its inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Function: cacheSolve
## Purpose: calcuate the inverse of a square matrix if it has not
##          already been calculated
cacheSolve <- function(x, ...) {

## Return a matrix that is the inverse of 'x'
  m <- x$getinv()

## Check to see if the inverse has already been calculated
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }

## If not, calculate the matrix inverse
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}