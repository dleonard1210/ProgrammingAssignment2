## This source file contains two functions, makeCacheMatrix and cacheSolve, which work together to 
## generate and cache the inverse of a matrix


## makeCacheMatrix takes an optional square matrix argument, and returns a list object that includes
## 4 functions (get, set, getinverse, setinverse), and two square matrices, one of which is the
## inverse of the other.
##
## The set function is used to define a square matrix; if the object passed in is not a square matrix,
## an error message is generated.
##
## The get function returns the value of the square matrix
##
## The setinverse function updates the value of the matrix's inverse; it must be a square matrix with
## the same dimensions as the original matrix, or an error message is generated.
##
## The getinverse function returns the value of the matrix's inverse


makeCacheMatrix <- function(m = matrix()) {
  inverse <- NULL  ## initialize the inverse matrix value to NULL
  
  if (!is.null(m) & (!is.matrix(m) | dim(m)[1] != dim(m)[2])) {
    message("Function argument must be a square matrix!")
    return()
  }
  
  set <- function(x) {
    if (!is.matrix(x) | dim(x)[1] != dim(x)[2]) {
      message("Matrix must be Square!")
      return()
    }
    m <<- x
    inverse <<- NULL  ##initialize the inverse matrix to NULL
  }
  get <- function() m
  
  setinverse <- function(i) {
    if (is.null(m) | !is.matrix(i) |sum(dim(i) == dim(m)) != 2) {
      message("The matrix and its inverse have mismatched dimensions!")
      return()
    }
    inverse <<- i
  }
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve takes as input a list object created by makeCacheMatrix; it calculates the inverse of
## the matrix included in that list, adds the inverse to the list, and returns its value
##
## Note: not all square matrices have an inverse; if an inverse cannot be computed, this function will
## trigger an error and no object will be returned

cacheSolve <- function(x, ...) {
  
  i <- x$getinverse()
  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  i <- solve(x$get())
  x$setinverse(i)
  i
}
