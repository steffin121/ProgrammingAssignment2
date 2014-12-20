## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix
## This function is designed to store a "Cache" matrix that 
## can be called by cacheSolve below. 

makeCacheMatrix <- function(x = matrix()) {
  # when a matrix is initially created, it has no inverse stored
  matrix_inverse <- NULL
  
  # sets the matrix
  # this function is (ostensibly) only called if one wishes to 
  # change the existing stored matrix as the initially passed x in the 
  # call to makeCacheMatrix is used as the stored matrix
  # Because the passed in x is changed, so must its inverse be reset
  # so the next call to cacheSolve does not use an old matrix's inverse
  set <- function(y) {
    x <<- y
    matrix_inverse <<- NULL
  }

  # returns the matrix x that was created
  get <- function() x

  # Sets the inverse of x
  # set does not calculate the inverse, it merely stores it
  # cacheSolve actually performs the calculation
  setinverse <- function(inverse) matrix_inverse <<- inverse

  # Returns the inverse of x that was stored previously
  # a smarter version of this function, might check if
  # matrix_inverse is NULL then call cacheSolve to calculate the 
  # inverse... recursive debugging issues abound
  getinverse <- function() matrix_inverse 

  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve
## this function is used to wrap a call to solve, which calculates the 
## inverse of a square matrix
## The inverse is only calculated if there is not an existing inverse stored 
## with the cacheMatrix x.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    # first we attempt to retrieve an existing inverted matrix
    invertedMatrix <- x$getinverse()
    
    # if the matrix that is stored is not null, then we return that version
    # thereby exiting the function
    if(!is.null(invertedMatrix)) {
      message("Using cached inverse")
      return(invertedMatrix)
    }

    # otherwise, we retrieve the matrix stored in X a "Cache" matrix
    # we calculate its inversion using solve.
    invertedMatrix <- solve(x$get(), ...)

    # we now store in x the inversion we calculated above
    x$setinverse(invertedMatrix)

    # prints the inverted matrix that was created
    invertedMatrix
}
