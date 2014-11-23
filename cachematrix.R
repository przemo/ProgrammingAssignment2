## Put comments here that give an overall description of what your
## functions do

# as stated in the assignment instructions the main purpose of the function is
# to create an object with the attached functions that allow for calculating an
# inverse of the matrix, however the object can be accesded multiple times, but
# it is updated only at the time when input data changes. It helps to save time
# when executing and accessing data in the loops.


## Write a short comment describing this function the functions checks if the
## input matrix is squared, it is assumed that all input matrices are
## invertable.

makeCacheMatrix <- function(x = matrix()) {
  d <- dim(x)
  if(d[1] != d[2]) stop("The matrix has no equal dimensios. Provide square matrix.")
  i <- NULL
  ## contructor of the object in the separate environment using <<-
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## getters to obtain the data and store the inverse of the input matrix
  get <- function() x
  setinverse <- function(inv) i <<- inv # storing an inverse in separate environment 
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function help to access the data from the object created by
## makeCacheMatrix() If the output was not created it computes the inverse of
## the matrix, when already exists it gets the content from the object and
## updates the output only when the input data changes.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse() 
  if(!is.null(m)) { # checking if the object exists
    message("getting cached data") # if yes it is jsut returned and no computation is performed 
    return(m)
  }
  data <- x$get() # if not calculate the inverse and return value
  i <- solve(data)
  x$setinverse(i)
  i
}
