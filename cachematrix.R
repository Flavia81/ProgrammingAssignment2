## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix is a function that creates and returns a list of functions
# that will be used by the next function (cacheSolve) 

makeCacheMatrix <- function(x = matrix()) {
# stores the cached value in the variable a and initializes it to NULL
a <- NULL

# this function creates the matrix in the working environment
set <- function(y) {
  x <<- y
  a <<- NULL
}

# this function gets the value of the matrix
get <- function() x

# this function inverts the matrix and stores it in cache
setMat <- function(inv) a <<- inv

# this function gets the inverted matrix from cache
getInv <- function() a


list(set = set, get = get, setMat = setMat, getInv = getInv)
}


## Write a short comment describing this function

# the function cacheSolve calculates the inverse of the matrix 
#created in the previous function makeCacheMatrix

cacheSolve <- function(x, ...) {

  # get the inverse of the matrix if it is stored in cache
  a <- x$getInv()
  
  # return inverted matrix from cache if it exists otherwise create the matrix
  if (!is.null(a)) {
    message("cached matrix")
    return(a)
  }
  
  # create matrix if it does not exist
  mat <- x$get()
  
  # calculates the inverse 
  mat = x$get()
  inv = solve(mat, ...)
  
  # sets the value of the inverse in the cache
  x$setMat(inv)
  
  # Return a matrix that is the inverse of 'x'
  return(inv)
}
