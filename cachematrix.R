## Put comments here that give an overall description of what your
## functions do

## this function gets an matrix and stores in cache its inverse
## x.get() returns the matrix
## x.set() updates the matrix with a new set of values
## x.getInv() returns the inverse from the cache

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  getInv <- function() inv
  list(set = set, get = get, getInv = getInv)
}

## the matrix is stored in data <- x$get())
## the inverse matrix is stored in inv <- x$getInv()
## first the function determines if the inverse of the matrix has previously been calculated. 
##if yes than it returns its value from the cache
##if is a new matrix it checks whether its determinant is not zero, an essential condition for a matrix to be invertible
##if the determinant is 0 than it promts to introduce a nex matrix wich can be set by x$set()
##if the determinant is not 0 than it calculates the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if (!is.null(inv)){
    message("cached data")
    return(inv)
  }
  data <- x$get()
  if (det(data) == 0){
    message("Determinant is 0! Choose another matrix!") 
  }
  else{
    inv <- solve (data)
    inv
  }
  
}
