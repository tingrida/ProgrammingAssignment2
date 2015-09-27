## Put comments here that give an overall description of what your
## functions do
## These two functions can be used to store and cache matrix and it's inverse
## As calculating inverse may take lot of time, caching inverse may save lot of cpu time
## And if you already know inverse of the matrix you can use it without calculating!


## This function creates ist with 4 functions:
## function get() which returns actual matrix
## function set(matrix) which overwrites actual matrix and resets inverse
## function getinverse which will return inverse if any available, overwise NULL
## function setinverse which overwrites inverse with any other matrix (even if it is not inverse of original one!)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<-y
    inv <<- NULL
  }
  get <- function() x
  getinverse <- function() inv
  setinverse <- function(inverse) inv <<- inverse
  list(set=set, get=get, getinverse=getinverse, setinverse=setinverse)
}


## This function calculates inverse of matrix defined with function makeCacheMatrix
## This function will not work with simple matrix!
## This function assumes that inverse exist!

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("cached solve found! getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
