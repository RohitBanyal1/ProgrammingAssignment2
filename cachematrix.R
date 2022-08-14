## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# library(MASS) is used to calculate inverse of a square as well as non squared matrices

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  # initialized inverse as null
  inv <- NULL                            
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # gets matrix x
  get <- function()x
  setinv <- function(inverse) inv <<- invverse
  getinv <- function(){
    # function to obtain inverse of x
    inver <- ginv(x)
    inver%*%x
  } 
  list(set=set, get=get,setinv=setinv,getinv=getinv)
}

## Write a short comment describing this function
# Use to get the cache data

cacheSolve <- function(x, ...) {
        
  inv <- x$getinv()
  # check if inverse is null
  if (!is.null(inv)) {
    message("getting cached data!")
    return(inv)
  }
  data <- x$get()
  # calculates inverse value
  inv <- solve(data,...)
  ## Return a matrix that is the inverse of 'x'
  x$setinv(inv)
  inv
}
