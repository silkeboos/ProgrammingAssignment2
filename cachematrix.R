## Put comments here that give an overall description of what your
## functions do

## Creates a list, which contains functions to set and get the value of a matrix
## and set and get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function()inv
  list(set=set, get=get,setinverse=setinverse, getinverse=getinverse)
}


## Function, which returns the inverse of the matrix. In the first step it checks if
## the inverse already has computed. If it is the case, it gets the result and it
## skips the prozess. If not the inverse is computed and the setinverse function
## sets the value in the cache

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
          message("getting cached data.")
          return(inv)
        }
        data <- x$get ()
        inv <- solve(data)
        x$setinverse(inv)
        inv
        
}
