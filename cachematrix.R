## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function creates a list that contains four functions, namely set(), get(), setinverse() and getinverse()
## The set function takes in a matrix, and overrides the inverse matrix (the result) stored in the variable 'inverse' with NULL.
## The get function returns the matrix that is input through the set function
## The setinverse function takes in the result generated from the cachesolve function, which is the inverse matrix
## The getinverse function returns the result stored in the variable 'inverse', which contains the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  
  set <- function(y){
    
    x <<- y
    inverse <<- NULL
    
  }
  
  get <- function() x
  
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


cachesolve <- function(x, ...) {

  inverse <- x$getinverse()        ## Store the existing(if any) inverse matrix in the 'inverse' variable
  
  if(!is.null(inverse)){           ## If the 'inverse' variable contains the inverse matrix, print out the
    message("getting cached data") ## message and return the inverse matrix
    return(inverse)
  }
  
  inverse <- solve(x$get(), ...)   ## Otherwise if the 'inverse' variable is empty, the solve function is used 
  x$setinverse(inverse)            ## to solve for the inverse matrix, where it is then stored in the 'inverse'
  inverse                          ## variable and then returned

}
