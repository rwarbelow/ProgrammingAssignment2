## The two functions below work together in order to create
## a matrix object that can cache an inverse of itself and
## retrieve that cached value (or compute the inverse if it 
## is not cached).

## This function returns a special matrix object that 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inversematrix <- NULL
  set <- function(y) {
    x <<- y
    inversematrix <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inversematrix <<- inverse
  getinverse <- function() inversematrix
  list(get = get, set = set, 
       getinverse = getinverse, 
       setinverse = setinverse)
}

## This function either retrieves the cached inverse or
## calculates it if it has not been cached. 

cacheSolve <- function(x, ...) {
  inversematrix <- x$getinverse()
  if(!is.null(inversematrix)) {
    message("getting cached matrix")
    return(inversematrix)
  }
  data <- x$get()
  inversematrix <- solve(data, ...)
  x$setinverse(inversematrix)
  inversematrix
}
