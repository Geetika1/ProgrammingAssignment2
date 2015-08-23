## The functions makeCacheMatrix and cacheSolve compute the inverse of a matrix and check if the
##inverse has already been calculated or not. If the inverse has been previously calculated,
##the inverse comes from the cache.

## This function sets the value of a matrix, gets the value of that matrix, 
##sets the inverse of the matrix, and gets the inverse of that matirx.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks if the inverse has already been calculated. If it has, then the
## inverse is retrieved from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting data from cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
