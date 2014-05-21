## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  globali <- NULL
  set <- function(y) {
    x <<- y
    globali <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) globali <<- inverse
  getinverse <- function() globali
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  locali <- x$getinverse()
  if(!is.null(locali)) {
    message("getting cached data")
    return(locali)
  }
  data <- x$get()
  locali <- solve(data, ...)
  x$setinverse(locali)
  locali
  
}
