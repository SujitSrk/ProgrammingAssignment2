## The two functions demonstrates the use of cache for reducing time for calculation of repetetive tasks
## Our task here is to calculate the inverse of a matrix and store it in a cache 
## by making use of scoping rules in R
 

##The function makeCacheMatix return a list containing 4 functions viz function to  
##set the value of the matrix (which is passed as argument)
##get the value of the matrix
##set the value of the inverse of the matrix in the parent environment (function makeCacheMatrix )
##get the value of the inverse of the matrix (by retrieving the value in the globali variable 
##in parent enviroment set by the set function)

makeCacheMatrix <- function(x = matrix()) {
  
  globali <- NULL
  ## Set the data (matrix) in parent environement
  set <- function(y) {
    x <<- y
    globali <<- NULL
  }
  ## Get the data (matrix) from the parent environement
  get <- function() x
  ## Set the inverse of matrix in parent environement
  setinverse <- function(inverse) globali <<- inverse
  
  ## get the inverse of matrix in parent environement
  getinverse <- function() globali
  
  ## Return a list of the four functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
}


## The following function calculates the inverse of the matrix
## created with the above function. 
##However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the matrix and 
##sets the value of the inverse in the cache via the setinverse function.

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
