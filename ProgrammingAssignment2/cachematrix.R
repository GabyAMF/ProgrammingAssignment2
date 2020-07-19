## Put comments here that give an overall description of what your
## functions do



##This first function is supposed to create a special matrix that is inverseable and caches it. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function computes the inverse matrix of the matrix created above (by the makeCacheMatrix function). 
##If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
