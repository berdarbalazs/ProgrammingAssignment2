## Functions to cache the inverse vale of a matrix
## makeCacheMatrix - create a special object to manage the data
## cacheSolve - get the cached inverse vale from makeCacheMatrix object (if this value is missing this function will compute it)

## makeCacheMatrix creates a special object what contains a matrix and can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## i - store the inverseatrix
  ## x - store the orinal matrix
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  ## getter / setter for inverse matrix
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i
  ## Return with the cacheMatrix structure
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Try to get inverse from cache
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## Get the original matrix calculate the inverse matrix and set it as inverse
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  
  ## Return a matrix that is the inverse of 'x'
  i
}

