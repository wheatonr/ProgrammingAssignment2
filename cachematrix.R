## R Programming Assignment 2
## make a matrix object that caches its solution

## make a new object
## eg m<-makeCacheMatrix(matrix(rnorm(9), nrow=3, ncol=3))
##
## m$get returns original matrix
## m$getinverse returns cached inverse or NULL if not computed yet
## m$set loads a new matrix, resets cached value
## m$setinverse caches a new value for the inverse

makeCacheMatrix <- function(x = matrix()) {
  ## init cache value to NULL
  cached_inverse <-NULL
  ## load new matrix, reset cache
  set <- function(y) {
    x <<-y
    cached_inverse <<-NULL
  }
  ## return matrix
  get <- function() x
  ## cache inverse
  setinverse <- function(inverse) cached_inverse <<-inverse
  ## return inverse
  getinverse <- function() cached_inverse
  ## return a list of available functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}



## check for a cached solution else solve
## and return cached solution
## eg cacheSolve(m)

cacheSolve <- function(x, ...) {
  ## retrieve cached value
  inverse <-x$getinverse()
  if (!is.null(inverse)) {
    ## inverse found in cache, nothing to do
    message("getting inverse from cache")
  } else {
    # inverse not found in cache, compute and store
    message("computing new inverse")
    inverse <- solve(x$get())
    x$setinverse(inverse)
  }
  # return inverse to caller
  inverse
}
