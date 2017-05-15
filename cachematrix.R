## This file corresponds to programming assignment 2
##Below are provided two functions. The first one makeCacheMatrix creates an object that can cache its inverse
##The second one computes the inverse



## 'makeCacheMatrix' his function creates a special "matrix" object that can cache its inverse.
###input : the "matrix" object
###output : a list of functions to get, set, set inverse and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## `cacheSolve`: This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. If the inverse has
   ##already been calculated (and the matrix has not changed), then
   ##`cacheSolve` should retrieve the inverse from the cache.
#input : a special matrix created by the function createCache
#output : the result of the inversion

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
