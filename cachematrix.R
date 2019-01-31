## Put comments here that give an overall description of what your
## functions do
# The makeCacheMatrix function will return a list with functions to set the matrix
# get the matrix, set the inverse matrix, and get the inverse matrix. The cacheSolve function
# will be used to run the makeCacheMatrix function and pass the values in the list (functions) 
# returned from the latter to the former, caching the values passed.
## Write a short comment describing this function
# Will write the function makeCacheMatrix set, get, setinv, getinv.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
# This function will calculate the inverse and cache the result.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
