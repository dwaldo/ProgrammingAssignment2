## Put comments here that give an overall description of what your
## functions do

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### makeCacheMatrix: This function creates a special "matrix" object
### that can cache its inverse. It creates a special "matrix", which is
### really a list containing a function to
###
### 1.  set the value of the matrix
### 2.  get the value of the matrix
### 3.  set the value of the inverse of the matrix
### 4.  get the value of the inverse of the matrix
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(val) inv <<- val
  getinv <- function() inv
  list(set = set, get = get,
    setinv = setinv,
    getinv = getinv)
}

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### cacheSolve: This function computes the inverse of the special
### "matrix" returned by `makeCacheMatrix` above. If the inverse has
### already been calculated (and the matrix has not changed), then
### `cacheSolve` should retrieve the inverse from the cache and skips
### the computation. Otherwise, it calculates the inverse of the
### matrix and set the value of the inverse in the cache via the 
### `setinv` function.
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
}
