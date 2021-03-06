### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Assignment #2:
### A set of functions that enable the caching of the inverse of a 
### matrix -- a potentially time-consuming computation. We use the 
### scoping rules of the R language to preserve state inside of an 
### R object.
###
### Example Usage:
### > c = rbind(c(1, -1/4), c(-1/4, 1))
### > cc <- makeCacheMatrix(c)
### > cacheSolve(cc)
###           [,1]      [,2]
### [1,] 1.0666667 0.2666667
### [2,] 0.2666667 1.0666667
### > cacheSolve(cc)
### getting cached data
###           [,1]      [,2]
### [1,] 1.0666667 0.2666667
### [2,] 0.2666667 1.0666667
###
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### makeCacheMatrix: This function creates a special "matrix" object
### that can cache its inverse. It creates a special "matrix", which is
### really a list containing a function to:
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
