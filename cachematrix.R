## A Caching Object:
##
## The functions below create a cacheMatrix object that will hold a 
## solved/inverted copy of a provided matrix (if the matrix is invertible),
## and provide the function to perform the actual inversion and caching.

## This function  creates a cache'able matrix.
##
makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x
  setinverse <- function(x_inverse) x_inv <<- x_inverse
  getinverse <- function() x_inv
  list( set = set,get = get, 
        setinverse = setinverse,
        getinverse = getinverse )
}


## This function inverts and caches the matrix.
##
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x_inv <- x$getinverse()
  if(!is.null(x_inv)) {
    message("getting cached matrix inverse.")
    return(x_inv)
  }
  data <- x$get()
  x_inv <- solve(data,...)
  x$setinverse(x_inv)
  x_inv
}