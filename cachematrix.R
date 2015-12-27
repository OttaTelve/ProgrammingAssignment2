## Caching the Inverse of a Matrix

## The first function, `makeCacheMatrix` creates a special "matrix", which is
## really a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  
  ## stores the four functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The following function "cacheSolve" calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it 'get's the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the data and sets the inverse a the `setinverse` function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse() ## get the inverse of the matrix from the cache
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## computes the inverse of the given matrix in case it was not calculated yet
  data <- x$get()
  inv <- solve(data, ...)
  
  ## stores the calculated inverse of the matrix in the cache
  x$setinverse(inv)
  
  ## returns the calculated inverse of the matrix
  inv
}
