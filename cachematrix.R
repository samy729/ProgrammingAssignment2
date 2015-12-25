## Caching the inverse of a matrix :
## Matrix inversion is usually a costly computation and to save time and cost 
## caching the inverse of a matrix is carried out rather than computing it repeatedly.
## Below are the pair of functions that cache the inverse of a matrix.

## Assumption : the matrix supplied here is invertible.

## "makeCacheMatrix" function creates a special "matrix" object that cache inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
    set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,setinverse = setinverse,
       getinverse = getinverse)
}


## "cacheSolve" function computes the inverse of the special "matrix" returned by "makeCacheMatrix" function above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve will 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matr <- x$get()
  inv <- solve(matr, ...)
  x$setinverse(inv)
  inv
}
