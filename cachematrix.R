## This will cache the Inverse of a Matrix 
## Inversing matrix is a time consuming process and it would be useful to cache the inverse of a matrix 
## rather than computin it repeatedly. 

## Write a short comment describing this function.
## A couple of functions are given here which can be used to create a special object that
## stores a matrix & caches the inverse.

## What this function does: This function creates a special "matrix object" which caches the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get= get,
       setInverse = setInverse,
       getInverse = getInverse)
  
  


}


## The inverse of the matrix created by makeCacheMatrix above will be calculated.
## If it (the inverse) is already there given the matrix has not changed, then this should get the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()
  if (!is.null(inv)) 
  {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

##The End
