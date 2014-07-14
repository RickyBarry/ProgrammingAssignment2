## Caching the Inverse of a Matrix
##
## These functions expose a matrix inversion function called cacheSolve which will
## cache the inverse of a matrix so that it does not need to be computed repeatedly.
## @author Ricky Barry
## @date   2014-07-14

## Given a matrix, create and return a new matrix-like object that
## can cache its inverse.
##
## @param  x a matrix which will be wrapped by the function 
## @return   a matrix-wrapping object which includes methods to
##           get/set data and get/set the cached inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  # get() returns the matrix
  get <- function() x
  # set() clears the data and the cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # getInverse() returns the cached inverse
  getInverse <- function() inv
  # setInverse() saves the inverse to the cache
  setInverse <- function(inverse) inv <<- inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Given a matrix object returned by makeCacheMatrix(), either returned
## the cached inverse or calculate the inverse and cache it.
##
## @param  x    a matrix object returned by makeCacheMatrix()
## @param  ...  any additional arguments to the solve() function
## @return      the inverse of the matrix, which may have been calculated
##              on the fly or returned from a cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' if it has been cached already
  ## otherwise calculate the inverse, cache it and return the inverse
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
}
# calculate the inverse, cache it, and return it
# This function assumes there is an inverse available
# Improvement for future (Allow for singular matrix)
# Error thrown for singular
# Error in solve.default(data, ...) : 
# Lapack routine dgesv: system is exactly singular.
data <- x$get()
inverse <- solve(data, ...)
x$setInverse(inverse)
inverse
}
## Sample run:
## > x = rbind(c(1, 0), c(0, 1))
## > m = makeCacheMatrix(x)
## > m$get()
# [,1] [,2]
# [1,]    1    2
# [2,]    2    1
# > cacheSolve(m)
# [,1]       [,2]
# [1,] -0.3333333  0.6666667
# [2,]  0.6666667 -0.3333333
# > cacheSolve(m)
# getting cached data
# [,1]       [,2]
# [1,] -0.3333333  0.6666667
# [2,]  0.6666667 -0.3333333