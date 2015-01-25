##Matrix inversion is usually a costly computation
##there are some benefit to caching the inverse of a matrix rather than computing it repeatedly 
##below are a pair of functions that cache the inverse of a matrix.


##The makeCacheMatrix function creates a special "matrix", which can cache its inverse. 
##it creates a list that contains 4 member functions: set, get, setinv and getinv


makeCacheMatrix <- function(x = matrix()) {

      minv <- NULL 
      set <- function(y) {
	  x <<- y
	  minv <<- NULL
      }

      get <- function() x 
      setinv <- function(inv) minv 
      getinv <- function() minv 
      list(set = set, get = get,
	       setinv = setinv,
	       getinv = getinv)
  }



##The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      minv <- x$getinv() 
      if(!is.null(minv)) { 
	  message("getting cached data")
	  return(minv) 
      }
      data <- x$get() 
      minv <- solve(data) 
      x$setinv(minv) 
      minv
  }
