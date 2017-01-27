## There are times that you can save computing time by caching the values of 
## rather than repeatedly computing the same figures.
## The following functions will be used to cache the inverse of a matrix.

## makeCacheMatrix is a function that will set/get the value of the matrix, as
## well as, set/get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      ## setting the value
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      ## getting the value
      get <- function() x
      ## setting the inverse
      setinverse <- function(inverse) inv <<- inverse
      ## getting the inverse
      getinverse <- function() inv
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data.")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data)
      x$setinverse(inv)
      inv
}
        
