## The following pair of functions will compute the inverse of a matrix.
## The first function will cache the inverse. The second function will
## output the result by looking up in the cache first,if it cannot find
## the result, it will compute the inverse.

## makeCacheMatrix function will creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The cacheSolve function takes the inverse of the matrix created above.
## It gets the inverse from the cache if it has been computed.
## Otherwise, it will compute the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data,...)
  x$setinverse(inverse)
  inverse  
}
