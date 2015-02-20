
## These functions takes a matrix and find its inverse. Rather than compute the inverse every time we would like it, however, the following two functions will compute it, and preserve it.

## This first function will take a matrix, and make a list that will be called by the next function. This list is a new 'function' which will set and get the values of the matrix, and set and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function will actually compute the inverse of the matrix. However, if we have already computed the inverse previously, it will tell us that we are asking for previously cached data.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
