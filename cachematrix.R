## A pair of functions that facilitate the calculation of matrix inverses
## using by caching solutions of matrix that has already been processed
##
## Example:
##   a = matrix(rnorm(100), 10, 10)
##   b = makeCacheMatrix(a)
##   cacheSolve(b)  # First call to cacheSolve
##   cacheSolve(b)  # This will return the cached result


## I am a matrix value holder, whose purpose is to wrap an instance of
## the passed in matrix, and provide a four-element function list:
##   matrix setter (set)
##   matrix getter (get)
##   matrix inverse setter (setinverse)
##   matrix inverse getter (getinverse)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## I calculate the inverse of a "matrix value holder", obtained with makeCacheMatrix(...)
## If the inverse of the passed in value holder has already been calculated,
## I will simply return the result back
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
