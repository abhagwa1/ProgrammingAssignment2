## Put comments here that give an overall description of what your
## functions do

## this function allows for the creation of a special matrix

makeCacheMatrix <- function(x = matrix()) {
  matr <- NULL
  set <- function(y) {
    x <<- y
    matr <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) matr <<- inverse
  getinverse <- function() matr
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## this function computes the inverse of the special matrix

cacheSolve <- function(x, ...) {
  matr <- x$getinverse()
  if (!is.null(matr)) {
    message("getting cached data")
    return(matr)
  }
  data <- x$get()
  matr <- solve(data, ...)
  x$setinverse(matr)
  matr
}

