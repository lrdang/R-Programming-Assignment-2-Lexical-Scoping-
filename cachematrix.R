## Our aim in assignment is to write two functions,"makeCacheMatrix" and "cacheSolve", namely. 
## The above two functions could cache the inverse of a matrix
  
## Write a short comment describing this function
## makeCacheMatrix is a function that creates a special "matrix" object that can 
## cache the inverse of the input, which is an invertible square matrix)

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
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
##---------------Checking the program------------------------
## m <- diag(x = 3,4,4)
## m1 <- makeCacheMatrix(m)
## m1$get()
## cacheSolve(m1)
##[,1]      [,2]      [,3]      [,4]
##[1,] 0.3333333 0.0000000 0.0000000 0.0000000
##[2,] 0.0000000 0.3333333 0.0000000 0.0000000
##[3,] 0.0000000 0.0000000 0.3333333 0.0000000
##[4,] 0.0000000 0.0000000 0.0000000 0.3333333