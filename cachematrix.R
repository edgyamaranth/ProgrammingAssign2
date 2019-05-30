## We will have two variables: makeCacheMatrix which stores a matrix
## and cacheSolve which caches the inverse of makeCacheMatrix

## inv: Invert a numeric or complex matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                         
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

##


## Getting the inverse of makeCacheMatrix

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}
