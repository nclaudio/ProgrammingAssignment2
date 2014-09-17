## This function creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL ##will recieve value inverse_m
  }
  get <- function() {x} ##here x is a free variable, will get its value from makeVector environment
  setInv <- function(inverse_m) {inverse <<- inverse_m} ##is going to pass inverse_m to inverse in the makevector environm
  getInv <- function() {inverse} ##will recieve value of inverse from above
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}i


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInv()
  if(!is.null(inverse)) {
    message("getting cached inverse matrix")
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$setInv(inverse)
  inverse
}
