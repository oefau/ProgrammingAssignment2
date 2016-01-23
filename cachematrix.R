## R function to cache the inverse of a non-singular matrix
####################################################################
## The first function, makeCacheMatrix creates a special "matrix" object,
## returns a list containing functions to 
##    1. set the  matrix elements
##    2. get the matrix
##    3. set the inverse of the matrix
##    4. get the matrix inverse



makeCacheMatrix <- function(x = matrix()) {
  # x is a non-singular matrix 
  inv <- NULL
  set <-function(y){
    x<<-y
    inv <<- NULL
  }
  get <- function()x
  setinv <- function(inverse) inv<<- inverse
  getinv = function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve returns the inverse of the special "matrix" x, 
## returned by makeCacheMatrix, 
## If the inverse previously computed return the cached inverse

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
          message("getting cached data")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        
        x$setinv(inv)
        return(inv)
}
