## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(mat=matrix()){
  inv<-NULL
  set <- function(mat2) {
    mat <<- mat2
    inv <<- NULL
  }
  get <- function() mat
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


##   The following function calculates the inverse of the 'special matrix' if it's a new matrix & returns the previously
## calculated inverse if it has already been calculated (its value is cached) 

cacheSolve <- function(mat, ...) {
  inv <- mat$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- mat$get()
  inv <- solve(data, ...)
  mat$setinv(inv)
  inv
}
