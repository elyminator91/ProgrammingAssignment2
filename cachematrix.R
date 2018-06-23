## makeCacheMatrix creates a list of functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

## This function makes a cache of the matrix

makeCacheMatrix <- function(x = matrix()) {
  #initialise matrix inverse
  inv <- NULL
  
  #method to set matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #method to get matrix
  get <- function() {
    x
  }
  
  #method to set matrix inverse
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  
  #method to get matrix inverse
  getinverse <- function() {
    inv
  }
  
  #return list of methods
  return(list(set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse))
}


## This function solves for the matrix inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #return inverse from cache if already exists
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  #cache inverse if not already exist in cache
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  return(inv)
}
