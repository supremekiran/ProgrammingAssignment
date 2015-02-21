## The function creates a special matrix object that can cache 
## its inverse 

## This function creates matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    m <<- y
    inv <<- NULL
  }
  get<- function(){m} 
  setInverse <- function(inverse){inv <<- inverse}
  getInverse <- function(){inv}
  list (set = set, get = get, getInverse = getInverse, setInverse = setInverse)
}


## This function returns the inverse of special matrix returned by 'makeCacheMatrix' function 
## if the inverse is already calculated it returns it from the cache  (if the matrix is not changed)

## Assuming provided matrix is invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' 
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
