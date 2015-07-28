##makeCacheMatrix and cacheSolve can be used to calculate and cache the inverse of a matrix. 

## makeCacheMatrix creates a special "matrix" that can cache its inverse. 
##This function creates a vector that contains a list of functions that can 
##be used to cache the value of a matrix, and get its value from that environment,
## calculate its inverse and get its inverse.

makeCacheMatrix <- function(x = matrix()) {

  m<-NULL
  set<- function (y){
    x<<- y
    m<<-NULL
  }
  
  get<- function() x
  setinverse <- function(solve) m<<-solve
  getinverse <- function() m
  list (set=set, get=get, setinverse = setinverse, getinverse = getinverse)
  
}

## cacheSolve first checks if the inverse of the "matrix" object created by makeCacheMatrix has already been cached. 
##If it has it returns the cached value. 
##If not it calculates and cashes the inverse of the maxrix x using functions defined in makeCacheMatrix.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if (!is.null(m)){
    message ("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
  
}
