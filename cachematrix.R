## Put comments here that give an overall description of what your
## functions do



## parent function that takes in the same arguments matrix()
makeCacheMatrix <- function(data = NA, nrow = 1, ncol = 1, byrow = FALSE,
                            dimnames = NULL) {
  inv <- NULL
  
  set <- function(y){
    x <<- matrix(data=data, nrow=nrow, ncol=ncol, byrow=byrow, dimnames=dimnames)
    inv <<- NULL
  }
  
  get <- function(){
    x
  }
  
  setinverse <- function(inverse){
    inv <<- inverse #modifying value at the parent function level
  }
  
  getinverse <- function(){
    inv
  } 
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## CacheSolve returns the cached inverse of x if availble. If not, it will calculate by using solve().

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse of x")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  message("setting cached inverse of x")
  x$setinverse(m)
  m
}
