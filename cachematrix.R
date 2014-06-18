## Put comments here that give an overall description of what your
## functions do

## W# creates "matrix" object that will cache its inverse.

makeCacheMatrix <- function(mat = matrix()){
  #reset value
  value <- NULL
  
  #anonymous function, takes param and stores value in own enviroment
  #for caching
  set <- function(matY) {
    mat <<- matY
    value <<- NULL
  }
  
  #anonymous function, get intial value
  get <- function() mat
  
  #setter, passes param from anonymous function and stores in own environment
  setSolve <- function(solve) value <<- solve
  getSolve <- function() value
  
  #set list of 4 values 
  list(set = set, get = get, setSolve = setSolve,  getSolve = getSolve)
}

## Write a short comment describing this function

#param mat holds value passed from cacheInvMatrix
cacheSolve<- function(mat){
  
  #gets the cached value
  value <- mat$getSolve()
  
  #if not null, print message and value from cacheInvMatrix
  if(!is.null(value)) {
    message("getting cached solve")
    return(value)
  }
  
  #if null, use solve function to invert matrix
  data <- mat$get()
  value <- solve(data)
  mat$setSolve(value)
  value  
}
