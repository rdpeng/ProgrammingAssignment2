## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
              int<- NULL #set initial inverse matrix value as null since it requires a default value
              
              #This creates the set function to create matrix
              set<- function(y){
                x<<- y
                int<- NULL
              }
              #This will create the get function to pull the current matrix
              get<- function() x
              
              #This will create the function which stores the solved inverted matrix
              setInverted <- function(inverted) int <<- inverted
              
              #This will allow us to pull the cached inverted matrix if there is one cached
              getInverted <- function() int
              
              #the list below allows us to pull parts of this matrix with $
              list(set = set,
                   get = get,
                   setInverted = setInverted,
                   getInverted = getInverted)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  int <- x$getInverted()
    #check if there is cached data and if so do this
    if(!is.null(int)){
    message("getting cached data")
    return (int)
    }
  #otherwise calculate inverse and print it
      
      data<- x$get()
      
      int<-solve(data)
      
      x$setInverted(int)
      
      int
}