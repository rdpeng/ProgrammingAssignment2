## Put comments here that give an overall description of what your
## functions do

##  makeCacheMatrix creates a vector of functions that 
##  gets and sets the value of the matrix,
##  gets and sets the value of the inverse
##  Call it in a matrix that we will store 

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y=matrix()){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setinv<-function(inverse)inv<<-inverse
  getinv<-function() inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)

}


## cacheSolve takes the vector created by makeCacheMatrix as its formal argument
## and tests it  to see if the matrix has already been solved.
## if it was already solved, it returns the cached value
## else it calculates the inverse of the operation, caches the value 
## and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x[4]
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x[2]
  inv<-solve(data,...)
  x[3](inv)
  inv
}
