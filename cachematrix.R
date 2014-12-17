##This function takes the matrix with inverse to be cached and returns a list containing 4 functions
##Dec 16 2014




makeCacheMatrix <- function(x = matrix()) 
{
  inv=NULL
  #set(matrix y) sets the matrix given and once the matrix to be inverted is reset the inverse is set to NULL
  set=function(y)
  {
    x<<-y
    inv<<-NULL
  }
  #get()  returns the matrix that needs its inverse cached
  get=function(){return(x)}
  #setInverse(matrix i) sets the inverse of the matrix to be cached once the inverse has been calculated
  setInverse=function(i){inv<<-i}
  #getInverse() returns the inverse of the matrix set in set if it has been calculated and returns NULL otherwise.
  getInverse=function(){return(inv)}
  output=list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
  return(output)
}


##This function takes the list above and:
##if the inverse has been calculated and stored it is returned
##if the inverse has not been calculated then it is calculated, stored and returned.
cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  inv=x$getInverse()
  if(!is.null(inv))
  {
    message("getting cached data\n")
    return(inv)
  }
  data=x$get()
  inv=solve(data,...)
  x$setInverse(inv)
  return(inv)  
}
