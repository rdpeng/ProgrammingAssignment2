## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix<- function(x=matrix())
{
  inv<- NULL
  set<- function(y) ## set the matrix
  {
    x<<- y
    inv<<- NULL
  }
  get<- function() ## get the matrix
  {
    x
  }
  setinv<- function(inverse) ##set the inverse
  {
    inv<<- inverse
  }
  getinv<- function() ##get the inverse
  {
    inv
  }
  list(set=set,get=get,setinv=setinv,getinv=getinv)  ## this list is used as the input to cacheSolve()
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<- x$getinv()
  if(!is.null(inv)) ## if the inverse has already been calculated
  {
    message("getting cached data")
    return(inv)  ## get it from the cache and skip the computation
  }
  mat.data=x$get() ## otherwise, calculate the inverse
  inv=solve(mat.data,...)
  x$setinv(inv) ## set the value of inverse in the cache via setinv function
  return(inv)
}
solve<- function(mat)
{
  temp <- makeCacheMatrix(mat) ## generate a new matrix
  cacheSolve(temp) ## output the inverse matrix

}
