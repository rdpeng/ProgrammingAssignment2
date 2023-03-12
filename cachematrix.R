## these combining functions will return matrix with the properties to cache any
## inverted matrices for repetitive use of the function.

##  function creates matrix that caches resulted inverse.

makeCacheMatrix <- function(x = matrix()) {
  reverse <- NULL
  set <- function(y){
    x <<- y
    reverse <<-NULL}
  get <- function()x
  #retrieve matrix x
  setinverse<- function(inverse) 
    reverse<<- inverse
  getinverse<- function(){
    reverse}
  #obtain inversion of matrix
  list( set= set, get=get,
        setinverse=setinverse,
        getinverse=getinverse)
}



#function returns inverse of matrix object.

cacheSolve <- function(x, ...) {
  cacheSolve <- function(x=matrix, ...)
    #gets cache data
  {
    reverse<- x$getinverse()
    if(!is.null(reverse))
      ## verifying if inverse is NULL
    {
      message(" cached data is served")
      return(reverse)
      #return inverse
    }
    matrix <- x$get()
    reverse <-solve(matrix)
    x$getinverse(reverse)
    reverse
    }
    
        ## Return a matrix that is the inverse of 'x'
}
