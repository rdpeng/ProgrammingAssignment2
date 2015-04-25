
##The 2 functions can help cache the inverse of a matrix, and eventually 
##not calculate the inverse if it is allready calculated.


##The first function creates a special matrix, which practically contains
##a list of functions who can do:set the matrix,get the matrix,set the inverse,get the inverse

makeCacheMatrix <- function(x=matrix()){
      inv <- NULL
      set <- function(y){
            x <<-y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(inv) inv
      getinv <- function() inv
      list(set=set,
           get=get,
           setinv=setinv,
           getinv=getinv)
}

##The second function calculates the inverse of the matrix,first checking if it is
##allready calculated.If it is necessary to calculate the inverse,it also sets the inverse with
##the setinv function

cacheSolve <- function(x,...){
      inv <- x$getinv()
      if(!is.null(inv)){
            message("calculated before")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data,...)
      x$setinv(inv)
      inv
}
