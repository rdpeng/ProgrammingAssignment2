##How to do the inverse of a matrix in R  

#create a matrix  object

makeCacheMatrix <- function (m=matrix()){
   y <- NULL
   set <- function (x)   {
   m <<- x
    y <<- NULL
   }
   get <- function () m
   setinverse <- function (solve) y <<- solve
   getinv <- function ()  y
   list(set=set, get=get,
   setinverse=setinverse,
   getinverse=getinverse)
}  


#return the inverse matrix  of m

cacheSolve <- function (m,...){
   y <- m$getinv()
   if(!is.null(y)){
   message ("getting cached data")
   return (y)
   }
   data <- m$get()
   m$setinv(y)
   y
}
