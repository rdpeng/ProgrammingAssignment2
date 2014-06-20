## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
   M_inv<-NULL
   set<-function(y){
   x<<-y
    M_inv<<-NULL
   }
   get<-function()x
   setM_inv<-function(MatrixInverse) M_inv<<-MatrixInverse
   getM_inv<-function()M_inv
   list(set=set,get=get,setM_inv=setM_inv,getM_inv=getM_inv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   M_inv<-x$getM_inv()
   if(!is.null(M_inv)){
   message("getting cached data")
   return(M_inv)
   }
   data<-x$get()
   M_inv<-solve(data,...)
   x$setM_inv(M_inv)
   M_inv
}
