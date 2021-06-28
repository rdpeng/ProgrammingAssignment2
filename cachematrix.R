## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##this below function creates a special "matrix" object that can
#cache its inverse

makeCacheMatrix<-function(x=matrix()){

  inv<-NULL
 
 set<-function(y){

    x<<-y
  
  inv<<-NULL
 
 }

 get<-function(){x}

setInverse<-function(inverse){inv<<-inverse}

getInverse<-function(){inv}

list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)

}


## Write a short comment describing this function
##cacheSolve() computes the inverse fom the matrix returned 
##by makeCacheMatrix() or retrieves the inverse from the cache
##if already calculated
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv<-x$getInverse()
 
      if(!is.null(inv)){

       message("Getting cached data")
 
       return(inv)

   }

    mat<-x$get()
 
    inv<-solve(mat,...)
  
    x$setInverse(inv)
   
    inv
}
