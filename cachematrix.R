## overall description of what these functions do :- 
##  the function makeCacheMatrix() , creates a special matrix
##   kind of object that caches it's own inverse
## the function cachesolve() this function computes the inverse of the special matrix returned by 
## makeCacheMatrix()

## This function  creates a matrix kind of object that caches it's own inverse

makeCacheMatrix <- function(x = matrix()) {
          
            invse_mat<- NULL
            set <- function(y){
              x<<-y
              invse_mat<<- NULL
            }
            get <-function() x
            set_invse<-function(invse) invse_mat <<- invse
            get_invse<-function()invse_mat
            list(set=set,get=get,set_inverse=set_invse,get_inverse=get_invse)
}


## If the inverse of the matric is already stored in Cache , then that value is returned
## else, inverse is computed and then returned

cacheSolve <- function(x, ...) {
  invse_mat<-x$get_inverse()
  
  ## message to be printed to know what actually happened in the function 
  
  msg<-"Inverse is not present in the cache and hence it is computed now"
  
  ## condition to check if the mean is already calculated and a nested condition to  
  ## check if the matrix is not changed adn this is achieved by checking for equality of the passed value of inverse
  ## and inverse of matrix  returned by the "get()" function of the object passed as argument 
 
 if(!is.null(invse_mat) ) 
 {                                                              
    if((identical(invse_mat,solve(x$get()))))
    {
        message("As mean is already calculated and matrix is not changed, getting the cahched data")                      
              return(invse_mat)
    }
    ## changing the message when the above if condition has failed
    else
    {
      msg<-"Inverse is calculated but Matrix is changed , now inverse is computed again for the new matrix."
    }
 }
  
## displaying the appropriate message
  message(msg)

#computaing the inverse of the matrix
  mathrix<-x$get()
  invse_mat<-solve(mathrix,...)
  x$set_inverse(invse_mat)
  invse_mat
}
