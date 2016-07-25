#This function computes the inverse of the special matrix 
returned by makeCacheMatrix 

CacheSolve <- function(x,...){
   inv<-x$get_inverse() 
   if(!is.null(inv)){ 						# checks to see if inverse has already been calculated
                    message ("getting cached data")
                    return (inv) 				# if inverse has been calculated return it and exit
                    }
   data<-x$get()
   m<-solve(data,...) # calculate inverse if it has not been calculated
   x$set_inverse(inv) # sets the value of inverse in the cache
   inv
}