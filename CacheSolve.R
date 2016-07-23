CacheSolve <- function(x,...){
   inv<-x$get_inverse()
   if(!is.null(inv)){
                    message ("getting cached data")
                    return (inv)
                    }
   data<-x$get()
   m<-solve(data,...)
   x$set_inverse(inv)
   inv
}