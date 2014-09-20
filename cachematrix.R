#Function to calculate and cache the inverse of a matrix#

#Function that creates a list of functions that can be called to do the following : i) set the value of a matrix, 
#ii) return that value if available, iii) set the value of the inverse of the matrix, 
# and/or iv) return that value if available

makeCacheMatrix<-function(x=matrix()){
        i<<-NULL
  
        #function to set the value of the matrix x in the cache and reset to NULL previous inverse value (i) that might be there
        set <-function(y){
        x<<-y
        i<<-NULL
        } 
  
        #function to return the value of the matrix x from the cache
        get<-function() x
  
        #function to set the value of the inverse of the matrix x in the cache
        setinverse<-function(inverse) i<<-inverse
  
        #function to return the value of the inverse of matrix x from the cache
        getinverse<-function() i
  
        #create a list containing the functions defined above
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

#function to calculate the inverse of the matrix by either calculating anew if cache is empty or returning the
#cached value if its available
cacheSolve<-function(x,...){
  
        #return inverse of matrix x from the cache if its available   
        i<-x$getinverse()
        if(!is.null(i)){
         message("getting cached data")
        return(i)
        }
        
        #
        data<-x$get()
  i<-solve(data,...)
  x$setinverse(i)  ## Return a matrix that is the inverse of 'x'
  i
}
