makeCacheMatrix <- function(x=matrix()) {
        i<-Null
       set<-function(y) {
               x<<-y
               i<<-NULL
       }
       get<-function() x
       setinverse<-function(inverse)  i<<-inverse
        getinverse<-function() i
        list(set=set
             get=get
             setinverse=setinverse
             getinverse=get,
             setinverse=setinverse,
             getinverse=getinverse)
        }
cashesolve<-function(x,...) {
        i<-x$getinverse()
        if(!is.null(i)){
                        message("getting cashed data")
                        return(i)
         }
        data<-x$get()
        i<-solve(data,...)
        x$setinverse(i)
        }
