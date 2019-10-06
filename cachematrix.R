
## makeCacheMatrix: This function creates a special "matrix" object 
#that can cache its inverse

makeCacheMatrix<- function(x=matrix()){
 s<-NULL
 
 set<- function(y) x<<-y;s<-NULL 
 get<- function() x
 setinverse<- function(solve) s<<- solve
 getinverse<- function() s
 list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. If the inverse has already been calculated
#(and the matrix has not changed), then the cachesolve should retrieve the 
#inverse from the cache 

cacheSolve<-function(x, ...){
 s<-x$getinverse()
 if(!is.null(s)) return(s)
 data<- x$get()
 s<-solve(data, ...)
 x$setinverse(s) 
s
}

