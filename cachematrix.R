## The function below creates a special matrix that can be utilized to cache its respective inverse

## The function sets the value of the matrix and its inverse and also get the value of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        invs<-NULL
     set<-function(y)  {
             x<<-y
             invs<<-NULL
             }
      get<-function()x
      setinverse<-function(inverse) invs<<-inverse
      getinverse<-function() invs
        list(set=set,
             get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}


## Computing the inverse of the special matrix

cacheSolve <- function(x, ...) {
        invs<-x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
                }
        data<-x$get()
        invs<-solve(data,...)
        x$setinverse(invs)
        invs
}
