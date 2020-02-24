
## makeCacheMatrix function:Firstly set value of the matrix.
##Secondly get value of the matrix then set the value of the inverse.
##Finally get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){  
    x<<-y
    m<<-NULL  }
  get<-function() x
  setinverse <-function(inverse) m <<-inverse
  getinverse <-function() m
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse) 

}

## cacheSolve function take inverse of the matrix.

cacheSolve <- function(x, ...) {
        m<-x$getinverse()
        if(!is.null(m)){message("getting cached data")
        return(m)}
        data<-x$get()
        m<-solve(data,... )  
        x$setinverse(m)
        m
}

