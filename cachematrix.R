## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

        makeCacheMatrix<-function(x=matrix()){
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setInv<-function(inv){ m<<-inv}
  getInv<-function()m
  list(set=set,get=get,setInverse=setInv,getInverse=getInv)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getInverse()
  if(!is.null(m)){
    message("Getting the cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setInverse(m)
  m
}
}
# sample run
# m1<-makeCacheMatrix(rbind(c(1,2),c(4,6)))
#cacheSolve(m1)
#     [,1] [,2]
#[1,]   -3  1.0
#[2,]    2 -0.5
# m1$get()
 #    [,1] [,2]
#[1,]    1    2
#[2,]    4    6
# cacheSolve(m1)
#Getting the cached data
#     [,1] [,2]
#[1,]   -3  1.0
#[2,]    2 -0.5
