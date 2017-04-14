# Lexical Scoping Example
#Week 3 Solution
# Ratchford

makeCacheMatrix <- function( x= numeric()){
  s<- NULL
  set<-function(y){
    x<<- y
    s<<- NULL
  }
  get<-function() x
  setSolve<-function(val) s<<-val
  getSolve<-function() s
  list(set=set,get=get,setSolve=setSolve, getSolve=getSolve)
}

cacheSolve <- function(x, ...){
  s<- x$getSolve()
  if(!is.null(s))
  {
    message('Get cached matrix inverse')
    return (s)
  }
  data<-x$get()
  s<-solve(data,...)
  x$setSolve(s)
  s
  
}