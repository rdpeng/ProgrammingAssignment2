makeVector<-function(x=numeric()){
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmean<-function(mean) m<<-mean()
  getmean<-function(mean) m
  list(set=set,get=get,
       setmean=setmean,
       getmean=getmean)
}
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getmean()
  if(!is.null(m)){
    message("getting cached data")
    return (m)
  }
  data<-x$get()
  inv<-mean(data,....)
  x$setmean(m)
  m
  
}