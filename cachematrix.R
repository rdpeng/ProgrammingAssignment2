## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeVector<-function(x= numeric()){
  m<-NULL#把m赋值为null
  set<-function(y){
    x<<-y #set(y)后，x=y,m=null
    m<<-NULL
  }
  get <-function() x #把x赋值给get
  setmean<-function(mean) m<<-mean #这里求均值，只是依然存疑，为什么这里要用function(mean）。。或者在本函数里，不会做mean相关的操作，真正的solve实在cachemean里执行？
  getmean<-function() m #这里把m复制给getmean（NULL）
  list(set=set,get=get,
       setmean=setmean,
       getmean=getmean)
}
cachemean<-function(x, ...){
  m<-x$getmean() #首先把getmean赋值给m，如果m不是NULL的话弹出下列提示
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }#出现缓存值
  data<-x$get()#把最开始的numberic向量赋值给data
  m<-mean(data, ...) 
  x$setmean(m)缓存m
  m
}
