makeCacheMatrix <- function(x = matrix()) {
+     m<-NULL
+     set<-function(y){
+         x<<-y
+         m<<-NULL
+     }
+     get<-function() x
+     setmatrix<-function(solve) m<<- solve
+     getmatrix<-function() m
+     list(set=set, get=get,
+          setmatrix=setmatrix,
+          getmatrix=getmatrix)
+ }
> 
> cacheSolve <- function(x=matrix(), ...) {
+     m<-x$getmatrix()
+     if(!is.null(m)){
+         message("getting cached data")
+         return(m)
+     }
+     matrix<-x$get()
+     m<-solve(matrix, ...)
+     x$setmatrix(m)
+     m
+ }
> a<-makeCacheMatrix()
> a$set(matrix(2:5,2,2))
> cacheSolve(a)
     [,1] [,2]
[1,]  -2.5  2
[2,]   1.5 -1
