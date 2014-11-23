## create a *square* matrix to caching the inverse of a matrix rather than compute it repeatedly.
## use the makeCacheMatrix and the caseSolve.
makeCacheMatrix <- function(x = matrix()) {
+   m<-NULL
+   set<-function(y){
+   x<<-y
+   m<<-NULL
+ }
+ get<-function() x
+ setmatrix<-function(mean) m<<- mean
+ getmatrix<-function() m
+ list(set=set, get=get,
+    setmatrix=setmatrix,
+    getmatrix=getmatrix)
+ };

## create the matrix during the call of makeCacheMatrix().
> makeCacheMatrix();
$set
function (y) 
{
    x <<- y
    m <<- NULL
}
<environment: 0x7f8d7ccfdb10>

$get
function () 
x
<environment: 0x7f8d7ccfdb10>

$setmatrix
function (mean) 
m <<- mean
<environment: 0x7f8d7ccfdb10>

$getmatrix
function () 
m
<environment: 0x7f8d7ccfdb10>


a <- makeCacheMatrix( matrix(c(3,7,13,19), nrow = 2, ncol = 2) )
summary(a);
#>              Length Class  Mode    
#> setMatrix    1      -none- function
#> getMatrix    1      -none- function
#> cacheInverse 1      -none- function
#> getInverse   1      -none- function

## computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
> cacheSolve <- function(x=matrix(), ...) {
+     m<-x$getmatrix()
+     if(!is.null(m)){
+       message("getting cached data")
+       return(m)
+     }
+     matrix<-x$get()
+     m<-solve(matrix, ...)
+     x$setmatrix(m)
+     m
+ };
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
cacheMean(a);
           [,1]        [,2]
[1,] -0.5588235  0.38235294
[2,]  0.2058824 -0.08823529
> inverse
