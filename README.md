

makeCacheMatrix = function(x = matrix()){
  inv = NULL
  set = function(y){
    x <<- y
    inv <<- NULL
  }
    get = function() {x}
    setInverse = function(inverse){inv <<- inverse}
    getInverse = function() {inv}
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
cachesolve = function(x, ...){
  inv = x$getInverse()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat = x$get()
  inv = solve(mat, ...)
  x$setInverse(inv)
  inv
}
setwd("C:\\Users\\georg\\Documents\\Curso R_Coursera\\R programming\\specdata\\specdata")
pmatrix = makeCacheMatrix(matrix(1:16, nrow = 4, ncol = 4))
> pmatrix$get
function() {x}
<environment: 0x000002077494fcc8>
> pmatrix$get()
     [,1] [,2] [,3] [,4]
[1,]    1    5    9   13
[2,]    2    6   10   14
[3,]    3    7   11   15
[4,]    4    8   12   16
> pmatrix$getInverse()
NULL
> cachesolve(pmatrix)
 Error in solve.default(mat, ...) : 
  Lapack routine dgesv: system is exactly singular: U[3,3] = 0 
> pmatrix = makeCacheMatrix(matrix(1:16, nrow = 4, ncol = 4))
> pmatrix = makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
> pmatrix$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4
> pmatrix$getInverse()
NULL
> cachesolve(pmatrix)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> pmatrix$getInverse()
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> 
