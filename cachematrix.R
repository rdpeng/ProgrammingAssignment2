R version 3.1.1 (2014-07-10) -- "Sock it to Me"
Copyright (C) 2014 The R Foundation for Statistical Computing
Platform: i386-w64-mingw32/i386 (32-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

[Workspace loaded from ~/.RData]

> ## Put comments here that give an overall description of what your
> ## functions do
> 
> ## This function is used to create a special matrix object that can cache its inverse.
> ## 1st I assign a blank matrix defined by the variable x.
> 
> makeCacheMatrix <- function(x = matrix()) {
+   m <- NULL
+   set <- function(y){
+     x <<- y
+     m <<- NULL
+   }
+   get <- function() x
+   setmatrix <- function(matrix) m<<- matrix
+   getmatrix <- function() m
+   list(set=set, get=get,
+        setmatrix=setmatrix,
+        getmatrix=getmatrix)
+ }
> 
> ## Here I wrote a function to solve for the inverse of my matrix x. And asked it to return the answer
> ## by getting it from the cached data if it exists or solving for the inverse of the matrix if not.
> 
> cacheSolve <- function(x=matrix(), ...) {
+   m <- x$getmatrix()
+   if(!is.null(m)) {
+     message("getting cached data")
+     return(m)
+   }
+   matrix <- x$get()
+   m <- solve(matrix, ...)
+   x $setmatrix(m)
+   m
+ }
> ## 1st I will store the function for the matrix in a variable a and set the matrix to be a 2 by 2 matrix
> ## from 1 to 4. Finally I will run the cacheSolve function I wrote above to return the inverse.
> a <- makeCacheMatrix()
> a $set(matrix(1:4, 2, 2))
> cacheSolve(a0)
Error in cacheSolve(a0) : object 'a0' not found
> cacheSolve(a)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> ## I had a typo the first time I tried to solve it...the last time it worked
> save.image("~/ProgrammingAssignment2/cachematrix.R.RData")
> 
