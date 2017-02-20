
R version 3.3.2 (2016-10-31) -- "Sincere Pumpkin Patch"
Copyright (C) 2016 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

>  makeMatrix <- function(x = matrix()) {
+      MATINVE <- NULL
+      set <- function(y) {
+          x <<- y
+          MATINVE <<- NULL
+      }
+      get <- function() x
+      setinverse <- function(inverse) MATINVE <<- inverse
+      getinverse <- function() MATINVE 
+      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
+  }
>  
>  
>  
>  
>  
>  cachematrix <- function(x, ...) {
+      MATINVE <- x$getinverse()
+      if(!is.null(MATINVE )) {
+          message("getting cached matrix data.")
+          return(MATINVE )
+      }
+      data <- x$get()
+      MATINVE<- solve(data)
+      x$setinverse(MATINVE)
+      MATINVE 
+  }
> x<-rbind(c(1,2,3),c(0,1,5),c(5,6,0)).
Error: unexpected symbol in "x<-rbind(c(1,2,3),c(0,1,5),c(5,6,0))."
> x<-rbind(c(1,2,3),c(0,1,5),c(5,6,0))
> x
     [,1] [,2] [,3]
[1,]    1    2    3
[2,]    0    1    5
[3,]    5    6    0
> m=makeMatrix(x)
> m$get()
     [,1] [,2] [,3]
[1,]    1    2    3
[2,]    0    1    5
[3,]    5    6    0
> cachematrix(m)
     [,1] [,2] [,3]
[1,]   -6  3.6  1.4
[2,]    5 -3.0 -1.0
[3,]   -1  0.8  0.2
> cachematrix(m)
getting cached matrix data.
     [,1] [,2] [,3]
[1,]   -6  3.6  1.4
[2,]    5 -3.0 -1.0
[3,]   -1  0.8  0.2
> 
