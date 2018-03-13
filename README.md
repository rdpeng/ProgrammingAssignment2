### Introduction

This is a submission for second assignment of R Programming course of Coursera by John Hopskin Unv, of the week 3 realated to cache a data using the scoping rule of R and the <<- operator where a data simulated can be reused/cached and not require to computed again. This assignment demonstrate the use of SET and GET function creation of R which is a handy tool for OOP of R and to use variable as global variable in environment different from original environment.  

### Example: Caching the Inverse of a Matrix

In this example we introduce the `<<-` operator which can be used to
assign a value to an object in an environment that is different from the
current environment. Below are two functions that are used to create a
special object that stores a numeric matrix and caches its Inverse.

The first function, `makeCacheMatrix` creates a special "Matrix", which is
really a list containing a function to

1.  set the value of the matrix
2.  get the value of the matrix
3.  set the value of the inverse
4.  get the value of the inverse

<!-- -->

   makeCacheMatrix <- function(x = matrix()) {
      #inverseMat=Inverse of matrix
      inverseMat <- NULL           #Inverse initialize to NULL
      set <- function(y) {
            
            ##Set the matrix from input data and and set inverse to NULL value
            
            x <<- y
            inverseMat <<- NULL
      }
      get <- function() x  #Get the value of matrix
      
      setinverse <- function(inv) inverseMat <<- inv   #Set inverse from 
      #simulation data
      getinverse <- function() inverseMat   #Get inverse of matrix 
      #from available data
      list(set = set, get = get,
           setinverse = setinverse,     #Create a list of matrix
           getinverse = getinverse)
}



The following function calculates the inverse of the special "matrix"
created with the above function. However, it first checks to see if the
inverse has already been calculated. If so, it `get`s the inverse from the
cache and skips the computation. Otherwise, it calculates the inverse of
the data and sets the value of the inverse in the cache via the `setinverse`
function.

    cacheSolve <- function(x, ...) {
      inverseMat <- x$getinverse()     #get the InverseMat=inverse of matrix     
      if(!is.null(inverseMat)) {
            message("getting cached data")  #cached
            return(inverseMat)
      }
      
      data <- x$get()                         #get the matrix as input
      inverseMat <- solve(data, ...)          #simulate inverse using solve()
      x$setinverse(inverseMat)                #set the inverse value
      inverseMat
      
      
}