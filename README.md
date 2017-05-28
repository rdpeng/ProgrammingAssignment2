### Introduction

This second programming assignment will cache potentially time-consuming 
computations. In this Programming Assignment I will take advantage 
of the scoping rules of the R language and how they can be manipulated 
to preserve state inside of an R object.

### makeCacheMatrix

The first function, `makeCacheMatrix` creates a special "matrix", which is
really a list containing a function to

1.  set the value of the matrix
2.  get the value of the matrix
3.  set the value of the inverse
4.  get the value of the inverse

<!-- -->

    makeCacheMatrix <- function(x = matrix()) {
      i<- NULL
      set <- function(y) {
       x <<- y
       i <<- NULL
      }
      get <- function() x
      setinv <- function(solve) i <<- solve
      getinv <- function() i
      list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
      }

The following function calculates the inverse of the special "matrix"
created with the above function. However, it first checks to see if the
inverse has already been calculated. If so, it `get`s the inverse from the
cache and skips the computation. Otherwise, it calculates the inverse of
the data and sets the value of the inverse in the cache via the `setinv`
function.

    cacheSolve <- function(x, ...) {
      i <- x$getinv()
      if(!is.null(i)) {
        message("getting cached inverse")
        return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinv(i)
      i
        
    }

### Practical example

Creation of a makeCacheMatrix object

    test<-makeCacheMatrix(matrix(1:4,2,2)) 

Since the object was just created, this first execution of cacheSolve() 
will compute the inverse of "test" and return it.

    cacheSolve(test)

In a second call of cacheSolve() over "test", the function will not 
calculate again the inverse, instead of the function will retrieve 
the inverse within "test" object.

    cacheSolve(test)
