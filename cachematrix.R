## Calculates the inverse of a matrix and stores it into cache. If it was already calculated takes it from memory.



## Creates an object, with setters and getters, in the form of a list. Because of closure (and the use of the <<-),
## it gives access to the variables from the parent environment.
##It is meant to store a matrix, with or without its inverse,in memory; to be used by cachesolve(). 

## Creates an object, with setters and getters, in the form of a list. Because of closure (and the use of the <<-),
## it gives access to the variables from the parent environment.
##It is meant to store a matrix and/or its inverse in memory; to be used by cachesolve(). 


makeCacheMatrix <- function(A = matrix()) {
                   Ainv <- NULL
                   SetMatrix <- function(B){
                     A <<- B
                     Ainv <<- NULL
                   }
                   GetMatrix <- function() A
                   SetInverse <- function(inverse) Ainv <<- inverse 
                   GetInverse <- function() Ainv
                list(SetMatrix = SetMatrix, GetMatrix = GetMatrix,SetInverse = SetInverse,GetInverse = GetInverse)   
}


## Takes an object of the form given by makeCacheMatrix(), takes the inverse from memory if it exists, 

## otherwise, it calculates it with solve(), e.i. it assumes the matrix is invertible.

## otherwise, it calculates it with inverse(), e.i. it assumes the matrix is invertible.


cacheSolve <- function(A, ...) {
        Ainv <- A$GetInverse()
        if (!is.null(Ainv)){
          message("getting cache inverse")
          return(Ainv)
        }
        
        AMatrix <- A$GetMatrix()
        Ainv <- solve(AMatrix,...)
        A$SetInverse(Ainv)
        Ainv
}

