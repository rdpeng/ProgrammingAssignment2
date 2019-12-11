##Assignment for R-Programming course of Coursera
## The function:Makecachematrix will create and store matrix

makeCacheMatrix <- function(x = matrix()) {
     
  x<<-matrix(rnorm(4),2,2)
  x  
}
## Thefunction:CacheSolve will return the Inverse matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
             y<<-solve(x)
             y   
}
#Mutiplyig the matrics to check if i get identity matrix.
z<-x%*%y
round(z)
