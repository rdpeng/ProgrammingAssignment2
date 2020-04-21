## Put comments here that give an overall description of what your
## functions do
## Week 3 Assignment on April 21, 2020 by addcheco
##Solving the inverse of inserted matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #Creates matrix that can cache its inverse
          inv <- NULL #inv starts as NULL, hold matrix inverse
          set <- function(y){ #assign new function for set
            x <<- y #value placed into x will fill in y 
            inv <<- NULL #set inv to NULL if there is a new matrix
          }
          get <- function() x #define the get function as the value of matrix argument 
          setinverse <- function(inverse) inv <<- inverse #assign value of inv in parent environment
          getinverse <- function() inv #gets inv value 
          list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) #creates a list of these function
          #important for the functions in the next operator using $
}


## Write a short comment describing this function
#This will give the inverse of the matrix cached above


cacheSolve <- function(x, ...) {
        inv <- x$getinverse() 
        if(!is.null(inv)){ #provides if loop that if inv is not null, cached data wil be obtained
          message("getting cached data")
          return(inv)
        }
        data <- x$get() #data will be obtained from the matrix from above function 
        inv <- solve(data, ...) #important for doing the inverse of the data defined above
        x$setinverse(inv) 
        
        ## Return a matrix that is the inverse of 'x'
        inv 
}

