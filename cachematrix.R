## The following two functions are used hand in hand and is 
## intended to create and set a matrix and also to find and set the 
## inverse of the given invertible matrix.

## This function returns a list of functions to the variable it is assigned to.
## The set() function sets a matrix passed to the function.
## The get() function retrives and prints the matrix.
## The setinverse() function, called by the second function, sets the inverse of the matrix.
## The getinverse() funtion prints the inverse of the matrix if available, else it prints NULL.

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  
  set<-function(y){ ## matrix assigned
    x<<-y
    i<<-NULL  ##default inverse value of new matrix until reassigned.
  }
  
  get<-function() x ## matrix printed
  setinverse<-function(inverse) i<<-inverse ## inverse assigned
  getinverse<-function() i ## inverse printed
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  ## list of functions returned
}


## This funtion will only calculate the inverse of the matrix passed if it hasn't 
## already been calculated. Else it will simply return the already calculated inverse.
## (We assume the matrix passed is invertible)

cacheSolve <- function(x, ...) {
  i<-x$getinverse() ## retrives the inverse from the previous function.
  
  if(!is.null(i)){ ## checks if inverse already exists
    message("getting cached data")
    return(i) ## exits function after printing if 'i' is not NULL 
  }
  
  data<-x$get() ## retrives the matrix otherwise
  i<-solve(data,...) ## finds inverse
  x$setinverse(i) ## assigns inverse by calling setinverse()
  i ## printing the inverse
}
