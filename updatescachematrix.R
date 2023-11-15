
## Write a short comment describing this function
##there are two functions makeCacheMatix, makeCacheMatrix
##makeCacheMatrix consitsts of set, get, setin, getinv
##libary(MASS) is used to calculate inverse for non squared as well as square matrices

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
          x<<-y
          inv<<-NULL
  }
  
  get<-function()x             #function to get matrix x
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){
                    inver<-ginv(x)
                    inver%*%x                #funtion to obtain invse of the matrix
                    }
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...)   ##gets cache data
        ## Return a matrix that is the inverse of 'x'
  { 
  inv<-x$getinv()
  if(!is.null(inv)){              #checking whether inverse is null
                message("getting cached data!")
                return(inv)         #returns inverse value
    
  }
  data<-x$get()
  inv<-solve(data,...)        #calculates inverse value
  x$setinv(inv)
  inv         ##returns a matirix that is the inverse of 'x'
}
