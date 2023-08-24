## Put comments here that give an overall description of what your
## functions do
# as request, user could use function cacheSolve to get a inverse value from a given materix.
# cacheSolve check whether the given matrix already has value stored, if yes, give result without caculeted by solve()
# if no, cacheSolve will caculated the inverse result and store the give matrix and related inverse value, so that next time avoid caculeted by solve()
# function makeCacheMatrix need be instantiated before useing, just like: fun<-makeCacheMatrix()
# using steps as below:
# 1:source("cachematrix.R")
# 2:fun<-makeCacheMatrix()
# 3:cacheSolve(fun,matrix(4:7,2))
# user could change the matrix input of step 3, makeCacheMatrix also provide function for verify whether the given function is Singular matrix. 

## Write a short comment describing function makeCacheMatrix
# temp for temp store matrix last time used, user could use fun$get() to get the matrix 
# tempInver is similar with temp, user could use fun$getinver() to get the inverse matrix 
# list inver for store inverse matrix and y for store given matrix, which will be check by sapply with identical. 

makeCacheMatrix <- function(x = matrix()) {
  temp<-NULL                                # template stroe matrix, if the matirx meet spec (is not singular matrix)
  tempInver<-NULL                           # template inverse matrix
  inver <- list()                           # inverse matrix list 
  y<-list()                                 # matrix list  
  
  set <- function(x) {
    z<-sapply(y,function(f) identical(f,x)) # search matrix in list y, if existed, no new value added in the list
    r<-which(z==TRUE)                       # get the order number in list y
    if(length(r)==0)                        # if no same matrix existed, add to list y
    {
      cls<- class(try(Res<-solve(x),silent = TRUE)) #Get class of x inverse and if x inverse existed, will store it in Res       
      if(cls[1]=="matrix")
      {
        N<-length(y)+1                               #Get the list length mark as id
        y[[N]] <<- x
        inver[[N]] <<- Res
        tempInver <<- Res
        temp<<-x
        message("Matrix is stored!")         
      }
      else {
        message("Singular matrix!")           
      }
    }
    else
    {
      temp<<-y[[r]]
      tempInver<<-inver[[r]]
      message(paste0("Same matrix exsited, order is ",r,"!"))
    }
  }
  get <- function() temp
  
  setinver <- function(x) {
    
    if(length(y)>0)                            # ify is empty directly caculated inverse value 
    {
      z<-sapply(y,function(f) identical(f,x))  # search matrix in list y, if existed, no new value added in the list
      r<-which(z==TRUE)                        # get the order number in list y
      if(length(r)==0)                         # if no same matrix existed, add to list y
      {
        cls<- class(try(Res<-solve(x),silent = TRUE))        #Get class of x inverse and if x inverse existed, will store it in Res    
        if(cls[1]=="matrix")
        {
          N<-length(inver)+1   
          y[[N]] <<- x
          tempInver <<- Res
          inver[[N]] <<- Res
        }
        else
        {
          tempInver<<-NULL
          message("Singular matrix!") 
        }
      }
      else
      {
        temp<<-y[[r]]
        tempInver<<-inver[[r]]
      }
    }
    else
    {
      cls<- class(try(Res<-solve(x),silent = TRUE))        #Get class of x inverse and if x inverse existed, will store it in Res    
      if(cls[1]=="matrix")
      {
        N<-length(inver)+1   
        y[[N]] <<- x
        tempInver <<- Res
        inver[[N]] <<- Res
      }
      else
      {
        tempInver<<-NULL
        message("Singular matrix!") 
      }
    }
    tempInver
  }
  
  getinver <- function() {                        # Get last stored inver value for no matrix as parameters
    if(!is.null(temp))
    {
      z<-sapply(y,function(f) identical(f,temp))  # search matrix in list y
      r<-which(z==TRUE)                           # get the order number in list y
      if(length(r)!=0)                            # if matrix existed
      {
        tempInver<<-inver[[r]]                    # get inverse matrix from list inver
      }
      else
      {
        tempInver<<-NULL;
      }
    }
    else
    {
      message("Please set matrix fristly!") 
      tempInver<<-NULL;
    }
    
    tempInver
  }
  
  getinver <- function(x) {                       # Get stored inver value of matrix x
    if(length(y)>0){
      z<-sapply(y,function(f) identical(f,x))       # search matrix in list y
      r<-which(z==TRUE)                             
      if(length(r)==0)                              # if no,return NULL
      {
        tempInver<<-NULL
      } 
      else                                          # if yes, return stored value
      {
        temp<<-y[[r]]
        tempInver<<-inver[[r]]
      }
    }
    else
    {
      tempInver<<-NULL
    }
    tempInver
  }
  
  
  list(set = set, get = get,
       setinver = setinver,
       getinver = getinver)
}


## Write a short comment describing this function  makeCacheMatrix
# cacheSolve have two parameters,frist is instantiation of real functions, second is matrix for looking for inverse. 
# the function will check whether given matrix has value store, if no will caculate one, if yes will get the inverse from store reuslt list directly


cacheSolve <- function(f, x = matrix()) {
  ## Return a matrix that is the inverse of 'x'
  message("Given matrix is:")
  print(x)
  m <- f$getinver(x)
  if(!is.null(m)) {
    message("getting cached data~")
    print(m)
    return()
  }
  else
  {
    m <- f$setinver(x)
    if(!is.null(m))
    {
      message("caculating inverse~")
      print(m)
    }
  }
}
