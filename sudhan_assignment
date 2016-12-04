## Put comments here that give an overall description of what your
## functions do

##makeCacheMatrix function - creates a matrix and stores the cache value of matrix
##makeCacheMatrix function- takes input as matrix and returns a list
##cacheSolve function - takes input as a list, usually from makeCacheMatrix
##inversematrix() - takes a matrix and finds out the inverse by Gauss Jordan Method

## Write a short comment describing this function

source("matrixinverse.R")
makeCacheMatrix <- function(x = matrix()) {
 inv = NULL
 
##sets the value of matrix 
 set <- function(y)
 {
   x <<- y
   inv <<- NULL
 }
 
 ##gets the value of the matrix x
 get <- function() x
 
 #assigns inverse variable to inv so that it can be cached later
 setinverse <- function (inverse) inv <<- inverse
 
 #gets the inverse of the matris, when called form the cacheSolve function
 getinverse <- function () inv

list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv))
  {
    message("Getting Cached Data")
    return(inv)
  }
  mat <- x$get()
  inv <- inversematrix(mat)
  ## inversematrix() function generates the inverse of the data
  ##look into subfunctions 
  ##checkmatrix() - function that checks if inverse exists for a matrix and 
  ## if the matrix is a square matrix
  ## augmentedmatrix() - function whihc creats augmented matrix of the entered
  ## matrix and identity matrix
  ## inversemat() - function that performs ERO and converts augmented matrix into
  ## inverse
  x$setinverse(inv)
  inv
}

mat1 <- matrix(1:9, 3, 3)
abc <- makeCacheMatrix(mat1)

cacheSolve(abc)




##Code for matrixinverse.R, which has been sourced


inversematrix <- function(x = matrix())
{
  value = TRUE
  inv <- checkmatrix(x)
  n <- nrow(inv)
  print(inv[,(n+1):(n+n)])
  for(i in 1:n)
  {
    for(j in 1:(n+n))
    {
      if (is.na(inv[i,j]))
      {
        value = FALSE
        break
      }
    }
  }
  
  if(value == FALSE)
    
    print ("Matrix doesn't have an Inverse")
  
}

checkmatrix <- function(abc)
{
  
  n <- nrow(abc)
  n1 <- n*n
  n2 <- n*n*2
  check1 <- is.matrix(abc)
  if(!check1)
  {
    print ("Input a matrix")
  }
  else
  {
    n <- nrow(abc)
    m <- ncol(abc)
    if(n != m)
    {
      print("Matrix is not square. Inverse doesn't exist")
    }
    else
    {
      print("Matrix is square. Checking for Inverse")
      augmat <- augmentedmatrix(abc)
      invmat <- inversemat(augmat)
      return(invmat)
    }
  }
  
}




augmentedmatrix <- function(x)
{
  
  n <- nrow(x)
  n1 <- n*n
  n2 <- n*n*2
  idmatrix<-  matrix(1:n1,n,n)
  augmat <-matrix(1:n2,n)
  n <- nrow(x)
  for(i in 1:n)
  {
    for (j in 1:n)
    {
      if(i == j)
        idmatrix[i,j] <- 1
      else
        idmatrix[i,j] <- 0
    }
  }
  
  for (i in 1:n)
  {
    for (j in 1:n)
    { 
      augmat[i,j] <- x[i,j]
    }
  }
  
  
  for (k in 1:n)
  {
    for (l in (n+1):(n+n))
    {
      augmat[k,l] <- idmatrix[k,(l-n)]
    }
  }
  augmat
}



inversemat <- function(x)
{
  n <- nrow(x)
  for(i in 1:n)
  {
    x[i,] <- x[i,]/x[i,i]
    for(k in 1:n)
    {
      if(i != k)
      {
        x[k,] <- x[k,] -(x[k,i]*x[i,])
      }
    }
  }
  x
  
}


