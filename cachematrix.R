## Below are two functions that are used to create a special 
#object that stores a numeric matrix and caches its inverse.



##The first function, makeCacheMAtrix() creates a special "vector", 
##which is really a list containing a function to
#set the value of the matrix       is    set() function
#get the value of the matrix       is    get() function  
#set the value of the inverse      is    setinverse() function
#get the value of the inverse      is    getinverse() function


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




###solve inverse of martix using solve() of matrix created by above function
#It first check if inverse already calculated in cache data of the matrix and skip
#Else it calclate inverse and cache it by setinverse() function

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