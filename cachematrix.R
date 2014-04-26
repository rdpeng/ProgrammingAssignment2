## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function



##-------------------------FUNCTION makeCacheMatrix ---------------------------------------------------------##
## This function returns a list which contains four parameters with functionality to work with a matrix
##
## The input parameter is a matrix and the detailed content stored in the four parameters of the list will 
##be explained through the program.
##-----------------------------------------------------------------------------------------------------------## 


makeCacheMatrix <- function(x = matrix()) {

 ##This variable is created and initialized to NULL in the current environment
  m <- NULL
  
   

  ##-------------------------set FUNCTION--------------------------------------------------------------------##
  ##
  ##The main objectives of this function are:
  ##
  ##1.- To create and initialize a matrix named "x" with the values stored in entry parameter of the function.
  ##2.- To create and inicializate a variable "m" to NULL.  
  ##
  ##Be aware of the following facts:
  ##
  ##1.- Neither these variables are the same than the previous ones defined in the main function, 
  ##    although they have got the same.
  ##2.- Nor are stored in the main environment. The reason is that the "<<" operator locate these new variables 
  ##    and their values in an environment different than the current one.
  
  ##  NOTE: This function is not going to be executed in this practical example. 
  ##---------------------------------------------------------------------------------------------------------##
    
   set <- function(y) { 
     x <<- y
     m <<- NULL    
   }
  
     
  ##-------------------------get FUNCTION--------------------------------------------------------------------##
  ##
  ##The main objective of this function is:
  ##
  ##1.- Retrieve the value of the matrix "x" which is stored in the main environment.
  ##---------------------------------------------------------------------------------------------------------##
  get <- function() x 
  
    
  ##-------------------------TORAGE OF CALCULATED MATRIX INVERSE IN THE OTHER ENVIRONMENT----------------------##
  ##
  ## The main objetives of this funcion is as follows:
  ##
  ## 1.- The assignation of the already calculated inverse matrix stored in the parameter "solve" to the 
  ##    variable "m" which is located in "another" environment" different than the current one.
  ##
  ##---------------------------------------------------------------------------------------------------------##
    
  setmatrix_inv <- function(solve) m <<- solve
  
  ##Another way to write this sentense is as follows:
  ##
  ##setmatrix_inv <-function(solve)
  ##{
  ##  m <<- solve
  ##}
 
  
  
  ##-------------------------RETRIEVING THE CALCULATED MATRIX INVERSE FROM MEMORY----------------------------##
  ##
  ##The main objective of this sentence is:
  ##
  ##Storing in the variable "getmatrix_inv" the value of the calculated matrix inverse which is in the variable "m"
  ##
  ##Be aware of the following:
  ##The variable "getmatrix_inv" is located in the main environment, whereas "m" is located in the 
  ##another environment.
  ##
  ##NOTE: This matrix "m" will only exists if the calculation of the matrix inverse has been done before, 
  ##otherwise the returned value will be NULL
  ##---------------------------------------------------------------------------------------------------------##
    
  getmatrix_inv <- function() m
  
  
  ##-------------------------BUILDING A LIST WITH THE FUNCTIONS DEFINITION-----------------------------------##
  ##
  ## This is the list which will be returned with the execution of the main function "makeCacheMatrix"
  ## The returned list contains the four parameters with the functionality of the functions defined above.
  ## 
  ## The aim of this list is to ease the daily work of a programmer who has to work making matrix inverse repeatedly.
  ## Using this list, the programmer has the code to work with matrix and calculate its inverse
  ##already written and only has to execute the main funcion to access to each functionality.
  ##---------------------------------------------------------------------------------------------------------##
    
   list(set = set, get = get,
       setmatrix_inv = setmatrix_inv,
     getmatrix_inv = getmatrix_inv)
 

}


## Write a short comment describing this function



##-------------------------cacheSolve FUNCTION----------------------------------------------------------------##
##
## This funcion calculate and returns a matrix inverse. If the "original" matrix doesn't has an inverse the funcion
## returns an error.
##
## The procedure used to calculate is as follows:
##
## 1.- The input parameter is the list of functions described on top of the program (returned by function "makeCacheMatrix")
##
## 2.- If the inverse of the "original matrix" was already calculated, the function gets the inverse matrix which
##     is stored in another environment (in memory). So, no extra processes and memory will be necessary to
##     obtain the inverse matrix.
##
## 3.- If the inverse of the "original matrix" isn't already calculated, the function will calculate it for the
##     first time and will store it in two variables, one stored in the current environment and other stored
##     in another environment (the last one will be used in future executions of the function if it is requested the 
##     inverse matrix of the same "original matrix")
##
## 4.- Finally, the returned variable is the inverse matrix
##-----------------------------------------------------------------------------------------------------------##



cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
        
 
  ##This sentence use the List's function "getmatrix_inv" to get the inverse matrix which is located in the "other environment"
  ## and store it in the variable "m" in the current environment.
  
  m <- x$getmatrix_inv()
  
  
  ## On the one hand, if the inverse matrix in "m" is not null, it means that the inverse matrix of the "original matrix" were already
  ## calculated and it was stored in the "other environment".
  ## So, the returned parameter is the already calculated inverse matrix and also a message to alert the user that
  ## this inverse matrix was already cached.
  ## At this point, The function ends.
  
  
  if(!is.null(m)) {
    message("getting cached Matrix")
    return(m)
  }
  
  ## On the other hand, if the inverse matrix in "m" is null, it means that the inverse matrix isn't still calculated,
  ## so the "original matrix" has to be retrieved using the List's fuction "get" and be stored in the variable "data" in the
  ##current environment.
  
  data <- x$get()
  
 ## Now, the inverse matrix is calculated using the function "solve" and it stores the result in 
 ## the variable "m" in the current environment.
  
  m <- solve(data, ...)
  
  ##Once the inverse matrix is stored in "m", the List's function "setmatrix_inv" is called and 
  ##the inverse matrix is stored in the "other environment", so it could be used in the future.
  
  x$setmatrix_inv(m)

 ##The inverse matrix is returned. And at this point the function ends.
  m        
        
}
