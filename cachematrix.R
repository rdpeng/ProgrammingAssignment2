#######     Programming Assignment 2      #######
#######     By Siddharth Chauhan          #######
#######     Date: Dec 27 2015             #######
#################################################

#######     A pair of functions are created that
#######     will cache a given matrix, compute its
#######     inverse, and cache that inverse as well



#######     Code Begins Here

## Constructs a matrix object, which is a list of functions that:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix())
{
      #x is intsntiated as an emptry matrix object
      #inv is initialized to NULL
      inv <- NULL
      
      #This function will set the value of the matrix
      #and its inverse so it is cached for future use
      set <- function(y)
      {
            x <<- y
            inv <<- NULL
      }
      
      #This function get the cached value of the matrix
      get <- function() x
      
      #This function sets the inverse of the matrix
      #so it is cached for future use
      set_inv <- function(inverse) inv <<- inverse
      
      #This function get the cached value of the inverse
      get_inv <- function() inv
      
      #A list of the functions is created and returned
      list(set = set, get = get,
           set_inv = set_inv,
           get_inv = get_inv)
}


## Calculates the inverse of the matrix object created above.
## It first checks to see if the inverse has already been calculated,
## and if it has, it returns that cached value. Otherwise, it computes
## the inverse, caches the value for the inverse, and then returns the inverse

cacheSolve <- function(x_list, ...)
{
      #'inv' is initialized as the inverse of cached matrix 'x_list'
      inv <- x_list$get_inv()
      
      #If 'inv' is not NULL, return the cached value
      if (!is.null(inv))
      {
            message("getting cached data")
            return(inv)
      }
      
      #If 'inv' is NULL, compute the inverse and cache it
      data <- x_list$get()
      inv <- solve(data, ...)
      x_list$set_inv(inv)
      
      ## Return a matrix that is the inverse of 'x_list'
      inv
}
