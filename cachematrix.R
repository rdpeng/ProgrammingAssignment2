## Put comments here that give an overall description of what your
## functions do
 
 #1.makeCacheMatrix helps in storing a matrix and it's computed inverse in the cache. 
 #2.cacheSolve checks if any value of inverse is there in the cache and if yes then it displays it's value . Otherwise, it computes the
   #inverse of the matrix stored in the cache and stores the computed value in the inv variable present inside the set_inverse function.
 

## Write a short comment describing this function

##makeCacheMatrix takes a matrix as an argument. The get_matrix function displays the content of the matrix passed as an argument.
 #set_matrix helps in overwriting the existing values of the matrix with new values. set_inverse takes the value of inverse as an 
 #argument. This value of inverse is the value stored in cache . The <<- operator is used to assign a value to an object 
 #in an environment that is different from it's current environment. This is done so that changes made in variables x and inv are saved,
 #since now they are assigned in an environment enclosing the environment in which they are defined. 
 #get_inverse gives the value of the inverse of the matrix.
 
makeCacheMatrix <- function(x = matrix()) {


        inv<-NULL
        
        set_matrix<-function(y)
        {
                x<<-y
                inv<<-NULL
        }
        
        get_matrix<-function(){ x }
        
        set_inverse<-function(inverse)
        {
                inv<<-inverse
        }
        
        get_inverse<-function(){  inv }
        
        list(set_matrix=set_matrix, get_matrix=get_matrix, set_inverse=set_inverse, get_inverse=get_inverse)

}


## Write a short comment describing this function


##cacheSolve is a function that basically assigns the inverse value of the matrix inputted by the user of the programme.It first checks
 #if a computed value of inverse has been inputted by the user or not. If it has been inputted, then it displays the value of the inverse
 #and the programme ends there.This is done to prevent recomputing of the already computed inverse value. If no value of inverse has been 
 #inputted then it finds the inverse of the matrix present in the cache(this matrix is stored in cache using makeCacheMatrix function) 
 #and displays the result. The computed inverse is also assigned to inv variable present inside the set_inverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inv<-x$get_inverse()

        if(!is.null(inv))
        {
                message("getting value of inverse from cache:")
                
                return(inv)
                
        }
          
        
        message("No value found in cache")
        
        mat<-x$get_matrix()
        
        inv<-solve(mat,...)
        
        x$set_inverse(inv)
        
        inv
        
}
