## The first function, makeCacheMatrix, takes a square matrix as its argument.
## It then creates a list consisting of four functions that are called by the
## function chacheSolve.  chacheSove attempts to find the inverse of the matrix
## but first checks to see if its inverse has already been calculated.  If the
## inverse has already been calculated then it retrieves the cached inverse 
## matrix.

## The function makeCacheMatirx takes a square matrix as its argument. It sets
## its inverse to Null and creates a list of four functions to 1) cache the 
## matrix 2) get the cached matrix 3) cache the inverse of the matrix and 
## 4) get the cached inverse matrix 

makeCacheMatrix <- function(mat = matrix()) {
        inv<-NULL #Set the inverse matrix to NULL
        
        ## Define functions to be used in cacheSolve ##
        
        cacheMat<-function(y) {
               mat<<-y  #create a matrix, y, in the parent environment identical 
                        #to "mat" 
               inv<<-NULL #set the inverse of the matrix to Null in the parent 
                          #environment
        }
        
        ## Create a function to get value of "mat" chached in the parent 
        ## environment
        
        getMat<-function() {mat}
        
        #Create a function to Cache the inverse of the matrix once it is found 
        cacheInv<-function(inverse) {
                inv<<-inverse
        }
        
        #Create a function to retrieve the inverse of the matrix from the cache
        getInv<-function() {inv}
        
        #Generate the list of the four functions
        #cacheSolve will then call these functions
        list(cacheMat = cacheMat, getMat = getMat,
             cacheInv = cacheInv,
             getInv = getInv)
        
}

## The function cacheSolve takes the list created by makeCacheMatrix as its
## argument.  It checks to see if the inverse of the matrix that was input as 
## an argument to makeCacheMatrix has been calculated. If so, it retrieves it 
## from the cache.  If not, it calculates it and stores it in the cache. 

cacheSolve <- function(x, ...) {
       
        ## Return a matrix that is the inverse of 'mat'
        
        inv <- x$getInv() #Returns the value of "inv", the matrix inverse, from 
                          #the parent environment
        
        ## If the inverse has already been solved, and its value is stored in
        ## the parent environment display the message "getting cached data"
        ## and return the cached inverse matrix.
        
        if(!is.null(inv)) {
                message("getting cached data") 
                return(inv) #Cached inverse of matrix is returned so the 
                            #following four commands are not executed.
        }
        data <- x$getMat() #If inv has not been solved get the matrix and
                           #assign it to "data"
        
        inv <- solve(data, ...) #Find the inverse of the matrix and assign
                                #it to "inv"
        
        x$cacheInv(inv) #Cache the inverse matrix in the parent environment
        inv             #Display the inverse of the matrix in the console
}
