
makeCacheMatrix <- function(x = matrix()) {
        #assign NULL to m
        m <- NULL
        
        #create a function to keep global_x as passed matrix and global_m to NULL
        set <- function(y){
                global_x <<- y
                global_m <<- NULL
        }
        
        #function(). a matrix stored by set() is returned
        get <- function() return(global_x)
        
        #function(). a matrix is stored as global value
        set_global_m <- function(m) global_m <<- m
        
        #function(). a matrix stored by set_global_m() is returned
        get_global_m <- function() return(global_m)
        
        list(set=set,get=get, set_global_m=set_global_m, get_global_m=get_global_m)

}


cacheSolve <- function(x) {
        #get the value from the global environment
        m <- x$get_global_m()
        
        #if m is not null, then it return the value and print the message
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        
        #if m is NULL, the inverse matrix is computed by solve() function
        data <- x$get()
        inverseMatrix <- solve(data)
        x$set_global_m(inverseMatrix)
        return(inverseMatrix)
}
