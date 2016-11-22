makeCacheMatrix<- function(x= matrix()){
       
         inv<-NULL
        
        set<- function(y) {
                x<<- y # storred in parrent env (i.e, makeCacheMatrix env)
                inv<<-NULL # storred in parrent env (i.e, makeCacheMatrix env)
        }
    #Although the set function is not used to cached data, this setter is used for 
        #importing data. E.g, my_matrix<-makeCacheMatrix(x) : where x a 3x3 matrix
        #                     i can feed my_matrix a new matrix by using the set 
        #                     function. So my_matrix$set(x-1) assignes my_matrix with 
        #                     a new matrix x-1. 
        
        
        
        get<- function() x # gets the x from the parent environment of makeVector().
        setinv<- function(solve) inv<<- solve
        getinv<- function() inv #  again using lexical scoping to find the correct symbol inv to retrieve its value (from the parrent env).
        list(set = set, get = get,
             setinv = setinv, getinv =getinv)
        
}

CacheSolve<- function(x, ...){
        inv<-x$getinv()# input argument form makeCacheMatrix
        if(!is.null(inv)) { 
                message("getting Cached data")
                return(inv)}
       matrix<- x$get() # matrix gets the content of x 
       inv<- solve(matrix)# inversion calculation by the solve function
       x$setinv(inv)
       
       # with this piece of code, i.e. x$setinv(inv), I set the just computed 
       # inv, so inv!= NULL. This is why when we call CacheSolve again the 
       # inv is gotten by the cached data! 
       
       inv
}
