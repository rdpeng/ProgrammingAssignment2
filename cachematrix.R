##This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
                    m <- NULL    ##initialize two objects, x&m
                    set <- function(y){    ##efine set() function 
                      x <<- y              #Assign the input argument to the x object in the parent evironment 
                      m <<- NULL           #Assign the value of NULL to the m object in the parent environment 
                    }
                    get <- function()x    ##define the getter for the matrix x
                    setinverse <- function(solve)m <<- solve  #define the setter for the inverse matrix m
                    getinverse <- function()m            #define the getter for the inverse matrix m
                    list(set = set, get = get,      #gives the name 'set'to the set() function, and 'get' to the get() function 
                         setinverse = setinverse,   #gives the name 'setinverse'to the setinverse() function
                         getinverse = getinverse)   #gives the name 'getinverse'to the getinverse() function

}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
              m <- x$getinverse()       #calls th getinverse() function on the input object 
              if(!is.null(m)){          #checks to see whether the result is NUL. 
                message("getting cashed data") #if(!is.null(m)) is true, return"getting cashed data"
                return(m)               #if(!is.null(m))is faluse,cacheSolve() inverses the matrix from the input object. 
              }
              data <- x$get()
              m <- solve(data)
              x$setinverse(m)
              m          #return the inversed matrix to the parent environment by printing m. 
}
