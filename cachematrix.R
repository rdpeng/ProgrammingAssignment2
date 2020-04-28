
# 'MakeCacheMatrix' takes a matrix as argument and recieved, when created, its own environment containing:
# 1) Two data objects: 'x' (to recieve a matrix) and 'j' (to store its inverse matrix)
# 2) a list of functions (with their own environment) that point to those two objects (in their parent environment)

# When 'MakeCacheMatrix' is given a matrix as argument and the resulting list is assigned to an object,
# that objects (that we will call 'MyMatrixObject') recieves a complete copy of the environment of 'MakeCacheMatrix'.

# - 'set' allows to change matrices and to reset the inverse matrix to 'NULL' without having to use 'MakeCacheMatrix' again.
# The deep assignment arrow (<<-) modifies the variables 'x' and 'j' found in the parent 'MyMatrixObject' environment.
# In that case, the data previously cached in the 'MyMatrixObject' is lost. To cache the inverse of several matrices,
# we would need to run 'MakeCacheMatrix' for each one and create as many objects.

# - 'get' allows to retrieve the matrix 'x'. The variable 'x' is absent from its environment, but due to lexical scoping,
#  the functions is able to find it in its parent 'MyMatrixObject' environment.

# - 'setinv' allows to cache the inverse of the matrix, after the function 'cacheSolve' has calculated it.
# The deep assignment arrow (<<-) modifies the variable 'j' in its parent 'MyMatrixObject' environment.

# - 'getinv' retrieves the inverse of the matrix from the cache in exactly the same way as the function 'get'.
# The variable 'j' is absent from its environment, but due to lexical scoping; the functions retrieves it from the parent environment.

makeCacheMatrix <- function(x = matrix()) {
        j<<-NULL
        set<-function(y){
                j<<-NULL
                x<<-y
        }
        get<-function(){
                x
        }
        setinv<-function(i){
                j<<-i
        }
        getinv<-function(){
                j
        }
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv)
}


# 'cacheSolve' takes as argument the 'MyMatrixObject' object created in the global environment by the function 'makeCacheMatrix'.
# It calls the functions contained in that object, which in turn use the variables belonging to the environment in which they were created (the 'MyMatrixObject' environment).

# The behavior of 'cacheSolve' depends on the following conditions:
# - If the inverse of the matrix has already been calculated, it is retrieved the cached inverse matrix.
# - If it has not been calculated, the function checks if the matrix is invertible.
#     - If it isn't, it returns the message "This matrix is not invertible", if it is not square, a message is returned, indicating the problem.
#     - If it is invertible, the inverse is calculated, cached in the 'MyMatrixObject' environment using the 'setinv'function, and returned.

cacheSolve <- function(mmo, ...) {
        i<-mmo$getinv()
        if(!is.null(i)){
                message("getting cached inverse matrix")
                return(i)
        }
        x<-mmo$get()
        d<-dim(x)
        if(d[1]!=d[2]){
                return("No inverse. The matrix should be square.")
        }
        if(det(x)!=0){
                i<-solve(x)
                mmo$setinv(i)
                return(i)        
        }
        message("This matrix is not invertible")
}

## To test the code:
##-----------------
#m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
#mcm<-makeCacheMatrix(m1)
#cacheSolve(mcm)
#(n1<-cacheSolve(mcm))
#n1%*%m1 #as expected, when we multiply the matrix by its inverse, we get an identity matrix

## To change matrices without using makeCacheMatrix again:
##-------------------------------------------------------
#m2<-matrix(1:4,nrow=2)
#mcm$set(m2)
#cacheSolve(mcm)
#(n2<-cacheSolve(mcm))
#n2%*%m2 #as expected, when we multiply the matrix by its inverse, we get an identity matrix

