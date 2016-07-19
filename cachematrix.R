## Functions to calculate and cache the inverse of a square matrix with non-zero determinant

## the function takes as input a matrix (the default arguemt is the null matrix) and sets the value of the matrix, 
#gets the value of the matrix, sets the value of the inverse, and gets the value of the inverse
        makeCacheMatrix <- function(x=matrix()) {
                m<-NULL
                set<-function(y){
                        x<<-y
                        m<<-NULL
                }
                get<-function() x
                setinverse<-function(solve) m<<-solve
                getinverse<-function() m
                list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
        }
        
        
        ## The function below has as an input a matrix. Uses the output of the above function to check whether the inverse 
        # has been cahed. If yes, retrieves and returns the inverse matrix. If the minverse has not been cached, the function calculates and
        #returns the inverse, and updates the field of the list that corresponds to the inverse
        
        cacheSolve <- function(x, ...) {
                 m<-x$getinverse()
                if(!is.null(m)){
                message("getting cached data")
                return(m)
                 }
                 data<-x$get()
                 m<-solve(data,...)
                 x$setinverse(m)
                m
        }
