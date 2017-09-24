## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


#makeCacheMatrix is a function that returns a list containing a function to -
#set value of matrix
#get value of matrix
#set value of inverse
#get value of inverse

makeCacheMatrix <- function(x = matrix()) {
        v<-NULL #setting the inverse of the matrix NULL to be a place holder
        set<-function(y){        #define a function to set the matrix y to a new matrix y and reset the inverse of the matrix to NULL
                x<<-y   
                v<<-NULL
        }
        get<-function() x       #return the matrix x
        setinverse<-function(inverse) v<<-inverse #set the inverse of matrix v to inverse
        getinverse<-function() v #returns the inverse v
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse) #return the special vector containg all functions just defined above
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        v<-x$getinverse()       #first check if v has a value or not
        if(!is.null(v)) {       #if v exist print out message "getting cached data" and return predetermined value v
                message("getting cached data")
                return(v)
        }
        data<-x$get()           #if v does exist, run function determined above to get data
        v<-solve(data,...)      #using the data retrieved to calculate the inverse of matrix and store it in v
        x$setinverse(v)         #set the inverse of matrix calculated above to v
        v                       ## Return a matrix that is the inverse of 'x'
        }
        
