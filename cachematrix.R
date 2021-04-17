## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#the makeCacheMatrix function allows me to create a list containing: 1) set the value of the vector 2) get the value of the vector 3) set the value of the mean 4) get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y) {
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmean<-function(mean) m<<-mean
        getmean<-function() m
        list(set=set, get=get,
             setmean=setmean,
             getmean=getmean)
}


## Write a short comment describing this function
#the cacheSolve function calculates the mean of the list created with the above function. However, it first checks to see if the mean has been calculated. If so, it get the mean from the cache and skips the computation. Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.
cacheSolve <- function(x, ...) {
        m<-x$getmean()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-mean(data,...)
        x$setmean(m)
        m
}
#testing of code
B<-matrix(c(1,2,3,4),2,2)
B1<-makeCacheMatrix(B)
cacheSolve(B1)
