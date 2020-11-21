## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix<-function(i = matrix()) 
    {
	k<-NULL
	set<-function(j)
    {
    i<<-j
	k<<-NULL
    }
	get<-function() 
	setInverse<-function(Inverse) 
	getInverse<-function() 
	list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
    }


## Write a short comment describing this function

cacheSolve <- function(i, ...) {
	invMatrix <- i$getInverse()
	if(!is.null(invMatrix)) {
		message("Cached Data")
		return(invMatrix)
	}
	data <- i$get()
	k<-solve(data,...)
	i$setInverse(k)
	k
    }}}
