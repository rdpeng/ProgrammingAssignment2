## Put comments here that give an overall description of what your
## functions do.
## This will prepare a matrix to be cached for avoiding repeating
## calculations for inverting matrix 
## it receives as input a square matrix and uses it to get inverse
makeCacheMatrix <- function(x = matrix()) {
##  Clean argument m setting to null
	m<-NULL
	set<- function(y) {  
## Sets initial arguments x and m to parent environment
		x<<-y
		m<<-NULL
		}
	get<- function() x
## Prepare arguments to receive results from inverting function
	setinv<- function(inv) m<<-inv
	getinv<-function() m
	list(set=set, get=get, setinv=setinv, getinv=getinv)
}
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
## Get previous value of inverted matrix
    m<-x$getinv()    
    if(!is.null(m)) {
## If previous value is not null, takes cached data and return it
    	message("Getting cached data")
    	return(m)
    	}
## If previous value is not calculated, use solve() to 
## get inverted matrix and returns it into m
    data<- x$get()
    m<-solve(data, ...)
    x$setinv(m)
    m	
 }

