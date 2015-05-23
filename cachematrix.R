## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## $set stores the matrix.
## $get can get the matrix.
## $setinv stores the inverse of the matrix.
## $getinv can get the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()){
		xinv<-NULL
		set<-function(y){
			x<<-y
			xinv<<-NULL
		}
		setinv<-function(inv) xinv<<-inv
		get<-function() x
		getinv<-function()  xinv
		list(set=set,get=get,getinv=getinv,setinv=setinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

		xinv<-x$getinv() ##Get the inverse of the matrix by $getinv().

		if(!is.null(xinv)) ## If the inverse of the matrix has exist , the function will not compute.
		{
			message("getting the inverse of the matrix")
			return (xinv)
		}
		data<-x$get()
		xinv<-solve(data, ...) ## Compute the inverse of the matrix by solve.
		x$setinv(xinv)
		return (xinv)
}
