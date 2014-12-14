## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix<-function(x=matrix()){
	rx=NULL
	get=function()x
	set=function(y){
		x<<-y
		rx<<-NULL
	}
	getSolve=function()rx
	setSolve=function(ry)rx<<-ry
	list(get=get,set=set,getSolve=getSolve,setSolve=setSolve)
}

## Write a short comment describing this function
cacheSolve<-function(x, ...){
	s=x$getSolve()
	if(!is.null(s)){
		return(s)
	}
	x$setSolve(solve(x$get()))
	x$getSolve()
}
