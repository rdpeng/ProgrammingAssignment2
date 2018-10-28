makeCacheMatrix<-function(x=matrix()){
	inver<-NULL           #initializa inverse matrix
	
	set<-function(y){      #set the matrix
		x<<-y
		inver<-NULL
	}
	get<-function() x      #get matrix
	setInver<-function(inversion) inver<<-inversion         #set inversion matrix
	getInver<-function() inver                                               #get inversion matrix
	
	list(set=set,get=get,setInver=setInver,getInver=getInver)
	#create function list which contains 4 functions

}

cacheSolve<-function(x,...){              ## Return a matrix that is the inverse of 'x'
	inver<-x$getInver()
	if(!is.null(inver)){
		message("getting cached data")
		return(inver)
	}
	mt<-x$get()
	inver<-solve(mt,...)       ##inverse the matrix 
	x$setInver(inver)
	inver
}