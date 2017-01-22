# Matrix inversions benefits us to caching the inverse of a matrix rather than computing it over and over again. 
# makeCacheMatrix function sets the value of the matrix then and get the value of the matrix. After it will set the value of inverse of the matrix and then get the value of inverse of the matrix

makeCacheMartix<- function(x=matrix()){
	inv<-NULL
	set<-function(y){
		x<<-y
		inv<<-NULL
	}
	get<-function()x
	setInv<-function(inverse)inv<<-inverse
	getInv<- function()inv
	list(set=set, get=get,
		setInv=setInv,
		getInv=getInv)
}



## This function returns the inverse of the matrix also but it will first check ifthe inverse has already been computed. 
## If it HAS it gets the result and skips the computation. 
## If it HAS NOT, it computes the inverse and then sets the value in the cache through the setInv function.

makeCacheMartix<- function(x=matrix()){
	inv<-NULL
	set<-function(y){
		x<<-y
		inv<<-NULL
	}
	get<-function()x
	setInv<-function(inverse)inv<<-inverse
	getInv<- function()inv
	list(set=set, get=get,
		setInv=setInv,
		getInv=getInv)
}



