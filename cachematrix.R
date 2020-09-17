#intro###########open me############
#So, the idea is simple, first we set a special list that contains values
#for certain things, in this case being "set,get,setInverse,getInverse"
#auto explainatory what they do, thing is they store the value of a matrix
#and can also store its inverse (we assume it always is invertible), and also
#the ability to set each one of those, that function will be "makeCacheMatrix"
#next is "cacheSolve" which looks in this special list, and looks for the value
# of the inverse of what we set as matrix, if it finds it under "getInverse"
#it returns that value, else it calculates it.
########################



#1#MakeCacheMatrix###############Open me########################
#Looks a lot scarier that what it actually is, or at least i think so
#first we set the inverse represented by (m) as null , as there is no inverse
#if there is no matrix, also we are gonna use it to store it, then we define
#each of the elements of the list, "set" just assigns values using the "<<-"
#operator to set values that are outside of its own envrioment
#now we set "get" as a function that just returns x, no argument needed
#getInverse and setInverse do the same pretty much
#the expected outcome of this is a list with 4 elements, 2 of those only return a valu
#stored in the other 2, basically creates a list that lets us store values easily
########################################




makeCacheMatrix<-function(x = matrix()) {
  m <- NULL
  #define the matrix in the list
  set <- function(matrix) {
    x <<- matrix
    m <<- NULL
  }
  #how to get it
  get <- function() x
  #how to set its inverse
  setInverse <- function(solve) m <<- solve
  #get the inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}




#2#cacheSolve############################Open me################
#this one is way easier, first it checks if our inverse exists, represented by (m)
#then if it doesnt, it calculates the inverse of our matrix, if it does exist then
#it will retrieve it and print it, pretty simple, looks scary at first
################################################


cacheSolve<- function(x,...){
  m <- x$getInverse()
  #cache the inverse, check if it exist
  if(is.null(m)){
    message("solving")
    data <- x$get()
    m<- solve(data,...)
    x$setInverse(m)
    return(m)
    
    #in case it doesnt, create it
  }
  #in case it does, get it
  message("getting cached data")
  x$setInverse(m)
  return(m)
  
}


#3#Test#############################open me#############
#well if you are reading this i thank you for grading taking the time to do it
#here is a quick example to show how it works 

a<- c(1:4)
b<- matrix(a,2,2)
c<- solve(b)
b;c
b%*%c ; c%*%b
#simple values

d<-makeCacheMatrix(b)
d$get()
d$setInverse(c)
cacheSolve(d)
#end of test , should be given "getting cached data"
#and the matrix c at the end
