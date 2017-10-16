## Put comments here that give an overall description of what your
## functions do

## what I want to do here is to present a function which enables me to get the inverse of x, automatically and taking advantage of cache which makes the calculation faster.

makeCacheMatrix <- function(x = matrix()) {
m<-NULL
set<-function(y)
  x<<-y
m<<-NULL
}
  get<-function()x   # I get the function
  setsolve<-function(solve) m <<- solve  # I set the command of the function, here solve, to receive the inverse
  getsolve<-function()m # it solves the function
  
  list(set=set, get=get,
       setsolve=setsolve,
       getsolve=getsolve) # it returns our demanded Output in a list
