makeCacheMatrix<- function(x= matrix()){
  inv_mat<- NULL
  set_mat<- function(y){
    x<<- y
    inv_mat<<- NULL
  }
  get<- function() x
  set_inv<- function (inverse) inv_mat<<- inverse
  get_inv <- function() inv_mat
  list (set= set_mat, get=get, setInv=set_mat, getInv=get)
}
cacheSolve<- function(x,...){
  inv= x$get()
  if (! is.null(inv)){
    message ("getting from cache")}else{
      return(inv)
    }
  
  mat.print<- x$getInv()
  inv<- solve (mat.print, ...)
  x$setInv(inv)
  return(inv)
}


