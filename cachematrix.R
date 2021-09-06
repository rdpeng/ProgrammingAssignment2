#makeCacheMatrix is a function that creates a 4-function list; set (stores the input matrix), get(retrieve the inverse matrix),
#setInv(stores the inverse matrix) and getInv(retrieve the inverse matrix)

makeCacheMatrix<- function(x= matrix()){
  inv_mat<- NULL
  set_mat<- function(y){
    x<<- y
    inv_mat<<- NULL
  }
  get<- function() x
  set_inv<- function (inverse) inv_mat<<- inverse
  get_inv <- function() inv_mat
  list (set= set_mat, get=get, setInv=set_inv, getInv=get_inv)
}

#cacheSolve is the function of the output list from makeCacheMatrix which return the inverse matrix as an output

cacheSolve<- function(x,...){
  inv= x$get()
  if (! is.null(inv)){
    message ("getting from cache")
      return(inv)
    }
  
  mat.print<- x$getInv()
  inv<- solve (mat.print, ...)
  x$setInv(inv)
  return(inv)
}


