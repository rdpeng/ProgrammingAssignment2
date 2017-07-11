## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## function to create special 'matrix'

makeCacheMatrix <-function(x =matrix())
{
  i <-NULL
  set <-function(y)
  {
    x <<-y
    i <<-NULL
  }
  get <-function() x
  set_inv <-function(inverse) i <<-inverse	##Getter and Setter functions
  get_inv <-function() i
  list(set=set, get=get, set_inv=set_inv, get_inv=get_inv)
}



## Write a short comment describing this function

## function to compute the inverse of the special 'matrix'

cacheSolve <-function(x, ...)
{
  i <-x$get_inv()
  if(!is.null(i))
  {
    message("Getting cached data..")
    return(i)
  }
  data <-x$get()
  i <-solve(data, ...)      ##solve is the function that finds the inverse of the matrix
  x$set_inv(i)
  i
}

## Return a matrix that is the inverse of 'x'
