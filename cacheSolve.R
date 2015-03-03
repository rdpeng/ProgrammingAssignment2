cacheSolve <- funciton(mat,...) {
  
  #Get the cached inverse matrix
  mat.inverse <- mat$getinverse()
  
  if(!is.null(mat.inverse)){
    
    message("getting cached data")
    return(mat.inverse)
    
  }else{
    
    data <- mat$get()
    
    #Calculate the inverse matrix
    mat.inverse <- solve(data,...)
    
    #Set the inverse matrix to cache
    mat$setinverse(mat.inverse)

    return(mat.inverse)
  }
  
}