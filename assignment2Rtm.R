#solution assignment 2:
#what the code does is:
#creating a function which is a list of functions which can be called individualy.
# it takes a matrix, than defines inverse to be null. defines "get" to fetch the
#matrix saved in memory, and set to "set" a function in the memory(not within the
#function itself),"set " also resets the "inverse " in the memory to null.
# it's 2 other functions of getinvrse and setinvrse are equaly self explinatory,they
#serve to retrive a matrix that's already been inversed and stored in memory, and
# to set a newly inversed matrix in memory, respectivally.
#the second function takes advantage of lexical scoping, which allows access
#not only to the first function, but also to it's variables as defined in
#global enviorment. it thus uses all that as it's input. what it does is first
#pulling  contants of inversed matrix to current enviorment and verifing wether
# it actually exists (first 3 lines)and if so prints the massage and value.
#if it doesnt already exists it oulls original matrix(mt$get) to current enviorment,
#inverses it("solve(data)"), sets it in memory and returns it's value.
#before using it run the first part, save it as "my_matfunc". than use that as
#input for the "cachesolve" function. enjoy:)


makeCacheMatrix<-function(mt=matrix()){
  inv<-NULL
  get<-function() mt
  set<-function(y){
    mt<<-y
    inv<<-NULL
  }
  getinvrse<-function () inv
  setinvrse<-function(invrse){
    inv<<-invrse
  }
  list(get=get,set=set,getinvrse=getinvrse,setinvrse=setinvrse)
}


cacheSolve<-function(mt,...){
  inv<-mt$getinvrse()
  if(!is.null(inv)){
    print ("getting inverse matrix from cache")
    return(inv)
  } 
  data<-mt$get()
  inv<-solve(data)
  mt$setinvrse(inv)
  inv
}

my_matfunc<-makeCacheMatrix(test_matrix)   #use me like this
cacheSolve(my_matfunc)