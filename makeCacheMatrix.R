# funtion to create a special matrix object that can cache its inverse

makeCacheMatrix<-function(x=matrix()){ #argument x defined with default mode of matrix
    inv<-NULL				   #inv is initilized as NULL. inv will hold the inverse matrix 	
    set<-function(y){   # set function defined that assigns new value of matrix in parent environment
                     x<<-y
                     inv<<-NULL # incase of new matrix inverse is set to NULL
                    }
    get<-function(){ x }  # get function returns the value of the matrix argument
    set_inverse<-function(solve){ inv<<-solve } # assigns value of inverse in parent environment
    get_inverse<-function() { inv }  # to get the value of inverse when called
  
    list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse) # needed to refer to the functions with the $ operator
}