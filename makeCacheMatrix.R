makeCacheMatrix<-function(x=matrix()){
    inv<-NULL
    set<-function(y){
                     x<<-y
                     inv<<-NULL
                    }
    get<-function(){ x }
    set_inverse<-function(solve){ inv<<-solve }
    get_inverse<-function() { inv }
  
    list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}