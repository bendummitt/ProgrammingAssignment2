## These functions will create a special object that stores a matrix and caches the 
## inverse of that matrix

## This function creates a special matrix which is a list containing functions to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse of the matrix
#get the value of the inverse of the matrix

makeCacheMatrix <- function(mat = matrix()) {
  inv<-NULL
  set<-function(y){
    mat<<-y
    inv<<-NULL
  }
  get<-function() mat
  setInv<-function(solve) inv<<-solve
  getInv<-function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function calculates the inverse of the special matrix created with the above function
## however, it first checks to see if the inverse has already been calculated. If it has, it 
## skips the calculation and returns the cached value along with a message.
## If it has not been calculated, it calculates it and sets the value of the inverse in the 
## cache via the setInv function

cacheSolve <- function(mat, ...) {
  inv<-mat$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- mat$get()
  inv <- solve(data)
  mat$setInv(inv)
  inv
}


