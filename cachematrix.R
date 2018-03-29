MakeCacheMatrix <- function(x = matrix()) {
  
  #Set vector and get vector
  m <- NULL
  set <- function(y) {
    x <<-y 
    m <<-NULL
  }
  
  get <- function() x
  
  #set the inverse matrix and get the inverse matrix
  setinv <-function(solve) m<<-solve
  
  #get the matrix
  getinv <-function() m
  
  #return a list with the 4 member functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
  
}



#If matrix is computed,

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv
  
  #If data is in cache return it
  if(!is.null(m)){
    message("getting cached data")
    return(m)    
  }
  
  
  #If data dosen't solve the matrix
  
  data <-x$get()
  m<-solve(data, ...)
  x$setinv(m)
  m #return result
  
}
