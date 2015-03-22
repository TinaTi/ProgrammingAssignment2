## This function creates a special "matrix" object that can cache its inverse

## This function creates matrix, then inverse it and cache it

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}
##This function first checks to see if there is already calculated inverse matrix for the "special" matrix. If so, it
##it gets the inverse matrix from cache and skips the computation. Otherwise, it calculates the inverse matrix of and
##sets it in the cache via the setmatrix function

cacheSolve <- function(x, ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}    ## Return a matrix that is the inverse of 'x'
}
