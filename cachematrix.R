# makeCacheMatrix - This function creates a special "matrix" object that can cache its inverse.
# 1. Sets the value of the matrix
# 2. Gets the value of the matrix
# 3. Sets the value of inverse of the matrix
# 4. Gets the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  d<-NULL
  set<-function(y){
    x<<-y
    d<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) d<<- solve
  getmatrix<-function() d
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMartix above
cacheSolve <- function(x=matrix(), ...) {
  d<-x$getmatrix()
  if(!is.null(d)){
    message("obtaining cached data")
    return(d)
  }
  matrix<-x$get()
  d<-solve(matrix, ...)
  x$setmatrix(d)
  d
}
#Sample Run

#x = rbind(c(2, -1/2), c(-1/2, 2))
#> d = makeCacheMatrix(x)
#> d$get()
#     [,1] [,2]
#[1,]  2.0 -0.5
#[2,] -0.5  2.0
#> cacheSolve(d)
#          [,1]      [,2]
#[1,] 0.5333333 0.1333333
#[2,] 0.1333333 0.5333333
#> cacheSolve(d)
#obtaining cached data
#          [,1]      [,2]
#[1,] 0.5333333 0.1333333
#[2,] 0.1333333 0.5333333