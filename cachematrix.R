## This function will cache the inverse of a matrix 
## Creates a special "matrix", a list containing a function to
##   - set the value of the matrix
##   - get the value of the matrix
##   - set the value of the inverse matrix
##   - get the value of the inverse matrix

makeCacheMatrix <- function(x) {
  
  m <- NULL 
 
    
  set1 <- function(y){
    x <<- y
    m <<- NULL
    }
  
  get1 <-function() x
  
  setmatrix <-function(solve) m <<- solve
  
  getmatrix <- function() m
    
  list(set1 = set1, get1 = get1, setmatrix = setmatrix, getmatrix = getmatrix)  
    
}


## Calculates the inverse of the matrix that was created in makeCachematrix
#function and reuses the cached matrix if availble.

cacheSolve <- function(x, ...) {

  
  m <- x$getmatrix()
  
  if(!is.null(m)){
    message("getting cached data")
    return(m)
    }
  
  y <-x$get1()
  m <- solve(y, ...)
  x$setmatrix(m)
  m
  
}
