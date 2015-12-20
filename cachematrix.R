## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function cachematrix is used to set, get matrix x for which inverse to be computed.
## It also gets the inverse of a matrix and resets the inverse in global environment.
makeCacheMatrix <- function(x = matrix()) {

invmat <- NULL
 ##return matrix x if asked
 getmatrix <- function() x
 
 ##set the matrix to a specified value and reset 
 setmatrix <- function(y)
  {
    x <<- y
    invmat <<- NULL
    
  }
  getinverse <- function() invmat
  setinverse <- function(inv) invmat <<- inv
  list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
  

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  ##First check cached matrix
  invmat <- x$getinverse()
  
  ##check if inverse has already been computed. If so return that matrix inverse
  
  cal <- is.null(invmat)
  if (!cal)
  {
    ## A Cached inverse available. Print message to console.
    print("output cached matrix")
    return(invmat)
    
  }
  else{
    ##If inverse is not computed so far get the matrix and prepare to compute.
    matrix.data <- x$getmatrix()
    
    ##The following step computes the inverse for assumed invertible matrix.
    invmat <- solve(matrix.data,...)
    
    ##Now reset the cache inverse
    x$setinverse(invmat)
  }
  
  
  return(invmat)
}
