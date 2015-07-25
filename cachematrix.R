## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{
  invMatrix <- NULL
  set <- function(y) 
  {
    x <<- y
    invMatrix <<- NULL
  }
  get <- function() x
  
  setInvMatrix <- function(inputMatrix) 
    invMatrix <<- inputMatrix
  getInvMatrix <- function() 
    invMatrix
  
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMarix<-x$getInvMatrix()
  if(!is.null(invMarix))
  {
    message("cached matrix")
    return(invMarix)
  }
  data<-x$getInvMatrix()
  invMarix <- solve(data,...)
  x$setInvMatrix(invMarix)
  invMarix
}


