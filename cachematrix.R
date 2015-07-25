## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{
  # initialize the inverse matrix value
  invMatrix <- NULL
  
  # set the value of the matrix
  set <- function(y) 
  {
    x <<- y
    invMatrix <<- NULL
  }
  # get the value of the matrix
  get <- function() x
  
  # set the value of the inverse
  setInvMatrix <- function(inputMatrix) 
    invMatrix <<- inputMatrix
  # get the value of the inverse
  getInvMatrix <- function() 
    invMatrix
  
  # return a list of all the above functions
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # check if the inverse is already cached,
  # if so, we get the inverse from the cache directly
  
  invMatrix<-x$getInvMatrix()
  if(!is.null(invMatrix))
  {
    message("cached matrix")
    #returns matrix
    return(invMatrix)
  }
  # else, we first get the matrix
  data<-x$getInvMatrix()
  # and calculate the inverse
  invMatrix <- solve(data,...)
  # next, cache the inverse of the matrix
  x$setInvMatrix(invMatrix)
  #returns the matrix
  invMatrix
}


