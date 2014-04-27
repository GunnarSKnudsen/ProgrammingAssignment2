# This script contains two functions which calculates and caches the inverse of a matrix.
# Example usage:
## > myMatrix <- makeCacheMatrix(matrix(1:4, 2, 2))
## > myInvMatrix <- cacheSolve(myMatrix)
## > myInvMatrix



## Function working as a Matrix wrapper.
### Input: the matrix to be inverted
### Output: List of four functions defined in makeCacheMatrix:
###           setMatrix
###           getMatrix
###           setInvMatrix
###           getInvMatrix
makeCacheMatrix <- function(x = matrix()) {
   #Initialize the inverted matrix (invMatrix) to NULL (empty)
   invMatrix <- NULL
   
   #Create function to set the matrix value
   setMatrix <- function(y){
      # Sets x (the normal matrix) to function argument - which in case is the matrix
      x <<- y
      # Unset the invMatrix, since our matrix (x) may have been altered
      invMatrix <<- NULL
   }
   
   # Get the matrix value
   getMatrix <- function() x
   # Set the inverted matrix:
   setInvMatrix <- function(arg) invMatrix <<- arg
   #Get the inverted matrix
   getInvMatrix <- function() invMatrix
   
   #Return a list with sayed functions:
   list(  setMatrix = setMatrix
        , getMatrix = getMatrix
        , setInvMatrix = setInvMatrix
        , getInvMatrix = getInvMatrix
   )
}



## function calculating and caching the inverse matrix
### Input: x, which is the result from makeCacheMatrix, e.g. a list of functions and a posibly cached matrix.
### Output: the inverted Matrix
cacheSolve <- function(x, ...) {
        
   # set InvMatrix to be calculated using getInvMatrix from makeCacheMatrix.
   invMatrix <- x$getInvMatrix()
   # If invMatrix is defined (ergo: the inverted matrix has been cached previosly:)
   if(!is.null(invMatrix)) {
      message("Getting Cached data")
      # Return previosly cached inverted Matrix
      return(invMatrix)
   }
   # Fetch matrix into variable data
   data <- x$getMatrix()
   # compute the inverse matrix
   invMatrix <- solve(data, ...)
   # Cache the inverse matrix
   x$setInvMatrix(invMatrix)
   # Return the inverted matrix
   invMatrix
}

##Testing - feel free to (un/)comment:
myMatrix <- makeCacheMatrix(matrix(1:4, 2, 2))
#myMatrix
myInvMatrix <- cacheSolve(myMatrix)
myInvMatrix
traceback()

