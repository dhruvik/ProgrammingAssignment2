#Below are two functions that are used to create a special object that stores a matrix and cache's its inverse.

#The first function makeCacheMatrix returns a list containing following functions
#setMatrix - set the value of the Matrix
#getMatrix - get the value of the Matrix
#setInverse -  set the value of the inverse of Matrix
#getInverse - get the value of the inverse of Matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL
  
  #Store the matrix
  setMatrix <- function(newMatrix){
    x <<- newMatrix
    inverse_matrix <<- NULL
  }
  
  #Get stored matrix
  getMatrix <- function(){
    x
  }
  
  #Cache Inverse of Matrix
  setInverse <- function(inverseMatrix){
    inverse_matrix <<- inverseMatrix
  }
  
  #Retrieve Inverse of Matrix
  getInverse <- function(){
    inverse_matrix
  }
  
  #Return List Containing all the functions
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


#The following function calculates the Inverse of the special "matrix" created with the above function. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #Get The Inverse of Matrix
        inverseMatrix <- x$getInverse()
        
        #Check if Inverse was cached. 
        
        #If yes, return that inverse
        if(!is.null(inverseMatrix)){
          message("getting cached data")
          return(inverseMatrix)
        }
        
        #If No, Calculate Inverse and cache the inverse for future references
        original_matrix = x$getMatrix()
        inverseMatrix <- solve(original_matrix)
        x$setInverse(inverseMatrix)
        
        #Return Inverse of Matrix
        inverseMatrix
}
