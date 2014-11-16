## set of functions that create a "special" matrix that can cache its inverse
## and calculate the matri inverse if it has not already been cached 

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  
  # set the matrix
  setMatrix <- function( Y ){
    x <<- Y
    inverse <<- NULL # if the matrix has changed, we set the 
                     # value of the inverse to NULL
  }

  # get the matrix
  getMatrix <- function(){return(x)}
  
  
  # functions to set and get the inverse of the matrix
  setInverse <- function(mInverse) inverse <<- mInverse
  getInverse <- function(){return(inverse)}
  
  # return a list with the available function associated with the special matrix
  return(list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse))
}


## calculates and returns the inverse of the matrix if it has not already been 
## cached

cacheSolve <- function(x, ...) {
  
        inverseM <- x$getInverse()
        # if the inverse is not null
        # return the cached inverse
        if (!is.null(inverseM)){
          message("returning cached inverse of matrix")
          return(inverseM)
        }
        
        # if cached inverse in null calculte inverse
        matrix1 <- x$getMatrix() 
        inverseM <- solve(matrix1,...)
        x$setInverse(inverseM)
        return(inverseM)
}
