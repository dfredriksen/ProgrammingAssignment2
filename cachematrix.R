## Helper functions to calculate the inverse of a matrix.
## Daniel Fredriksen, 6/30/2014

## makeCacheMatrix -- Create a matrix object which contains functions for getting and setting the matrix values as well
## as containing variables to store a cached version of its inverse.

makeCacheMatrix <- function(my_matrix = matrix()) 
{
      #inverse cache store property
      inverse <- NULL
 
      #initialize value of matrix
      set <- function(y) 
      {
            my_matrix <<- y
            inverse <<- NULL
      }
    
      #return value of matrix
      get <- function() {my_matrix}
    
      #set the inverse cache store
      setinverse <- function(calculated_inverse) {inverse <<- calculated_inverse}
    
      #return the inverse cache store
      getinverse <- function() {inverse}
    
      #return a list containing the above methods
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Obtain a the inverse of a matrix from it's cache store if it exists, otherwise calculate the inverse and store it in 
## the cache.

cacheSolve <- function(my_matrix, ...) 
{    
    #fetch the inverse from the cache store
    inverse <- my_matrix$getinverse()
      
    #if inverse is cached, return the result
    if( !is.null(inverse) )
    {
          message("getting cached data")
          return(inverse)
    }
    
    
    #otherwise calculate the inverse and store it
    temporary_matrix <- my_matrix$get()
    inverse_result <- solve(temporary_matrix, ...)  
    my_matrix$setinverse(inverse_result)
    
    #return the inverse calculation
    inverse_result
}
