##  Creates a special "matrix" object that can cache and 
##  computes the inverse of the special "matrix"

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(matrix_to_reverse = matrix()) {
  ## Empty the cache
  reversed_matrix_value <- NULL
  
  ## Define the set function
  set <- function(reversed_matrix) {
    matrix_to_reverse <<- reversed_matrix
    reversed_matrix_value <<- NULL
  }
  ## Define the get function
  get <- function() matrix_to_reverse
  
  ## Set on cache the reversed value
  setReversedValue <- function(reversed_value) reversed_matrix_value <<- reversed_value
  
  ## Get the reversed value
  getReversedValue <- function() reversed_matrix_value
  
  ## Return the values
  list(set = set, get = get,
       setReversedValue = setReversedValue,
       getReversedValue = getReversedValue)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(matrix_to_reverse, ...) {
        ## Return a matrix that is the inverse of 'x'
        reversed_value <- matrix_to_reverse$getReversedValue()
        
        ## If the reversed value is null, it means that the values hasn't been cached
        if(!is.null(reversed_value)) {
          message("getting cached data")
          return(reversed_value)
        }
        
        ## Get the matrix to reverse
        data <- matrix_to_reverse$get()
        
        ## Reverse the matrix
        reversed_value <- solve(data, ...)
        
        ## Cache the revered value of the matrix
        matrix_to_reverse$setReversedValue(reversed_value)
        
        ## Return the reversed value
        reversed_value
}
