## These functions will cache the inverse of a matrix.
## 'makeCacheMatrix' 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## Set the matrix value from the function call
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Source the matrix from the function call
  get <- function() x
  
  ## Assign the value of the solved inverse matrix into the inv variable
  setinv <- function(solved) inv <<- solved
  
  ## Source the solved inverse matrix from the inv variable
  getinv <- function() inv
  
  ## Put the above methods into a list object
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## This function computes the inverse of the special "matrix" returned by 
## `makeCacheMatrix` above. 
                                                    
cacheSolve <- function(x = matrix(), ...) {
  ## Return a matrix that is the inverse of 'x' if already present
  inv <- makeCacheMatrix(getinv()) 
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## Source the matrix using the get method
  data <- makeCacheMatrix(get())
  
  ## 'Solve' the matrix to get the inverse of it
  inv <- solve(data, ...)
  
  ## Put the calculated inverse matrix into the cache
  makeCacheMatrix(setinv(inv))
  
  ## Print the inverse matrix to the screen
  inv
  
  ## Got past the "Error in x$getinv : $ operator is invalid for atomic vectors" with the bracketed method call but I don't know why it is stopping after the "getting cached data" message.
  ## Output from cacheSolve():
  
  ##> cacheSolve(mat)
  ##getting cached data
  ##$set
  ##function (y) 
  ##{
  ##  x <<- y
  ##  inv <<- NULL
  ##}
  ##<environment: 0x0000000017e55f70>
    
  ##  $get
  ##function () 
  ##  x
  ##<environment: 0x0000000017e55f70>
    
  ##  $setinv
  ##function (solved) 
  ##  inv <<- solved
  ##<environment: 0x0000000017e55f70>
    
  ##  $getinv
  ##function () 
  ##  inv
  ##<environment: 0x0000000017e55f70>
  ##   End of output   ##
  
}

