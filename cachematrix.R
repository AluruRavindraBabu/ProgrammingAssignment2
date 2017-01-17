## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

## Computing the inverse of a square matrix can be done with the solve function in R. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.


makeCacheMatrix <- function(x = matrix()) {
  ## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
                  init.mat <- NULL     ### Initializing matrix with NULL
                      set <- function(y) {
                        x <<- y
                        init.mat <<- NULL
                      }
  
                      get <- function() x
                      setinv <- function(matinverse) init.mat <<- matinverse
                      getinv <- function() init.mat
                      list(set = set, get = get, setinv = setinv, getinv = getinv)              
                      
}



##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
## Return a matrix that is the inverse of "x"
cacheSolve <- function(x, ...) {
  
  init.mat <- x$getinv()
  if(!is.null(init.mat)) {
    print("cached data")
    return(init.mat)
  }
  data <- x$get()
  init.mat <- solve(data, ...)
  x$setinv(init.mat)
  init.mat
}
