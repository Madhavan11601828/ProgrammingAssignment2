## Week 3 Assignment; submission by June 8, 2020; GitHub user: Venu Madhavan Mangena
## In order to fulfill the process of compleation of Coursera Data Science: R Programming 

## the following function that will create a  "matrix" object which will cache its inverse

makeCacheMatrix <- function(x = matrix()) { ## defined function with the argument mentioned "matrix" as default mode.
  inv <- NULL                             ## the variable inv initialized as NULL and it will hold inverse of matrix. 
  set <- function(y) {                    ## the set function defined for assigning new 
    x <<- y                             ## in the parental environment, the value of the matrix will be considered
    inv <<- NULL                        ## if there exists a new matrix, then set matrix "inv" to "NULL"
  }
  get <- function() x                     ## the get function definedwhich will return the value of the matrix argument
  
  setinverse <- function(inverse) inv <<- inverse  ## in parental environment, assign the value of inv
  getinverse <- function() inv                     ## gets the value of inv where called
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## for refering purpose this will be needed, thefunction with operator, $ 
 
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
