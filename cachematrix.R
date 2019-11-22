## The objective of this function is to create the inverse of a matrix. 
## It is going to be stored in a cache, which is essentialy a way to store objects.
## The definition of a matrix's inverse is "that the product of the matrix
## and its inverse is the identity matrix, if the inverse exists."

## We start with creating a function for a matrix "x" called makeCacheMatrix
## The function will be constructed following the Vector example shown before. 
makeCacheMatrix <- function(x = matrix()) {
  ## in this case InvMatrix's value is set as NULL ie it can be any value in the future  
  InvMatrix <- NULL
    set <- function(y){
      ##this sets the previouslly called "x" as the new function Y and uses "<<"
      ## to store the new value
      x <<- y
      InvMatrix <<- NULL
    }
    get <- function() x
    ## we are trying to set the inverse of a matrix and not the mean, so
    ## we need to change the example for clarity
    ## the function to get the inverse of a matrix is called 'solve'
    setInverse <- function(solvematrix) InvMatrix <<- solvematrix
    getInverse <- function() InvMatrix
    ##to return the special vector
    list(set = set, get=get, setInverse=setInverse, getInverse=getInverse)
}

##After this, if you try to call the inverse with 'getInverse' in an example,
##it comes up as "NULL" because the inverse has not been calculated yet
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'.
  InvMatrix <- x$getInverse()
    ##here we start a new if function that will calculate the cached data
    ##if the inverse is not already calculated before (ie, if it does not return NULL)
    ##we pass an "if" argument to check if it's null or not, if it IS NULL, it will
    ##use the cached function from before to calculate the inverse of the matrix
    if(!is.null(InvMatrix)) {
      message("getting cached data")
      return(InvMatrix)
    }
  matrix <- x$get()
  InvMatrix <- solve(matrix, ...)
  x$setInverse(InvMatrix)
  InvMatrix
}
  ##without cacheSolve, makeCacheMatrix does not work
