## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Take a simple matrix  and make a special cache-enabled matrix 'Object(?)'
# that can cache some  of its properties so you don't have to
# recalculate them  . Here it caches the  inverse  of the matrix it represents
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invr) inv <<- invr
  getinv <- function() inv
  
  # Self learning -- added some  other functions, they print
  printmatrix<-function() print(x)
  printinv<-function() print(inv)
  
  # Show this function's  members to outer world
  #Do this by Encapsulating members in a list and send the list to caller
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv,
       printmatrix = printmatrix,
       printinv= printinv
  )
  
}


## Write a short comment describing this function

# Solver method to compute the inverse of a matrix. 
# It takes as input a retooled matrix create by makeCacheMatrix above
# and computes the value of inverse only if it is not already present 
# in the input 'cahceMatrix'. Else it will compute and save the inverse 
# to the input, and then return the value 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #check if the input  already knows its inverse
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("No need to solve. Getting previously cached inverse")
    return(inv)
  }
  # else calculate
  data <- x$get()
  inv <- solve(data, ...)
  # and save to original matrix 'object'
  x$setinv(inv)
  
  #and retunr 
  inv 
}



# An extra  helper function to 
# test our code 
testIt<-function()
{
  mt<- rbind(c(1, -1/4), c(-1/4, 1))  # an invertible matrix 
  mtt<-makeCacheMatrix( mt )
  imtt<-cacheSolve(x=mtt)
  mtt$printmatrix()
  mtt$printinv()
  imtt
  mtt$get() %*% imtt
  
  
}
