## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL #create empty inverse variable i
  
  set <- function(y) { #set value of the matrix
    x <<- y
    i <<- NULL
  }
  get <- function() x  #get the value of the matrix
  setinverse <- function(inv) i <<- inv  #set the inverse of the matrix
  getinverse <- function() i  # get the inverse of the matrix
  list(set = set, get = get,
       setinverse = setinverse ,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x=matrix, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) { #Check if matrix inverse was already calculated
    message("getting cached data") #if yes, print message and inverse i
    i
  }
  else{ #if inverse was not in cache calculate inverse
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
  }
}
