## The function makeCacheMatrix calculates the inverse of entered matrix
## and stores it in cache 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y){
        x <<- y
        inverse <<- NULL
      }
      get <- function() {x}
      setinverse <- function(solve){inverse <<- solve}
      getinverse <- function(){inverse}
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve prints the inverse and also checks whether the same matrix's
## inverse has been calculated before

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inverse <- x$getinverse()
      if(!is.null(inverse)){
        message("Getting cached data of inverse")
        return(inverse)
      }
      mat<- x$get()
      inverse <- solve(mat,...)
      x$setinverse(inverse)
      inverse
}



