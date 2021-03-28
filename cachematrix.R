## The functions cache the inverse of a matrix

## this function will cache the inverse of the matrix.  It returns a list

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  setmatrix<- function(y){
    x<<-y
    m<<-NULL
    
  }
  getmatrix<-function() x
  setinverse<-function(inverse) m<<-inverse
  getinverse<-function() m
  list(setmatrix = setmatrix, getmatrix = getmatrix,
     setinverse = setinverse,
     getinverse = getinverse)
    
}

## computes the inverse of the output from the makeCachematrix function. If the inverse was already found
##it will retrieve the inverse from the cache 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
       m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$getmatrix()
        r <- solve(data, ...)
        x$setinverse(r)
        r
}


