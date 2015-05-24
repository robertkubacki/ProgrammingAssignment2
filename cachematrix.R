## Functions that cache the inverse of a matrix
##
## Usage example:
##
## > source('cachematrix.R')
## > m <- makeCacheMatrix(matrix(c(2, 2, 2, 0), c(2, 2)))
## > cacheSolve(m)
## [,1] [,2]
## [1,]  0.0  0.5
## [2,]  0.5 -0.5


## Create a special "matrix", which is a list containing
## a function to
##   - set the value of the matrix
##   - get the value of the matrix
##   - set the value of the inverse matrix
##   - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  ##   - set the value of the matrix
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ##   - get the value of the matrix
  
  get <- function() x
  
  ##   - set the value of the inverse matrix
  
  setinverse <- function(inv) i <<- inv
  
  ##   - get the value of the inverse matrix
  getinverse <- function() i
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## Calculate the inverse of the special "matrix" created with the above
## function, reusing cached result if it is available


cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  m <- x$get()
  i <- solve(m, ...)
  x$setinverse(i)
  i
}

