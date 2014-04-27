## cacheMatrix R - Receives a matrix, with four functions, get, set, get inverse and set inverse

## First it creates a function with a matrix as a parameter, and sets two main attributes m (where the inverse matrix will be cached), and x where the actual matrix is stored
## set & get allow the matrix to be set, and retreived
## getInverse and setInverse, are functions in which the latter receives an inverse matrix and set it. 
## getInverse retreives attribute m (inverse matrix)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m 
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}




## cacheSolve - Its a function that receives an X parameter (makeCacheMatrix), checks if the inverse of the matrix has been previously solved, and retreives it, else it solves it and stores it. 

## First it gets the Inverse matrix from the makeCacheMatrix function, getInverse, if m (where the inverse matrix should be stored) is empty it sets the variable data with the matrix , solves the inverse and stores the inverse matrix on m and set it into the setInverse function from makeCacheMatrix

## The function checks if the variable m is empty at the beggining so it can retrieve the cached data instead of solving for the inverse again. 

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve (data, ...)
  x$setInverse(m)
  m
}
