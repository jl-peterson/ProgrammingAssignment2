## This set of functions is to cache the inverse of a matrix if present

## Creates a matrix which: 
## 1. Sets the value of a matrix
## 2. Retrieves the inverse
## 3. Sets the inverse value
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function will first check to see if the inverse 
## of the matrix has already been calculated. 
## If true: uses that value and does not do any calculations 
## If false: Computes value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()
  if(!is.null()) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
