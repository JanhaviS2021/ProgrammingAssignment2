## Programming assignment 2
## File contains two functions makeCacheMatrix and cacheSolve

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()){
  s <- NULL
  #set the value of the matrix
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  #solve the matrix and cache its inverse
  getsolve <- function() s
  #set inverse of matrix
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    # If inverse of matrix exists matrix not changed and cached value of matrix is returned
    return(s)
  }
  #When inverse value does not exist, then inverse for the matrix is computed and retured 
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
