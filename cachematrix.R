## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x) {
    m <- NULL
    sqM <- matrix(1:x^2, nrow = x, ncol = x)
    set <- function(y){
      sqM <<- y
      m <<- NULL
    }
    get<- function() sqM
    setinv <- function(solve) m <<- solve
    getinv <- function() m 
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    m <- sqM$getinv()
    if(!is.null(m)) {
      message("getting cached data")
      m
    } else {
      data <- sqM$get()
      m <- solve(data, ...)
      sqM$setinv(m)}
    m
        ## Return a matrix that is the inverse of 'x'
}
