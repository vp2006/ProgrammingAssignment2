## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The below function does the following 
## 1. set the value of a matrix
## 2. get the value of the matrix set in step 1
## 3. set the value of the inverse of the matrix. 
##    Note the special symbol "<<-" that is being used for assignment
## 4. get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y){
    x <<- y
    inv <<- NULL
  }
  get <- function ()x
  setinverse <- function (inverse) inv <<- inverse
  getinverse <- function ()inv
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function

## The following function returns the inverse of the matrix :
## it first checks if the inverse has already been computed. if yes,
## it gets the result that has been cached and skips the computation. if not,
## it computes the inverse, sets the value in the cache via the setinverse 
## function. Costly computations can benefit a lot using this feature in R.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data.")
    return (inv)
  }
  
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}

## Providing below the sample run for above listed functions.
## Sample Run 
##x <- rbind (c(1,-12/40),c(-12/40,1))
##> m <- makeCacheMatrix(x)
##> m$get()
##[,1] [,2]
##[1,]  1.0 -0.3
##[2,] -0.3  1.0
## Note that the message "getting cached data" does not appear the first time 
## cacheSolve function is called.
##> cacheSolve(m)
##[,1]      [,2]
##[1,] 1.0989011 0.3296703
##[2,] 0.3296703 1.0989011
## Note that the message "getting cached data" gets printed the second time
## the function cacheSolve is invoked. This time the function retrieves the
## value from cache.
##> cacheSolve(m)
##getting cached data.
##[,1]      [,2]
##[1,] 1.0989011 0.3296703
##[2,] 0.3296703 1.0989011