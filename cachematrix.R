## Put comments here that give an overall description of what your
## functions do

## The first function, `makeCacheMatrix` creates a list containing
## functions to

## 1. set:  set the value of the matrix to be inverted
## 2. get: get the value of the matrix to be inverted
## 3. setinv:  set the value of the inverse
## 4. getinv:  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  
  list(set=set, get=get,
       setinv=setinv,
       getinv=getinv)

}


## cacheSolve calculates the inverse of the matrix created with
## the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it `get`s the 
## inverse from the cache and skips the computation. Otherwise,
## it calculates the inverse of the matrix and sets the value of
## the inverse in the cache via the `setinv` function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  mat <- x$get()
  i <- solve(mat,...)
  x$setinv(i)
  i
}
