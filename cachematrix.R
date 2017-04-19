##Functions that calcultate the inverse of a matrix and store its result in the cache so it doesn't have to be
##done repeatedly.

##Create a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(solve) inv <<- solve
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)

}


##Calculates the inverse of the matrix created by the previous function. Should that inverse already have been
##calcualted, it is retrieved from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)){
    message("Getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInv(inv)
  inv
}
