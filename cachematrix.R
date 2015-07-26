## The following functions work in tandem to quickly generate an inverse of a 
## matrix. The makeCacheMatrix function provides a means of storing and retrieving 
## inverted and non-inverted values.
##
## The cacheSolve function uses the functions defined in makeCacheMatrix to 
## determine if the matrix given to makeCacheMatrix needs to be inverted. If not, 
## the cached value is returned, sparing the time and effort to calculate. This 
## cached value is stored in the parent environment, via the "<<-" operator. If 
## the matrix passed has not been the most recent invert, then the new matrix is 
## caculated and the new value is stored in the parent environment.

## This function takes a matrix (or a default) and returns a list of functions 
## to set and get it, as well as its inverses.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  get <- function() x
  set.inverse <- function(inv) inverse <<- inv
  get.inverse <- function() inverse
  list(get = get, set.inverse = set.inverse, get.inverse = get.inverse)
}


## This function takes the collection of functions from makeCacheMatrix and 
## inverts the stored matrix, provided a cached version of the same solution 
## doesn't already exist. Otherwise, the cached version is returned.

cacheSolve <- function(funColl, ...) {
  inverse <- funColl$get.inverse()
  
  if (!is.null(inverse)) {
    message("Getting cached data.")
    return(inverse)
  }
  
  data <- funColl$get()
  inverse <- solve(data)
  funColl$set.inverse(inverse)
  inverse
}
