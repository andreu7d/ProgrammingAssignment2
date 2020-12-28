## Functions that enable the cache-ing the calculations of the inverse of a 
## matrix. In the first run it calculate the inverse of a matrix, if it was
## already calculated, and the data was not changed, it return the cached inverse

#' Matrix pseudo object that is able to cache the inverse of a matrix
#'
#' @param x A matrix that will be used as source data
#' 
#' @return List of methods to call [get, set, getInverse, setInverse]  
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  
  get <- function() x 
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function()  i
  
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)

}


#' Validates that matrix object that it is non-null, numeric and invertible
#' 
#' @param data A matrix that is invertible
#' 
#' @return logical TRUE (validated) or FALSE (non-valid)
#' 
validate <- function(data){
  if (is.null(data)){
    error("Data is null")
    return(FALSE)
  }
  
  if (sum(apply(a, c(1,2), is.numeric)) / length(a) != 1){
    error("Matrix is not fully numeric, expect numeric matrix")
    return(FALSE)
  }
  
  if (det(data) == 0){
    error("Matrix is not invertable, determinant is 0, returning NA")
    return(FALSE)
  }
  
  TRUE
  
}

#' Gets the inverse of a makeCacheMatrix, using cache if possible
#' 
#' @param x makeCacheMatrix closure with an invertible matrix in it
#' 
#' @param ... aditional parameters passed to solve 
#' 
#' @return Inverse of x
#' 
#' @example 
#'  x <- matrix(c(5,1,0,
#'            3,-1,2,
#'            4,0,-1), nrow=3, byrow=TRUE)
#'  mat <- makeCacheMatric(x)
#'  cacheSolve(mat)
cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()
  if (!is.null(inv)){
    message("using cached data")
    return(inv)
  }
  data <- x$get()
  
  ## validate data 
  if (!validate(data)) {
    return(NA)
  }
  
  
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
