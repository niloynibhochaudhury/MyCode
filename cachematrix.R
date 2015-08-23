#Purpose: This function creates a special "matrix" object that can cache its inverse
#Input  : An invertible Matrix
#OutPut : A list containing 
#           function to set input values
#           function to fetch the values of the input Matrix
#           function to set the cache of the inverse Matrix
#           function to fetch the cache of the inverse matrix

makeCacheMatrix <- function(inputMat = matrix(numeric(0), nrow = 0, ncol = 0)) {
  invMat <- NULL

  set <- function(cacheMat) {
    inputMat <<- cacheMat
    invMat <<- NULL
  }
  get <- function() inputMat
  setinv <- function(inv) invMat <<- inv
  getinv <- function() invMat
  list(set = set, 
       get = get, 
       setinv = setinv,
       getinv = getinv)
}

#Purpose: This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
#         If the inverse has already been calculated (and the matrix has not changed), 
#         then the cachesolve should retrieve the inverse from the cache.
#Input  : Special Matrix object returned by makeCacheMatrix function
#Outout : Inverse of the given Matrix

cacheSolve <- function(inputMat, ...) {
  invMat <- inputMat$getinv()

  if(!is.null(invMat)) {
    message("getting cached data")
    return(invMat)
  }

  inputMat_fnc <- inputMat$get()
  
  invMat <- solve(inputMat_fnc, ...)

  inputMat$setinv(invMat)

  invMat
}