## Assignment: Caching the Inverse of a Matrix 
## Matrix inversion is usually a costly computation and there may be
## some benefit to caching the inverse of a matrix rather than compute
## it repeatedly. 
## Here is a pair of functions that cache the inverse of a matrix.
##
## By: Sangita Gupta

## This function, makeCacheMatrix(), creates a special "matrix",
## which is really a list containing a function to:
## 1. "set" and "get" the value of a matrix.
## 2. "set" and "get" the value of the inverse of the matrix.

makeCacheMatrix <- function(mData = matrix()) {
  matrixInverse<-NULL
  matrixData<-NULL
  
  set<-function(yData){
    matrixData<<-yData
    matrixInverse<<-NULL
  }
  
  get<-function() {matrixData}
  
  setInverseMatrix<-function(mInverse){
       matrixInverse<<-mInverse
  }
  
  getInverseMatrix<-function(){matrixInverse}
  
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)

}


## This function computes the inverse of a matrix.
## For this assignment, we assume that the matrix supplied is always
## invertible. If the inverse for this data has not been calculated,
## then calculate, cache and return its value. If the inverse for this data
## has been calculated then return its cached vaue.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mInverse<-x$getInverseMatrix()
  if(!is.null(mInverse))
  {
    print("Getting cached inverse matrix")
  }
  else
  {
    mData<-x$get()
    mInverse<-solve(mData)
    x$setInverseMatrix(mInverse)
  }
  return(mInverse)
}

