# This function creates a matrix object that can cashe its inverse
# It sets the value of the matrix, gets the value of matrix, sets the value of the inverse 
# matrix and gets the value of the inverse matrix.

makeCasheMatrix <- function(x = matrix()) {
  invs <- NULL
  set <- function (y) {
    x <<- y
    invs <<- NULL
  }
  get <- function() x
  setinverse <- function (inverse) invs <<-inverse
  getinverse <- function () invs
  list (set = set, 
        get = get, 
        setinverse = setinverse, 
        getinverse = getinverse)
}

# Function computes the inverse of the matrix returned by the makeCasheMatrix function

casheSolve <- function(x, ...) {
  invs <- x$getinverse()
  if(!is.null(invs)) {
    message("Getting cashed data")
    return(invs)
  }
  data <- x$get()
  invs <- solve (data,...)
  x$setinverse(invs)
  invs
}
  
# Trial:
# example <- makeCasheMatrix(matrix(c(4,7,2,6,5,3,5,6,2), nrow=3,ncol=3))
# casheSolve(example)


