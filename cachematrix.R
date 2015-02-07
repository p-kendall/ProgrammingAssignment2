## R Programming course, assignment 2
## These functions compute the inverse of a matrix and can cache the inverse 
## so it need not be re-computed


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

      ## NULL is used to indicate that the inverse has not been computed
      Minverse <- NULL
      
      ## used to get the matrix data
      get <- function() x
      
      ## sets the value of Minverse
      setinverse <- function(invset) Minverse <<- invset
      
      ## gets the value of Minverse
      getinverse <- function() Minverse
      
      ## returns a list of three functions
      list(get = get,
           setinverse = setinverse,
           getinverse = getinverse)
      
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated
## then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      
      Minv <- x$getinverse()
      
      ## see if the inverse has already been computed
      if(!is.null(Minv)) {
            message("getting cached data")
            return(Minv)
      }
      
      ## assign the matrix to "data"
      data <- x$get()
      
      ## compute the inverse using solve(). Assumes the matrix is invertible.
      Minv <- solve(data, ...)
      
      ## set the value of Minverse in the special "matrix" x 
      x$setinverse(Minv)
      
      ## return the inverse
      Minv
        
}


## used to test the functions above

test <- function() {
      
      ## an invertible matrix
      M<-matrix(1:9,3,3)
      M[1,3]<-2

      ##first call, shoul dcompute the inverse
      z<-makeCacheMatrix(M)
      cacheSolve(z)
      print("first call to cacheSolve")

      ## second call, should use the cached inverse
      cacheSolve(z)
      print("second call to cacheSolve")

}
