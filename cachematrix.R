## These functions creates a special "matrix"
## Then solves and caches the inverse of that "matrix"

## This function creates a special "matrix" objects and gets/caches the inverse
## Defines the Set and Get of the Matrix and the Set and Get of the Inverse
makeCacheMatrix <- function(valMatrix = matrix()) {
     invMatrix <- NULL #inverse matrix initialized to NULL
     
     ## Set Value of Matrix
     set <- function(valMatrix) {
          orgMatrix <<- valMatrix
          invMatrix <<- NULL
     }
     
     ## Get Value of Matrix
     get <- function() {
          orgMatrix
     }
     
     ## Get Value of Inversed Matrix
     setInversed <- function(valMatrix) {
          invMatrix <<- valMatrix
     }
     
     ## Get Value of Inversed Matrix
     getInversed <- function() invMatrix #used to get inversed matrix
     
     #return the list (an list of functions to communicate with the cache)
     list(set=set, get=get,
          setInversed=setInversed,
          getInversed=getInversed )
}


## Returns the Inverse Matrix
cacheSolve <- function(valMatrix, ...) {
     invMatrix <- valMatrix$getInversed()
     
     ## if inversed matrix is available, return it.
     if(!is.null(invMatrix)){
          message("Retrieving cached data...")
          return(invMatrix)
     }
     
     ## if inversed matrix is not available, calculate it.
     message("Calculating Data and Caching...")
     orgMatrix <- valMatrix$get()
     invMatrix <- solve(orgMatrix)
     valMatrix$setInversed(invMatrix)
     invMatrix      
}
