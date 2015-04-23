## Put comments here that give an overall description of what your
## functions is to calculate inverse of a matrix

## Create a temporary matrix
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y){
    matrix <<- y
    inv <<- NULL
  }
    get <- function(){
    matrix
  }
  
  
  setInverse <- function(inverse) {
     
    inv <<- inverse
  }
  
  
  getInverse <- function() {
    
    inv
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the matrix

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  
  m <- solve(data) %*% data
  
  x$setInverse(m)
  
  m 
}
