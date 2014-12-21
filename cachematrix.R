## Two functions are defined 
## 1) makeCacheMatrix - to set the matrix with setters and getters function 
##   and saving the matrix in cache.
## 2) cacheSolve - to solve the inverse of a matrix and save it to cache through 
##    above mentioned function.


##  makeCacheMatrix - save a matrix with setters and getters method 
##  for matrix as well as the inverse of the matrix
 
makeCacheMatrix <- function(x = matrix()) {

  ## variable inv holds the inverse matrix object
    inv <- NULL
    
    ## function to reset the matrix if needed
    set <- function(y) {
      x <<- y
      inv <- NULL
    }
    ## get the matrix 
    get <- function() x
    ## set the inverse of the matrix 
    setinv <- function(invert) inv <<- invert
    ## get the inverse of the matrix
    getinv <- function() inv
    
    ## list created to make the right call for setter and getter 
    ## functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
} 


## cacheSolve - calculates the value of inverse of matrix if needed 
## or takes it from the cache if already calculated

cacheSolve <- function(x, ...) {
  
        ## gets the value of inverse of x
        inv <- x$getinv()
        
        ## if its not null returns the value of the inverse
        if(!is.null(inv)) {
         print("getting cache data")
          return(inv)
          
        }
        
        ## inv is null then it calculates the inversion of matrix and returns
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
      
}
