## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ##  initialize the inverse function
    inv <- NULL
    
    ##  set the matrix to a new matrix
    set <- function(m)  {
        x <<- m
        inv <<- NULL
    }
    
    ##  access the matrix
    get <- function() x
    
    ##  set the value of inv to that of i
    setinv <- function(i) inv <<- i
    
    ##  access the inverse of the matrix
    getinv <- function() inv
    
    ##  return the list of the above methods
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    ##  get the value of inv stored in the cacheMatrix
    inv <- x$getinv()
    
    ##  return the inverse if the inverse has already been calculated
    if(!is.null(inv))   {
        print("Getting the cached inverse")
        
        ##  return the inverse and end the function call
        return(inv)
    }
    
    ##  access the matrix
    mat <- x$get()
    
    ##  calculate the inverse of the matrix
    inv <- solve(mat)
    
    ##  set the value of inverse to inv in x
    x$setinv(inv)
    
    ##  return the inverse
    inv
}
