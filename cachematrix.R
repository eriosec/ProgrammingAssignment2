## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function that gets passed an argument of type matrix
## named x, and does the following:
## 1. It INITIALIZES a matrix of length 0 and assigns it to a matrix variable
##    named invmatrix into which the inverse of x will be stored
## 2. It then defines a function named "set" into which the matrix argument of
##    "makeCacheMatrix" gets passed and assigns it back to the x name that 
##     resides in the parent environment of "set" which happens to be the 
##     execution environment of the function "makeCacheMatrix". The "set" 
##     function also assigns a matrix of length 0 to the invmatrix name also 
##     residing in the parent environment of "set". This in effect caches the 
##     argument matrix and sets its inverse to a 0x0 matrix
## 3.  It then defines a function named "get" which returns the matrix that was
##     cached in the "set" function
## 4.  It then defines a function named "setinverse" which gets passed an inverse
##     matrix and binds it to the invmatrix name that resides in the parent 
##     environemtn of "set" which is also the execution environment of the 
##     function "makeCacheMatrix. This in effect caches the inverse matrix
## 5.  It then defines a function named "getinverse" which returns the inverse
##     matrix that was cached with the "setinverse" function
## 6.  At the end, the function makeCacheMatrix returns a list of the "set", "get",
##     "setinverse", and "getinverse" functions.


makeCacheMatrix <- function(x = matrix()) {
    
    invmatrix <- matrix(numeric(0),0,0)
    
    enclosingenv<-environment(makeCacheMatrix)
    
    runenv<-environment()
    
    callenv<-parent.frame()
    
    set <- function(ymatrix) {
        x <<- ymatrix
        invmatrix <<- matrix(numeric(0),0,0)
    }
    
    get <- function() {
        x
    }
    
    setinverse <- function(inv) invmatrix <<- inv
    
    getinverse <- function() invmatrix
    
    list(enclosingenv=enclosingenv,
         runenv=runenv,
         callenv=callenv,
         set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    invmatrix <- x$getinverse()
    
    if(length(invmatrix)!=0) {
        message("getting cached matrix")
        return(invmatrix)
    }
    
    data <- x$get()
    
    invmatrix <- solve(data)
    
    x$setinverse(invmatrix)
    
    invmatrix
}
