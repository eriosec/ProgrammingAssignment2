## 
## Overall functions Descriptions:

## A.  "makeCacheMatrix" is a function that takes an argument of type matrix
##      named x, and does the following:
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
##     matrix and binds it to the invmatrix name that resides in its parent 
##     environemtn which is also the execution environment of the 
##     function "makeCacheMatrix. This in effect caches the inverse matrix
## 5.  It then defines a function named "getinverse" which returns the inverse
##     matrix that was cached with the "setinverse" function
## 6.  At the end, the function makeCacheMatrix returns a list of the "set", "get",
##     "setinverse", and "getinverse" functions.

## B. "cacheSolve" is a function that takes an argument x of type list which
##    needs to have been created using the "makeCacheMatrix" function and then
##    does the following:
## 1. It checks the value of the inverse matrix that was returned through
##    the function "getinverse" in the list of values of the argument x that
##     was passed to "cacheSolve"
## 2. If the length of the inverse matrix is not 0, i.e a 0x0 matrix,
##     then the function ends by returning the inverse matrix that is cached
##     in the "getinverse" function 
## 3. If the length of inverse matrix is zero then the value of the matrix
##    that was originally passed to the "makeCacheVector" function is accessed 
##    and assigned the name: xmatrix. 
## 4. xmatrix is passed to the function solve() to calculate the inverse of
##    xmatrix and the returned matrix is assigned the name: inverse
## 5. The inverse is then cached using the "setinverse" function which is 
##    in the list of values of the argument x that was passed to "cacheSolve"
## 6. Finally, the inverse is returned
##

## Short Descrition of "makeCacheMatrix":

## When bound to a name, "makeCacheMatrix" caches the matrix that gets passed as 
## argument and intitializes its inverse to  a 0x0 matrix and makes both
## available through the lists of functions that are returned.

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


## Short description of "cacheSolve":

## "cacheSolve" is a function that takes an argument x of type list which
##  needs to have been created using the "makeCacheMatrix" function so that  
##  the matrix originally passed to "makeCacheMatrix" can be accessed and
##  its inverse returned either from cache or through calculation which is 
##  then cached through the "setinverse" function that was returned in 
##  the list of values of x.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    inverse <- x$getinverse()
    
    if(length(inverse)!=0) {
        message("getting cached matrix")
        return(inverse)
    }
    
    xmatrix <- x$get()
    
    inverse <- solve(xmatrix)
    
    x$setinverse(inverse)
    
    inverse
}
