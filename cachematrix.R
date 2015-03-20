## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
