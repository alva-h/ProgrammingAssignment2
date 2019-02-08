## These 2 functions demonstrate the advantages of scoping in R. The functions inverse a matrix and cache the results. 
## When the functon is called again, it only performs the inverse if the contents of the matrix changed.
## Otherwise, the inverse if returned from cache

## this function creates a matrix which is a list containing a function to set and get the value of a matrix AND
## set and get the inverse of the matrix


makeCacheMatrix <- function(M = matrix()) {
        ## initialize the inverse matrix to NULL
        invM<-NULL  
        
        ## create function that assigns input argument to M in parent environment AND
        ## assigns NULL to the inverse matrix in the parent environment
        setMatrix <- function(y) {
                M <<- y
                invM <<- NULL
        }
        
        ## gets the value of the inverse
        getMatrix <- function() M
        
        ## calculates the inverse
        setinverse <- function(inv) invM <<- inv
        
        ## gets the inverse
        getinverse <- function() invM
        
        ## passes the value of the makeCacheMatrix()
        list(setMatrix = setMatrix,    ## gives name ‘setMatrix’ to the setMatrix() function
             getMatrix = getMatrix,    ## gives name ‘getMatrix’ to the getMatrix() function
             setinverse = setinverse, ## gives name ‘setinverse’ to the setinverse() function
             getinverse = getinverse) ## gives name ‘getinverse’ to the getinverse() function
}

cacheSolve <-function(x,...){
        invM<-x$getinverse()
        ## gets inverse if it exists
        if (!is.null(invM)) {
                message("getting cached data")
                return(invM)
        }
        ## inverse does not exist so calculate the inverse using solve()
        data<-x$getMatrix()
        i<-solve(data, ...)
        x$setinverse(i)
        i
}  
