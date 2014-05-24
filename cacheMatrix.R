## This pair of functions creates a matrix object that can cache its inverse
## and return that inverse if it has been calculated

## makeCachematrix creates a matrix object that is able to cache its inverse 
## for retrieval if it has already been calculated

makeCachematrix <- function(x = matrix()) { ## x is a variable argument specifying a matrix
        m <- NULL ## initiates the varialble m 
        set <- function(y) { ## set is a function that sets the y and m variables in relation to x 
                ## in the makeCachematrix environment
                x <<- y ## sets the x variable in the set environment
                m <<- NULL ## sets the m variable in the set environment
        }
        get <- function() x ## retrieves the matrix specified by x
        setmatrix <- function(solve) m <<- matrix ## stores m as the inverse of x
        getmatrix <- function() m ## retrieves the matrix specified by m 
        list(set = set, get = get, ## specifies a list of the functions in the makeCachematrix environment 
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

## cacheSolve retrieves or calculates the inverse of the matrix x 
## depending on the matrix x cached in makeCachematrix 

cacheSolve <- function(x = matrix()) { ## x is a variable argument specifying a matrix
        m <- x$getmatrix() ## retrieves the value of m defined in the makeCachematrix environment
        if(!is.null(m)) { ## checks if m in the makeCachematrix environment
                ##has a value defined by the same x matrix 
                message("getting cached data") ## if the inverse of x has been cached 
                ## this message is displayed
                return(m) ## the inverse is returned as defined # checks if m in the makeCachematrix environment
                ## has a value defined by the same x matrix 
        }
        data <- x$get()  ## defines variable "data" as the matrix specified by x
        m <- solve(data) ## defines m as the inverse of matrix x  
        ## and sets it in the makeCachematrix environment
        x$setmatrix(m) ## sets the inverse of x in the makeCachematrix environment
        m ## returns the inverse matrix of matrix x
}
