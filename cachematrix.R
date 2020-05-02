## Put comments here that give an overall description of what your
## functions do

## Similarly to the example functions provided in the assignment,
## makeCacheMatrix creates an opject that will store a numeric matrix
## and cache its inverse. Futhermore, CacheSolve checks for a 
## data cache from the previous function. If it exists, it pulls it.
## Otherwise, it solves for the matrix inverse and returns it.

## Write a short comment describing this function


##Similarly to the example provided, the makeCacheMatrix will
## set the value of the matrix, get the value of the matrix..
## set the value of the inverse, and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m<- NULL
        
        set <- function(y){
          x<<-y
          m<<-NULL
          
        }
        
        get <-function()x
        setinverse <- function(solve) m <<- solve
        getinverse <- function()m
        list(set = set, get =get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function

## cacheSolve checks for cached matrix from makeCacheMatrix function
##If no cached matrix exists, solves for inverse to input matrix
## returns set inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinverse()
        ## if cached data is not null(it exists), retrieve here
        if(!is.null(m)){
              return(m)
        }
        #If it does not, solve for inverse here
        data<-x$get()
        m<-solve(data,...)
        x$setinverse(m)
        #return inverse here
        return(m)
}
