
## This pair of functions are used to cache large computation of matrix inversion
## that is time-consuming and costly.
## They will firstly check whether the computation of certain matrix inversion has
## been done before and stored in cache.
## If yes, they will directly retrieve from cache without taking repeated computing time.
## If no, they will calculate it and save it in cache for the next time use. 

## A simple instance:
## a<-matrix(c(8,2,7,4,3,6,9,5,1),3)
## a.inverse<-makeCacheMatrix(a)
## cacheSolve(a.inverse)
##             [,1]       [,2]        [,3]
## [1,]  0.16363636 -0.3030303  0.04242424
## [2,] -0.20000000  0.3333333  0.13333333
## [3,]  0.05454545  0.1212121 -0.09696970
## cacheSolve(a.inverse)
## getting cached data
## [,1]       [,2]        [,3]
## [1,]  0.16363636 -0.3030303  0.04242424
## [2,] -0.20000000  0.3333333  0.13333333
## [3,]  0.05454545  0.1212121 -0.09696970

## makeCacheMatrix() creates a list of functions that can cache the inverse
## of certain matrix and the matrix itself.
  

makeCacheMatrix <- function(x = matrix()) {
              inverse<-NULL
              set<-function(y){
                   x<<-y
                   inverse<<-NULL
                   }                 ## set the value of matrix to parent environment
              get<-function() x      ## get the value of matrix
              setinverse<-function(minverse) inverse <<-minverse
                                     ## set the inverse of matrix to parent environment
              getinverse<-function() inverse
                                     ## get the inverse of matrix
              list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
                                     ## return list of functions
}


## cacheSolve() returns the inverse of certain matrix cached by makeCacheMatrix().
## If it could find the inverse from the cache, it will show the inverse immediately.
## Otherwise, it will calculate the inverse, as well as store the result in cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
          inverse<-x$getinverse()    ## fetch the value in cache
          if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
          }                          ## if the variable is not NULL, return it.
          data<-x$get()              ## store the original matrix in cache
          inverse<-solve(data, ...)  ## calculate the inverse
          x$setinverse(inverse)      ## store the inverse in cache
          inverse
}
