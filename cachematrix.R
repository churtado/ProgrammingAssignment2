## Programming Assignment 2 - Lexical Scoping
## The following program creates a special type of "matrix" that can cache its own inverse
## It is useful for saving computational effort of obtaining the inverse of a matrix for 
## which the inverse was previously calculated.
## In reality, the "matrix" is a list of functions that allow for caching the inverse.

## In summary, its an exercise on how to use closures in R

## In this function, we define a method to get the matrix itself, get its inverse, or set 
## the value of the matrix or its inverse.
makeCacheMatrix <- function(x = matrix()) {

      #Set the inverse initially to null
      i<-NULL
      
      #Set the matrix to be equal to what is passed in y
      set <- function(y){
            x <<- y
            i <<- NULL
      }
      
      #Simply return the matrix 
      get <- function() x
      
      #Set the inverse to the value passed as a parameter to the function
      setinverse <- function(inverse) i <<- inverse
      
      #Simple return the inverse
      getinverse <- function() i
      
      #Return a list of all of these functions
      list(set = set, get = get, setinverse=setinverse, getinverse=getinverse)
  
}


## This function will try to see if the inverse of a 
## matrix of the type we previously defined has an inverse.
## If it finds the inverse has been calculated, it will return the inverse.
## If no inverse is found, the inverse is calculated with the solve() function, 
## set in our special "matrix". Lastly, the function returns the inverse.
cacheSolve <- function(x, ...) {
  
      ## Call the getinverse function on our special "matrix" to see if the inverse has already been calculated
      i <- x$getinverse()
    
      ## If the value of the inverse is calculated (it's not NULL), simply return it and we're done
      if(!is.null(i)){
            print('Returning cached value')
            return(i)
      }
      
      ## If the return statment was not executed, the rest of the function gets executed
      
      ## Obtain the matrix value with its get() function
      matrix <- x$get()
      
      ## Use the solve() function on the actual matrix to obtain the inverse
      i <- solve(x$get())
      
      ## Set the inverse into our special "matrix"
      x$setinverse(i)
      
      ## Return the inverse
      i
}