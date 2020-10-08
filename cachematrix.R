##The first function stores the matrix as a list, along with its inverse as a null 
##value in the initial set up. 
##The list also contains the functions set/get - to set and retrieve 
##value of matrix respectively- as well as the setinverse/getinverse 
## functions to set and retrieve the inverse of the matrix. 
##The first step in console is to pass the function in the makeCacheMatrix to
##create a list that contains the matrix and inverse(after its calculated)
##The second function checks for whether the given list already contains the 
##inverse. If it does, then it retrieves this inverse and if it doesn't, then it
##calculates the inverse and enters the inverse into the list so that later,
##whenever the same matrix's inverse is asked for, it'll directly give the inverse
##instead of solving.
## this function makes a list of four functions 
## to set/retrieve both matrix and its inverse. 

makeCacheMatrix <- function(x = matrix()) 
{
  inverse<- NULL
  
  set<- function(y)
    {
      x<<- y
      inverse<<-NULL
    }
  
  get = function() x
 
  
  setinverse<- function(inv) 
    {
      inverse<<-inv
     
  }
  
  getinverse<- function() 
  {
      inverse
  }
    
  
  list(set=set, get=get,
       setinverse=setinverse, 
       getinverse=getinverse)
  

}
## this function checks whether the passed list contains the inverse and acts
##accordingly, by using the functions defined in the list itself.
cacheSolve <- function(x, ...) 
{

  inverse<-x$getinverse()
  if (!is.null(inverse))
  {
    message("retrieving cached inverse")
    return(inverse)
  }
    matrix<- x$get()
    matrix
    inverse<- solve(matrix)
    x$setinverse(inverse)
    inverse
}
  

