## Put comments here that give an overall description of what your code does
## makeCacheMatrix : 
##                  Input: a matrix . Example : x where x=rbind(c(1, -1/4), c(-1/4, 1))
##                  Output: a list of vector that can be called to alter other vectors.
##                  ***set the output list to vector 'z' so that it can be used by makeCacheMatrix(). 
## Description : This function accepts a matrix and based on if the function is called or vectors within the function,
##               it either returns a list of vectors or based in whether this matrix was provided last time as input 
##               or not it returns an inverse matrix or a NULL value.
##
##
## cacheSolveMatrix : 
##                  Input: a matrix. Example : x where x=rbind(c(1, -1/4), c(-1/4, 1)) and the list vector('z') from makeCacheMatrix 
##                         (implicit - no need to explicite mention this one)
##                  Output: Either an inversed matrix.[or if you are messing with the input then an error message]  
## Description : This function accepts a matrix and based on if this matrix was provided last time as input 
##               or not it returns an inverse matrix or a valid error message.(to help you with the debugging).
##
## Caution : As per the assignment the input should be always Inversible, 
##          However validations are built in to handle: 
##           1. Non-Inversible Matrix: Common logic if determinent of a matix is ~ 0 then the inverse of the matrix is not possible.
##             So if your det(matrix)<=0 then the system will not accept the input.
##           2. Non Matrix vectors: incase you feel like testing the code just by entering x=NULL or something funny like that.
##           3. Non square Matrix: Incase the input is a matrix but not qite what we are expecting.
##
##
##      ***To help you with testing this code a set of test data is already created and kept at the bottom of this code.
##         You may either 1: Run this whole code and the test cases will get executed automatically or 
##                        2: after running the functions seperately you may select only the testcase section and run them.
##
##
## Author : laik.sandeep@gmail.com
##



## Write a short comment describing this function
## makeCacheMatrix : 
##                  Input: a matrix . Example : x where x=rbind(c(1, -1/4), c(-1/4, 1))
##                  Output: a list of vector that can be called to alter other vectors.
##                  ***set the output list to vector 'z' so that it can be used by makeCacheMatrix(). 
## Description : This function accepts a matrix and based on if the function is called or vectors within the function,
##               it either returns a list of vectors or based in whether this matrix was provided last time as input 
##               or not it returns an inverse matrix or a NULL value.
##


makeCacheMatrix <- function(x = matrix()) {
  ##Initialization
  m <- NULL
  oldx <- matrix()
  
  ##Not sure why this piece of code was there; didn't touch it
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ##Returns the old Input matrix
  get <- function() x
  
  ##Saves the Input and the output matrix for future use within this env
  setsolv <- function(solv,x){ 
    m <<- solv
    oldx <<- x
  }
  
  ##Checkes if the input matrix is a new one or same as old one, if old it returns the pre-calculated inverse matrix, 
  ##else sets the new Matrix as the matrix to be used for calculating inverse going further & resets the inverse matrix value to NULL
  ## which will trigger new inverse matrix calculation in the calling function.
  getsolv <- function(newx){
    if(dim(oldx) == dim(newx) && all(oldx == newx)){
      return(m)
    }
    else{
      message("The input matrix is new : Recalculating the Inverse Matrix")
      x <<- newx
      m <- NULL
      return(m)
    }
  }
  
  ##A standerd way to make the vectors available to the calling function 
  ##by putting the vectors in a list and later using them with $ sign.
  list(set = set, get = get,
       setsolv = setsolv,
       getsolv = getsolv)
  
}


## Write a short comment describing this function
## cacheSolveMatrix : 
##                  Input: a matrix. Example : x where x=rbind(c(1, -1/4), c(-1/4, 1)) and the list vector('z') from makeCacheMatrix 
##                         (implicit - no need to explicite mention this one)
##                  Output: Either an inversed matrix.[or if you are messing with the input then an error message]  
## Description : This function accepts a matrix and based on if this matrix was provided last time as input 
##               or not it returns an inverse matrix or a valid error message.(to help you with the debugging).
##

cacheSolveMatrix <- function(x, ...) {
  
  ## Print the original Matrix
  message("------X-------\nOriginal Matrix:")
  print(x)
  
  ## Error handling : If not a matrix
  if(class(x)!="matrix"){
    message("Invalid value of X - Not a matrix. Execution aborted.\n")
    return(0)
  }
  ## Error handling : If not a inversible matrix
  if(det(x)==0){
    message("Det(x) = 0; Inverse matrix populating not possible. Execution aborted.\n")
    return(0)
  }
  
  
  m <- z$getsolv(x)
  if(!is.null(m)) {
    message("Printing CACHED/OLD Inversed Martix:")
    return(m)
  }
  data <- z$get()
  m <- solve(data)
  z$setsolv(m,x)
  message("Printing new Inversed Martix:")
  return(m)

}






###Test Input(s) :

###Valid Scenario:

##Set a inversible matrix to m
x <- rbind(c(1, -1/4), c(-1/4, 1))

##Calling makeCacheMatrix with X so that the values gets initialised.
z<- makeCacheMatrix(x)


##Calling cacheSolveMatrix with X(the actual inversible matrix) and Z(the list of vectors from makeCacheMatrix) as parameters
t1 <- 0
t1 <- cacheSolveMatrix(x)
print(t1)
##Re calling the cacheSolveMatrix with same matrix as above to show that it returns the cache data
t1 <- 0
t1 <- cacheSolveMatrix(x)
print(t1)
##Re calling the cacheSolveMatrix with new inversible matrix to show that it returns the new data accordingly
x <- rbind(c(1, 1/2), c(1/2, 1))
t1 <- 0
t1 <- cacheSolveMatrix(x)
print(t1)
##Re calling the cacheSolveMatrix with same matrix as above to show that it returns the cache data
t1 <- 0
t1 <- cacheSolveMatrix(x)
print(t1)


###Invalid Scenario 1: with non inversible matrix
x=rbind(c(3,6,3),c(5,2,1),c(1,2,1))
t1 <- 0
t1 <- cacheSolveMatrix(x)
print(t1)

###Invalid Scenario 2: with non matrix vector
x=c(3,6,3)
t1 <- 0
t1 <- cacheSolveMatrix(x)
print(t1)

###Invalid Scenario 3: with non square matrix
x=rbind(c(3,6,3))
t1 <- 0
t1 <- cacheSolveMatrix(x)
print(t1)