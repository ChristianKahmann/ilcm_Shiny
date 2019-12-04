require(microbenchmark)
require(R6)

#################################################################
#### This part is taken from http://adv-r.had.co.nz/Performance.html 
#### Additional a Talk by Winston CHang http://blog.revolutionanalytics.com/2017/07/the-r6-class-system.html


#Create a function. This function fill be called as a method by the different Classes. 
f <- function(x) NULL


#Dispatch with s3 class
s3 <- function(x) UseMethod("s3")
s3.integer <- f


#Dispatch with a S4 class
A <- setClass("A", representation(a = "list"))
setGeneric("s4", function(x) standardGeneric("s4"))
setMethod(s4, "A", f)
a <- A()

#Write a RC Class
rc <- setRefClass("RC", methods = list(rc = f))
rc <- RC$new()


R6 <- R6Class("R6", public = list(r6=f) )
r6 <- R6$new()

print(
microbenchmark(
  fun = f(),
  S3 = s3(1L),
  R6 = r6$r6()
))


##################################################################################################
## Test the time it takes to change an object n times and obtain the resulting object.
##
## A class with 
##  3 numeric variables (a,b,c) 
##  a get-method for b 
##  a method that adds x to b
##################################################################################################
## S3 Variant
S3V1List <- function()
{
  
  me <- list(
    a = 0,
    b = 0,
    c=0
  )
  
  ## Set the name for the class
  class(me) <- append(class(me),"S3V1List")
  return(me)
}
###Functions
### One Getter 
get_b <- function(object)
{  
  UseMethod("get_b",object)
}

get_b.default <- function(object)
{
  print("Sorry. Wrong Object")
  return(NULL)
}

get_b.S3V1List <- function(object)
{
  return(object$b)
}


# Define the add-function 
add_x_to_b <- function(object, x = 1){
    UseMethod("add_x_to_b", object)
}

#Associate with the class object
add_x_to_b.S3V1List <- function(object, x = 1){
  object$b <- object$b + x
  return (object)
}

# Create a fallback if the object has no association with this function
add_x_to_b.default <- function(object, x=1){
  print("Sorry. Wrong object")
}



#################################################################################################

## Version 2 Environment Approach
S3V2Env <- function()
{
  
  ## Get the environment for this
  ## instance of the function.
  thisEnv <- environment()
  
  a <- 0
  b <- 0
  c <- 0
   
  ## Create the list used to represent an
  ## object for this class
  me <- list(
    
    ## Define the environment where this list is defined so that I can refer to it later.
    thisEnv = thisEnv,
    
    ## Function Definitions
    getEnv = function(){
      return (get("thisEnv",thisEnv))    
    },
    get_b = function()
    {
      return(get("b",thisEnv))
    },
    
    add_x_to_b = function( x = 1 )
    {
      assign("b",  get("b",thisEnv) +x,  thisEnv) 
      return(0)
    }
    
  )
  
  ## Define the value of the list within the current environment.
  assign('this',me,envir=thisEnv)
  
  ## Set the name for the class
  class(me) <- append(class(me),"S3V2Env")
  return(me)
}

## R6 Variant
library(R6)

R6Test <- R6Class("R6Test",
    public = list(
      a = 0,
      b = 0,
      c = 0,
      get_hair = function() {
        return (self$b)
      },
      add_x_to_b = function(x=1) {
        self$b = self$b + x
        invisible(self)
      }
    )
)

## Change an Object Attribute in for loop n times by adding 1 to b
## Designed so you can obtain the object with the changed values (The reason for the one return statement)

S3V1List_loop<- function(s, n=10000){
  for(i in 1:n) {
    s = add_x_to_b(s,1)
  }
  return(s)
}

S3V2Env_loop <- function(s, n=10000){
  for(i in 1:n) {
    s$add_x_to_b(1)
  }
}

R6Test_loop <- function(s, n=10000){
  for (i in 1:n){
    s$add_x_to_b(1)
  }
  
}
#######################################
##Run benchmark

## Instatiate test Variables
s3v1 = S3V1List()
s3v2 = S3V2Env()
r = R6Test$new()



numberofcalls = 1000
print(
  microbenchmark(
    s3_ListApproach_loop = S3V1List_loop(s3v1,numberofcalls),
    s3v2_EnvironmentApproach_loop = S3V2Env_loop(s3v2,numberofcalls),
    R6 = R6Test_loop(r,numberofcalls)
  ))