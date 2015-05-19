##these two functions calculate the inverse of a matrix and if the inverse 
##has already been calculated before it return the stored result

## makeCacheMatrix creates a matrix which is a list containing a function to:
## 1.set the value of the matrix
##2.get the value of the matrix
##3.set the value of the inverse (solve function)
##4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        v<-NULL
        set<-function(y){
                x<<-y 
                v<<-NULL
        }
        get<-function(){
                x
        }
        setinverse<-function(solve){ 
                v<<-solve
        }
        getinverse<-function(){ 
                v
        }
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


##if the value stored with getinverse exists and is not null, cachesolve returns 
##the value, if null it gets data from makeCacheMatrix and inverse it and return 
##that value

cacheSolve <- function(x, ...) {
        v<-x$getinverse() 
        if(!is.null(v)){
                message("getting cached data")
                return(v) 
        }
        data<-x$get 
        v<-solve(data,...)
        x$setinverse(v)
        v 
}
