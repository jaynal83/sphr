# step-1: name of the function
# step-2: inputs
# step-3: processing
# step-4: output

# write a function to calculate coefficient of variation for a given numeric input

fCV <- function(numVec){
  avg <- mean(numVec)
  std <- sd(numVec)
  cv <- std/avg
  return(cv)
}

# Create an example numeric vector "x" and compute cv using the newly written function

x <- c(2,3,4,5)
fCV(numVec = x)

# Create a new numeric vector y with one observation is missing (NA)
# compute cv using the function that has been written, can comment on the output
y <- c(2,3,4,5,NA)

fCV(numVec = y)

# Now we will modify the return part of the function and see how the same function
# behaves with the same input x

fCV <- function(numVec){
  avg <- mean(numVec)
  std <- sd(numVec)
  cv <- std/avg
  return(avg)
}
fCV(numVec = x)

# Modify return statement further to take three output as a vector
fCV <- function(numVec){
  avg <- mean(numVec)
  std <- sd(numVec)
  cv <- std/avg
  return(c(avg, std, cv))
}
fCV(numVec = x)

# What is the problem in the output? 

# Return the output as a named vector
fCV <- function(numVec){
  avg <- mean(numVec)
  std <- sd(numVec)
  cv <- std/avg
  return(c(mean= avg, sd= std, cv=cv))
}
fCV(numVec = x)

# Return the output as a data frame
fCV <- function(numVec){
  avg <- mean(numVec)
  std <- sd(numVec)
  cv <- std/avg
  return(data.frame(mean= avg, sd= std, cv=cv))
}

output<-fCV(numVec = x)

#Return the output as a list
fCV <- function(numVec){
  avg <- mean(numVec)
  med <- median(numVec)
  std <- sd(numVec)
  cv <- std/avg
  central <- c(mean = avg, median = med)
  return(list(central.tendency=central, sd= std, cv=cv))
}

output<-fCV(numVec = x)


y<- c(x, NA)
y

fCV(numVec = y)



fCV <- function(numVec){
  avg <- mean(numVec, na.rm = T)
  std <- sd(numVec, na.rm = T)
  cv <- std/avg
  return(c(mean= avg, sd= std, cv=cv))
}
fCV(numVec = y)


fCV <- function(numVec){
  avg <- mean(numVec, na.rm = T)
  std <- sd(numVec, na.rm = T)
  cv <- std/avg
  return(list(mean= avg, sd= std, cv=cv))
}

y<- c(1,2,3)
z<- c("a","b")

fCV(numVec = y)
fCV(numVec = z)




fCV <- function(numVec){
  if(!is.numeric(numVec))
    stop("Input should be numeric")
  avg <- mean(numVec, na.rm = T)
  std <- sd(numVec, na.rm = T)
  cv <- std/avg
  return(c(mean= avg, sd= std, cv=cv))
}

xx <- letters[1:10]

fCV(numVec = xx)


fDescriptive_1 <- function(numVec, type = "classical"){
  if(!is.numeric(numVec))
    stop("Input should be numeric")
  
  type= tolower(type)
  avg <- mean(numVec, na.rm = T)
  std <- sd(numVec, na.rm = T)
  med <- median(numVec, na.rm = T)
  medad <- mad(numVec, na.rm = T)
  out1 <- c(mean = avg, sd = std)
  out2 <-c(median = med, mad = medad)
  if(type== "classical")
    return(out1)
  else if (type == "robust")
    return(out2)
}
fDescriptive_1(numVec = x, type = "CLASSICAL")
fDescriptive_1(numVec = x, type = "rObUST")

fDescriptive(numVec = x)

fDescriptive_2 <- function(numVec, type = ''){
  if(!is.numeric(numVec))
    stop("Input should be numeric")
  
  type= tolower(type)
  if(type== "classical"){
    avg <- mean(numVec, na.rm = T)
    std <- sd(numVec, na.rm = T)
    out1 <- c(mean = avg, sd = std)
    return(out1)
  }
  else if (type == "robust"){
    med <- median(numVec, na.rm = T)
    medad <- mad(numVec, na.rm = T)
    out2 <-c(median = med, mad = medad)
    return(out2) 
  }
}

x <- rnorm(1000)

fDescriptive_1(numVec = x, type = "classical")
fDescriptive_2(numVec = x, type = "classical")

library(microbenchmark)

microbenchmark(fDescriptive_1(numVec = x, type = "classical"), 
               fDescriptive_2(numVec = x, type = "classical"), times = 10000)
