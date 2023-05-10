##############################
# FUNCTION: simboxplot2
# purpose: To simulate data for a box plot
# input: name, size, mean, S.D.
# output:  The box plot 
#-------------------- 
simboxplot2 <- function(name1, name2, size1, size2, mean1, mean2, sd1, sd2) {
  
  #Name of Groups 
  nName <- c(name1, name2 )
  
  #Number of Observations in each group
  nSize <- c(size1, size2)
  
  #Mean of each group
  nMean <- c(mean1, mean2)
  
  #Standard Deviation of each group
  nSD <- c(sd1, sd2)
  
  #ID vector for each row
  
  ID <- 1:(sum(nSize))
  resVar <- c(rnorm(n=nSize[1], mean=nMean[1], sd=nSD[1]),
              rnorm(n=nSize[2], mean=nMean[2], sd=nSD[2])) 
  
  TGroup <- rep(nName, nSize)
  ANOdata <- data.frame(ID,TGroup,resVar)
  
  #Plotting ANOVA data
  
  ANOplot <- ggplot(data=ANOdata,aes(x=TGroup, y=resVar, FILL=TGroup)) + 
    geom_boxplot()
  print(ANOplot)
  
  #Modeling ANOVA data
  
  ANOmodel <- aov(resVar~TGroup, data = ANOdata)
  print(ANOmodel)
  print(summary(ANOmodel))
  aggregate(resVar~TGroup,data=ANOdata,FUN = mean)
  #unlist(z)
  #z
  #unlist(z)[7]
  ANOsum <- list(Fval = unlist(z)[7], 
                 probF = unlist(z)[9])
  ANOsum
  
  #Basic ggplot of ANOVA data
  
  ANOPlot <- ggplot(data=ANOdata,
                    aes(x=TGroup,
                        y=resVar,
                        fill=TGroup)) +
    geom_boxplot()
  print(ANOPlot)

}

#############################################
