require(mvtnorm)

# Based on R-News posting by Robin Hankin, 16 Dec 2005
# http://tolstoy.newcastle.edu.au/R/help/05/12/17693.html
CorrelatedXY <- function(N, mean1,mean2, 
                         variance1,variance2, 
                         correlation)
{
  corvar <- correlation * sqrt(variance1*variance2)
  rmvnorm(n=N,mean=c(mean1,mean2),
          sigma=matrix(c(variance1, corvar,
                         corvar, variance2), 2,2))
}

# Let's now create six sets of correlated, normally-distributed values with these parameters:
N <- 50000
mean1 <- 2
mean2 <- 5
variance1 <- 3
variance2 <- 2

# Let's set this random number seed so this experiment is repeatable:
set.seed(36)


# Produce first section of the stream:
S <- CorrelatedXY(N, mean1, mean2, variance1, variance2, 0.0) #pair with no correlation
#produce pairs with correlation 0.2, 0.4, 0.6, 0.8 and 1.0
cor <- c(0.4, 0.8, 1, -0.6)
for (i in 1:4)
{
  S <- cbind(S, CorrelatedXY(N, mean1, mean2, 
                                 variance1, variance2,
                                 cor[i] ))
}

#Produce second section of the stream:
#change means/variances
mean1 <- 4
mean2 <- 8
variance1 <- 1
variance2 <- 9

S2 <- CorrelatedXY(N, mean1, mean2, variance1, variance2, -0.6) #pair with no correlation
#produce pairs with correlation 0.2, 1, 0.6, 0.4 and 0.8
cor <- c(0.4, 0.8, 0, 1)
for (i in 1:4)
{
  S2 <- cbind(S2, CorrelatedXY(N, mean1, mean2, 
                               variance1, variance2,
                               cor[i] ))
}

S <- rbind(S, S2)

colnames(S) <- c("a","b","c","d","e","f","g","h","i","j")

S <- as.data.frame(S)

write.csv(S, file="correlatedStream.csv", row.names=F, quote=F)
