library(ggplot2)
library(dplyr)

#p is the probability that a driver has a bad week
p <- 0.02
#n is the number of drivers in a city
n <- 200
#Number of potential missing drivers
k <- 5

#Probability that MV does not lose money
monte <- mean(replicate(100, {
  samp <- sample(c(TRUE, FALSE), size=190, replace = TRUE, prob=c(1-p,p))
  if(sum(samp) > 190-k){
    1
  } else {
    0.00001
  }
}))

#Vector of values as n decreases
big_monte <- rowMeans(replicate(100, {
  diff <- rep(NA, length(seq(n, 10, -10)))
  i <- 1
  for(x in seq(n, 10, -10)){
    diff[i] <- mean(replicate(100, {
      samp <- sample(c(TRUE, FALSE), size=x, replace = TRUE, prob=c(1-p,p))
      if(sum(samp) > x-k){
        1
      } else {
        0.00001
      }
    }))
    i <- i+1
  }
  diff
}))

dat <- data.frame(cbind(big_monte, index = seq(n, 10, -10)))
ggplot(data = dat, aes(x=index, y=big_monte)) +
  geom_line()+
  geom_point()+
  ylab("Probability you make money") +
  xlab("Number of drivers") +
  title("Probability of Revenue on Number of Drivers")
