GeomSeq <- function(start, end) {
  vec <- c()
  for (i in start:end) {
    vec <- append(vec, 1/i)
  }
  return(vec)
}

specificity <- GeomSeq(1, 1000)
k.value <- 1:1000

plot(log(k.value), log(specificity))

library(ggplot2)

d <- data.frame(cbind(k.value, specificity))

gp <- ggplot(d, aes(x = log(k.value), y = log(specificity))) + 
  geom_line(colour = "lightcoral", size = 1)

gp
gp <- gp + geom_line(aes(x = log(k.value), y = log(cost.norm)), colour = "royalblue", size = 1)

gp <- gp + geom_line(aes(x = log(k.value), y = log(dser[0:1000])), colour = "springgreen", size = 1)

gp + ylab("") + xlab("Log(k)")


myplot <- ggplot(data = d, aes(x = log(k.value))) + 
  geom_line(y = log(cost.norm), colour = "Log(Cost)") + 
  geom_line(y = log(specificity), colour = "Log(Specificity)") + 
  geom_line(y = log(accuracy.norm), colour = "Log(Accuracy)") + 
  scale_colour_manual("", 
                      values = c("Log(Cost)"="springgreen", "Log(Specificity)"="lightcoral", 
                                 "Log(Accuracy)"="royalblue")) + 
  xlab("Log(k")
  
myplot

cost <- rlnorm(1000)
cost.norm <- (cost - min(cost))/(max(cost) - min(cost))
cost.norm
hist(cost.norm)
cost.norm <- sort(cost.norm)

d <- cbind(d, cost.norm)
d


accuracy <- log(d$specificity)
accuracy.norm <- (accuracy - min(accuracy))/(max(accuracy) - min(accuracy))
d <- cbind(d, accuracy.norm)

plot(d$k.value, log(d$accuracy.norm))
