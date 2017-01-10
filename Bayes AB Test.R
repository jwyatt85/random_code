
library(ggplot2)
library(grid)
library(gridExtra)
library(tidyr)

curve(dbeta(x, 1,1)) # these are our weak priors for the AB test.  We don't know or have any reason to believe A over B

# # curve(dbeta(x, 36+4, 114+4), col = "blue")
# # curve(dbeta(x, 50+4, 100+4), add = TRUE, col = "red")

x <- seq(0,1,length=100)
db <- dbeta(x, 36+4, 114+4)
db2 <- dbeta(x, 50+4, 100+4)

ggplot() + geom_line(aes(x, db), colour = "blue", linetype = 1, size = 1) + 
  geom_line(aes(x, db2), colour = "red", linetype = 1, size = 1) + theme_bw()

# ggplot() + geom_line(aes(x, db), colour = "blue") + 
#   geom_line(aes(x, db2), colour = "red")

n.trials <- 100000
prior.alpha <- 4
prior.beta <- 4

a.samples <- rbeta(n.trials,36+prior.alpha,114+prior.beta)
b.samples <- rbeta(n.trials,50+prior.alpha,100+prior.beta)
p.b_superior <- sum(b.samples > a.samples)/n.trials

### Using dbeta with ggplot

# p.b_superior
# par(mfrow = c(2,1))
# hist(b.samples/a.samples, breaks = 40)
# lines(density(b.samples/a.samples), lwd = 2)
# plot(ecdf(b.samples/a.samples)) 

data <- as.data.frame(b.samples/a.samples)
names(data) <- "V1"

hist_plot <- ggplot(
  data = data, 
  aes(V1)
) + geom_histogram(col = "blue", bins = 30, aes(fill = ..count..)) + 
  scale_fill_gradient("Count", low = "green", high = "red") + theme_bw()

cdf_data <- data.frame(x = b.samples/a.samples)
cdf_plot <- ggplot(
  cdf_data, 
  aes(x)
) + stat_ecdf() + 
  geom_hline(yintercept = .50, col = "red") +
  geom_hline(yintercept = .975, linetype = "dotdash", col = "black") + 
  geom_hline(yintercept = .025, linetype = "dotdash", col = "black") + 
  annotate("text", x = 2.0, y =.55, label = "95% confidence interval: Median") + 
  theme_bw()
  

grid.arrange(hist_plot, cdf_plot)




#plot(ecdf(rbeta(1000, 1,1)))


