
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
p.b_superior <- mean(b.samples > a.samples)
# probability that p.b is superior to A is 96% probability
# or you could say a p-value of : .04167 (1-p.b_superior)


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


#### Varience Explained AB Test Bayesian Analysis ####
library(dplyr)
library(tidyr)
library(Lahman)

# Grab career batting average of non-pitchers
# (allow players that have pitched <= 3 games, like Ty Cobb)
pitchers <- Pitching %>%
  group_by(playerID) %>%
  summarize(gamesPitched = sum(G)) %>%
  filter(gamesPitched > 3)

career <- Batting %>%
  filter(AB > 0) %>%
  anti_join(pitchers, by = "playerID") %>%
  group_by(playerID) %>%
  summarize(H = sum(H), AB = sum(AB)) %>%
  mutate(average = H / AB)

# Add player names
career <- Master %>%
  tbl_df() %>%
  select(playerID, nameFirst, nameLast) %>%
  unite(name, nameFirst, nameLast, sep = " ") %>%
  inner_join(career, by = "playerID")

# Estimate hyperparameters alpha0 and beta0 for empirical Bayes
career_filtered <- career %>% filter(AB >= 500)

# m <- MASS::fitdistr(career_filtered$average, dbeta,
#                     start = list(shape1 = 1, shape2 = 10))

m <- MASS::fitdistr(career_filtered$average, dbeta,
                    start = list(shape1 = 1, shape2 = 1))

alpha0 <- m$estimate[1]
beta0 <- m$estimate[2]

# For each player, update the beta prior based on the evidence
# to get posterior parameters alpha1 and beta1
career_eb <- career %>%
  mutate(eb_estimate = (H + alpha0) / (AB + alpha0 + beta0)) %>%
  mutate(alpha1 = H + alpha0,
         beta1 = AB - H + beta0) %>%
  arrange(desc(eb_estimate))

aaron <- career_eb %>% filter(name == "Hank Aaron")
piazza <- career_eb %>% filter(name == "Mike Piazza")
two_players <- bind_rows(aaron, piazza)

two_players


library(broom)
library(ggplot2)
theme_set(theme_bw())

two_players %>%
  inflate(x = seq(.28, .33, .00025)) %>%
  mutate(density = dbeta(x, alpha1, beta1)) %>%
  ggplot(aes(x, density, color = name)) +
  geom_line() +
  labs(x = "Batting average", color = "")


piazza_simulation <- rbeta(10000, piazza$alpha1, piazza$beta1)
aaron_simulation <- rbeta(10000, aaron$alpha1, aaron$beta1)

sim <- mean(piazza_simulation > aaron_simulation)
sim # this means there's a 60% probability piazza is better than Aaron


