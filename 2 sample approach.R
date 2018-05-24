
data <- read.csv("/Users/emilyjohnson/Desktop/Stats_Senior_Project_Data.csv", header = TRUE)
library(ggplot2)
head(data)
data_sub <- data[c(1:3,19)]
head(data_sub)
data_sub <- data.frame(data_sub)
data_sub <- na.omit(data_sub)
data_sub <- data_sub[-109,]
data_study1 <- subset(data_sub, studyid < 700)
data_study2 <- subset(data_sub, studyid > 700)

for(i in 1:nrow(data_sub)){
  
  if((data_sub$VISIT[i] == 1 & data_sub$bottype[i] == 1) | 
     (data_sub$VISIT[i] == 2 & data_sub$bottype[i] == 2)){
    data_sub$type[i] <- 'A'
  } else {
    data_sub$type[i] <- 'B'
  }
  
}

# Group A: clear bottle on 1st visit, opaque bottle on 2nd visit
groupA <- subset(data_sub, type == 'A')
groupA_clear <- subset(groupA, bottype == 1)
groupA_opaque <- subset(groupA, bottype == 2)
groupA <- cbind(groupA_clear, groupA_opaque$tfamtml)
groupA <- subset(groupA[,c(1,4,6)])
names(groupA) <- c("StudyID", "Clear Bottle mL", "Opaque Bottle mL")

groupA1 <- subset(groupA, StudyID < 700)
groupA2 <- subset(groupA, StudyID >= 700)

# Group B: opaue bottle on 1st visit, clear bottle on 2nd visit
groupB <- subset(data_sub, type == 'B')
groupB_clear <- subset(groupB, bottype == 1)
groupB_opaque <- subset(groupB, bottype == 2)
groupB <- cbind(groupB_clear, groupB_opaque$tfamtml)
groupB <- subset(groupB[,c(1,4,6)])
names(groupB) <- c("StudyID", "Clear Bottle mL", "Opaque Bottle mL")

groupB1 <- subset(groupB, StudyID < 700)
groupB2 <- subset(groupB, StudyID >= 700)

# sample 1: clear
mu_1c <- mean(groupA$`Clear Bottle mL`)
sd_1c <- sd(groupA$`Clear Bottle mL`)
mu_2c <- mean(groupB$`Clear Bottle mL`)
sd_2c <- sd(groupB$`Clear Bottle mL`)
mu_dc <- mu_1c - mu_2c

# Simulate data 
n = 100
clear_sample <- data.frame(visit1 = rnorm(n,mu_1c,sd_1c), visit2 = rnorm(n,mu_2c,sd_2c))
clear_sample$diff <- clear_sample$visit1 - clear_sample$visit2

# sample 2: opaque
mu_1o <- mean(groupB$`Opaque Bottle mL`)
sd_1o <- sd(groupB$`Opaque Bottle mL`)
mu_2o <- mean(groupA$`Opaque Bottle mL`)
sd_2o <- sd(groupA$`Opaque Bottle mL`)
mu_do <- mu_1o - mu_2o

# Simulate data
n = 100
opaque_sample <- data.frame(visit1 = rnorm(n,mu_1o,sd_1o), visit2 = rnorm(n,mu_2o,sd_2o))
opaque_sample$diff <- opaque_sample$visit1 - opaque_sample$visit2

boxplot(clear_sample$diff, opaque_sample$diff, names = c("Clear Differences","Opaque Differences"), 
        main = "Comparing Differences of 2 Samples", col = c(2,4))

t.test(clear_sample$diff, opaque_sample$diff, alternative = "two.sided")

# is there a way to do a true false arguement if we need pooled sd or not?
# Power Simulation for 2 sample
# need to incorporate r into the function, but not sure how to with sd_pooled??? 
power_2samptest <- function(n1, n2, s1a, s1b, s2a, s2b, r1, r2, delta1, delta2, pooled)
{
  sd1_diff <- sqrt(s1a^2 + s1b^2 - 2*r1*s1a*s1b)
  sd2_diff <- sqrt(s2a^2 + s2b^2 - 2*r2*s2a*s2b)
  if(pooled == TRUE)
  {
    std_error <- (sqrt(((n1 - 1)*sd1_diff^2 + (n2 - 1)*sd2_diff^2)/(n1 + n2 - 2)))*sqrt(1/n1 + 1/n2)
  }
  else
  {
    std_error <- sqrt(sd1_diff^2/n1 + sd2_diff^2/n2)
  }

  df <- n1 + n2 - 2
  mu0 <- 0 
  reps <- 10000
  outsideCI <- numeric(reps)
  set.seed(2)
  for (i in 1:reps) {
    x <- rnorm(n1, delta1, sd1_diff)
    y <- rnorm(n2, delta2, sd2_diff)
    CI.lower <- (mean(x)-mean(y)) - qt(0.975, df)*std_error
    CI.upper <- (mean(x)-mean(y)) + qt(0.975, df)*std_error
    outsideCI[i] <- ifelse(mu0 < CI.lower | mu0 > CI.upper, 1, 0)
  }
  return(mean(outsideCI))
}
# returns power

# n1, n2, s1a, s1b, s2a, s2b, r1, r2, delta1, delta2, pooled
power_2samptest(30,30,5,7,5,7,.5,.5,40,35,FALSE)

# to check 
power.t.test(n=30, delta = 5, sd = 6.244998, type = "two.sample")
##

# Example 1 - same sample size, correlation, sd, diff mean difference

n <- 35
r <- seq(0.5,0.9,by=0.025)
diff1 <- 20
sd_a <- 8
sd_b <- 8
k = length(r)
power_2samp <- data.frame(n1 = rep(NA,k), n2 = rep(NA,k), r1 = rep(NA,k), r2 = rep(NA,k),
                          diff1 = rep(NA,k), diff2 = rep(NA,k), totaldiff = rep(NA,k), 
                          s1 = rep(NA,k), s2 = rep(NA,k), power = rep(NA,k))
index <- 0
for(i in 1:length(r)) {
          index <- index + 1
          power_2samp[index,1] <- n
          power_2samp[index,2] <- n
          power_2samp[index,3] <- r[i]
          power_2samp[index,4] <- r[i]
          power_2samp[index,5] <- diff1
          power_2samp[index,6] <- diff1 + 4
          power_2samp[index,7] <- 4
          power_2samp[index,8] <- sqrt(sd_a^2 + sd_b^2 - 2*r[i]*sd_a*sd_b)
          power_2samp[index,9] <- sqrt(sd_a^2 + sd_b^2 - 2*r[i]*sd_a*sd_b)
          power_2samp[index,10] <- power_2samptest(n, n, sd_a, sd_b, 
                                                   sd_a, sd_b, r[i], r[i],
                                                   diff1, diff1 + 4, TRUE)
          # n1, n2, s1a, s1b, s2a, s2b, r1, r2, delta1, delta2, pooled
}
power_2samp

ggplot(power_2samp, aes(x = r1, y = power, col = "red")) + geom_line() + labs(title = "Power vs. Correlation", x = "Correlation", y = "Power") + theme(legend.position="none") + theme(plot.title = element_text(hjust = 0.5))  

# Example 2 - same sample size, sd, diff correlation

n <- 35
r1 <- seq(0.525,0.9,by=0.025)
r2 <- 0.5
diff1 <- 20
sd_a <- 8
sd_b <- 8
k = length(r1)*length(r2)
power_2samp <- data.frame(n1 = rep(NA,k), n2 = rep(NA,k), r1 = rep(NA,k), r2 = rep(NA,k),
                          diff1 = rep(NA,k), diff2 = rep(NA,k), totaldiff = rep(NA,k), 
                          s1 = rep(NA,k), s2 = rep(NA,k), power = rep(NA,k))
index <- 0
for(i in 1:length(r1)) {
  for(j in 1:length(r2)){
  index <- index + 1
  power_2samp[index,1] <- n
  power_2samp[index,2] <- n
  power_2samp[index,3] <- r1[i]
  power_2samp[index,4] <- r2[j] 
  power_2samp[index,5] <- diff1
  power_2samp[index,6] <- diff1 + 4
  power_2samp[index,7] <- 4
  power_2samp[index,8] <- sqrt(sd_a^2 + sd_b^2 - 2*r1[i]*sd_a*sd_b)
  power_2samp[index,9] <- sqrt(sd_a^2 + sd_b^2 - 2*r2[j]*sd_a*sd_b)
  power_2samp[index,10] <- power_2samptest(n, n, sd_a, sd_b, 
                                           sd_a, sd_b, r1[i], r2[j],
                                           diff1, diff1 + 4, TRUE)
  # n1, n2, s1a, s1b, s2a, s2b, r1, r2, delta1, delta2, pooled
  }
}  
power_2samp
ggplot(power_2samp, aes(x = r1-r2, y = power, col = "red")) + geom_line() + labs(title = "Power vs. Difference Between Correlations of Two Groups", x = "Difference between CorrelationS", y = "Power") + theme(legend.position="none") + theme(plot.title = element_text(hjust = 0.5))  


# Example 3 - same sample size, correlation, diff sd

n <- c(35, 40)
r <- seq(0.5,0.9,by=0.05)
diff1 <- 10
sd_a <- 40
sd_b <- 35
k = length(r)*length(n)*length(diff1)*length(sd_a)*length(sd_b)
power_2samp <- data.frame(n1 = rep(NA,k), n2 = rep(NA,k), r1 = rep(NA,k), r2 = rep(NA,k),
                          diff1 = rep(NA,k), diff2 = rep(NA,k),  
                          s1 = rep(NA,k), s2 = rep(NA,k), power = rep(NA,k))
index <- 0
for(f in 1:length(n)) {
  for(g in 1:length(r)) {
    for(h in 1:length(diff1)){
      for(i in 1:length(sd_a)) {
        for(j in 1:length(sd_b)){
          index <- index + 1
          power_2samp[index,1] <- n[f]
          power_2samp[index,2] <- n[f]
          power_2samp[index,3] <- r[g]
          power_2samp[index,4] <- r[g]
          power_2samp[index,5] <- diff1[h]
          power_2samp[index,6] <- diff1[h]
          power_2samp[index,7] <- sqrt(sd_a[i]^2 + sd_b[j]^2 - 2*r[g]*sd_a[i]*sd_b[j])
          power_2samp[index,8] <- sqrt((sd_a[i]-5)^2 + sd_b[j]^2 - 2*r[g]*sd_a[i]*sd_b[j])
          power_2samp[index,9] <- power_2samptest(n[f], n[f], sd_a[i], sd_b[j], 
                                                   sd_a[i], sd_b[j], r[g], r[g],
                                                   diff1[h], diff1[h], TRUE)
          # n1, n2, s1a, s1b, s2a, s2b, r1, r2, delta1, delta2, pooled
        }
      }
    }
  } 
}
power_2samp

#
# power_2samptest <- function(n1, n2, s1a, s1b, s2a, s2b, r1, r2, delta1, delta2, pooled)
  
r <- seq(from = .2, to = .9, by = .1)
power_vals <- rep(NA,length(r))
for(i in 1:length(r))
{
  power_vals[i] <- power_2samptest(n1 = 40, n2 = 40, s1a = 5, s1b = 6, s2a = 5, s2b = 6, r1 = r[i], 
                        r2 = r[i], delta1 = 1, delta2 = 7, pooled = FALSE)
}
power_matrix_2samp <- cbind(r, power_vals)
power_matrix_2samp


