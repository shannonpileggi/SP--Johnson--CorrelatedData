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

# Power Function #
power_func <- function(r, n, s1, s2, delta)
{
  sd_diff <- sqrt(s1^2 + s2^2 - 2*r*s1*s2)
  power <- power.t.test(n, delta = delta, sd = sd_diff, type = "paired")
  return(power$power)
}

n <- c(35,40,45)
r <- seq(0.5,0.9,by=0.05)
diff1 <- c(10, 15, 20)
s <- c(35, 40, 45)
k = length(r)*length(n)*length(s)*length(diff1)
power_2samp <- data.frame(n = rep(NA,k), r = rep(NA,k), diff1 = rep(NA,k), diff2 = rep(NA,k), 
                       totaldiff = rep(NA,k), s = rep(NA,k), power = rep(NA,k))
index <- 0
for(f in 1:length(n)) {
  for(g in 1:length(r)) {
    for(h in 1:length(diff1)){
        for(j in 1:length(s)) {
      index <- index + 1
      power_2samp[index,1] <- n[f]
      power_2samp[index,2] <- r[g]
      power_2samp[index,3] <- diff1[h]
      power_2samp[index,4] <- 1.5*diff1[h]
      power_2samp[index,5] <- diff1[h] - 1.5*diff1[h]
      power_2samp[index,6] <- s[j]
      power_2samp[index,7] <- power_func(r[g], n[f], s[j], 
                                      s[j], diff1[h] - 1.5*diff1[h])
        
      }
    }
  } 
}
power_2samp

ggplot(power_2samp, aes(x = r, y = power, col = as.factor(s))) + geom_line() + facet_wrap( ~ totaldiff, ncol = 3, labeller = label_both) + xlab("Correlation") + ylab("Power")  + labs(col = "Sample Size")
