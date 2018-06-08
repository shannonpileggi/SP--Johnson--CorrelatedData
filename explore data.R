data <- read.csv("/Users/emilyjohnson/Desktop/Senior Project/Stats_Senior_Project_Data.csv", header = TRUE)
head(data)
data_sub <- data[c(1:3,19)]
head(data_sub)
tapply(data_sub$tfamtml, as.factor(data_sub$bottype), mean, na.rm = TRUE)
## botttype = 1 --> clear bottle
## botttype = 2 --> opaque bottle
## mean for clear bottle = 105.44579 mL
## mean for opaque bottle = 96.24353 mL
tapply(data_sub$tfamtml, as.factor(data_sub$bottype), sd, na.rm = TRUE)
## sd for clear bottle = 52.43197 mL
## sd for opaque bottle = 44.76531 mL

data_sub <- data.frame(data_sub)
data_sub <- na.omit(data_sub)
data_sub <- data_sub[-109,]

# Dividing data in Group A and Group B 
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
names(groupA) <- c("Study ID", "Clear Bottle mL", "Opaque Bottle mL")
groupA$Difference <- groupA$`Clear Bottle mL` - groupA$`Opaque Bottle mL`
mean(groupA$`Clear Bottle mL`)
mean(groupA$`Opaque Bottle mL`)
median(groupA$`Clear Bottle mL`)
median(groupA$`Opaque Bottle mL`)
mean(groupA$Difference)
sd(groupA$Difference)
sd(groupA$`Clear Bottle mL`)
sd(groupA$`Opaque Bottle mL`)
plot(groupA$`Clear Bottle mL`, groupA$`Opaque Bottle mL`, 
     xlab = "Clear Bottle mL", ylab = "Opaque Bottle mL", main = "Group A", pch = 20)
abline(lm(groupA$`Clear Bottle mL`~groupA$`Opaque Bottle mL`), col = "red")
cor(groupA$`Clear Bottle mL`, groupA$`Opaque Bottle mL`)
# mean difference = 8.879838
# sd difference = 47.65847
# mean clear bottle = 98.37078 mL
# mean opaque bottle = 89.49094 mL
# sd clear bottle = 53.70171 mL
# sd opaque bottle = 44.66042 mL
# correlation = 0.5435
groupASummary <- cbind(rbind(mean(groupA$`Clear Bottle mL`),sd(groupA$`Clear Bottle mL`)),rbind(mean(groupA$`Opaque Bottle mL`), sd(groupA$`Opaque Bottle mL`)),rbind(mean(groupA$Difference),sd(groupA$Difference)))
rownames(groupASummary) <- c("Mean", "Std. Dev")
colnames(groupASummary) <- c("Clear Bottle", "Opaque Bottle", "Difference")
t.test(groupA$`Clear Bottle mL`, groupA$`Opaque Bottle mL`, paired = TRUE)
length(groupA$`Study ID`)
power.t.test(n = 37, delta = 8.88, sd = 47.66, type = "paired")

# Determining necesary sample size for specific powers
powers <- seq(from = .7, to = .9, by = .05)
samplesizes <- rep(NA,length(powers))
for(i in 1:length(powers))
{
  power <- power.t.test(delta = mean(groupA$Difference), sd = sd(groupA$Difference), 
                        power = powers[i], type = "paired")
  samplesizes[i] <- power$n  
}
power_matrixA <- cbind(powers, samplesizes)
power_matrixA

 
# Group B: opaue bottle on 1st visit, clear bottle on 2nd visit
groupB <- subset(data_sub, type == 'B')
groupB_clear <- subset(groupB, bottype == 1)
groupB_opaque <- subset(groupB, bottype == 2)
groupB <- cbind(groupB_clear, groupB_opaque$tfamtml)
groupB <- subset(groupB[,c(1,4,6)])
names(groupB) <- c("Study ID", "Clear Bottle mL", "Opaque Bottle mL")
groupB$Difference <- groupB$`Clear Bottle mL` - groupB$`Opaque Bottle mL`
groupBSummary <- cbind(rbind(mean(groupB$`Clear Bottle mL`),sd(groupB$`Clear Bottle mL`)),rbind(mean(groupB$`Opaque Bottle mL`), sd(groupB$`Opaque Bottle mL`)),rbind(mean(groupB$Difference),sd(groupB$Difference)))
rownames(groupBSummary) <- c("Mean", "Std. Dev")
colnames(groupBSummary) <- c("Clear Bottle", "Opaque Bottle", "Difference")
groupBSummary
mean(groupB$Difference)
sd(groupB$Difference)
mean(groupB$`Clear Bottle mL`)
mean(groupB$`Opaque Bottle mL`)
sd(groupB$`Clear Bottle mL`)
sd(groupB$`Opaque Bottle mL`)
plot(groupB$`Clear Bottle mL`, groupB$`Opaque Bottle mL`, 
     xlab = "Clear Bottle mL", ylab = "Opaque Bottle mL", main = "Group B", pch = 20)
abline(lm(groupB$`Clear Bottle mL`~groupB$`Opaque Bottle mL`), col = "red")
cor(groupB$`Clear Bottle mL`, groupB$`Opaque Bottle mL`)
# mean difference = 6.974264
# sd difference = 45.84868
# mean clear bottle = 109.7927
# mean opaque bottle = 102.8184
# sd clear bottle = 49.44655
# sd opaque bottle = 44.46599
# correlation = 0.5276
# Ho: There is no difference in mean intake (mL) between using a clear feeding bottle and an opaque feeding bottle.
power.t.test(n = 38, delta = 6.97, sd = 45.85, type = "paired")
t.test(groupB$`Clear Bottle mL`, groupB$`Opaque Bottle mL`, paired = TRUE)

# Determining necesary sample size for specific powers
powers <- seq(from = .7, to = .9, by = .05)
samplesizes <- rep(NA,length(powers))
for(i in 1:length(powers))
{
  power <- power.t.test(delta = mean(groupB$Difference), sd = sd(groupB$Difference), power = powers[i], 
                        type = "paired")
  samplesizes[i] <- power$n  
}
power_matrixB <- cbind(powers, samplesizes)
power_matrixB

#Power Simulation 
n <- 456
mu <- 6.974264
sigma <- 45.84868
mu0 <- 0
reps <- 10000
outsideCI <- numeric(reps)
set.seed(2)
for (i in 1:reps) {
  x <- rnorm(n, mu, sigma)
  CI.lower <- mean(x) - qt(0.975, n-1)*sd(x)/sqrt(n)
  CI.upper <- mean(x) + qt(0.975, n-1)*sd(x)/sqrt(n)
  outsideCI[i] <- ifelse(mu0 < CI.lower | mu0 > CI.upper, 1, 0)
}
mean(outsideCI)
##

# Power Function #
power_func <- function(r, n, s1, s2, delta)
{
  sd_diff <- sqrt(s1^2 + s2^2 - 2*r*s1*s2)
  power <- power.t.test(n, delta = delta, sd = sd_diff, type = "paired")
  return(power$power)
}


## Sample size function
# alpha is sign. level, beta = power
samplesize_calc <- function(alpha, beta, delta, s1, s2, r){
  sd_diff <- sqrt(s1^2 + s2^2 - 2*r*s1*s2)
  effect_size <- diff/sd_diff
  n <- ((qnorm((1-alpha/2)) + qnorm(beta))/effect_size)^2
  return(n)
}
samplesize_calc(.05, .2, mean(groupA$Difference), sd(groupA$`Clear Bottle mL`),
                sd(groupA$`Opaque Bottle mL`), cor(groupA$`Clear Bottle mL`, groupA$`Opaque Bottle mL`))

r <- seq(.1, .95, by = .05)
samplesizes <- rep(NA,length(r))
for(i in 1:length(r)){
  samplesizes[i] <- samplesize_calc(.05, .8, mean(groupA$Difference), sd(groupA$`Clear Bottle mL`),
      sd(groupA$`Opaque Bottle mL`), r[i])
  
}
matrix <- cbind(r, samplesizes)
matrix <- data.frame(matrix)
ggplot(matrix, aes(x = r, y = samplesizes, col = "blue")) + geom_line() + labs(title = "Sample Size vs. Correlation", x = "Correlation", y = "Sample Size") + theme(legend.position="none") + theme(plot.title = element_text(hjust = 0.5))


## New simulation group A
r <- seq(0.1,0.95,by=0.1)
n <- 37
delta <- mean(groupA$Difference)
s1 <- sd(groupA$`Clear Bottle mL`)
s2<- sd(groupA$`Opaque Bottle mL`)
k = length(r)
powersA <- data.frame(power = rep(NA,k), r = rep(NA,k))
index <- 0
for(i in 1:length(r)) {
    index <- index + 1
    powersA[index,1] <- power_func(r[i], n, s1, s2, delta)
    powersA[index,2] <- r[i]
}
powersA

## New simulation group B
r <- seq(0.1,0.95,by=0.1)
n <- 38
delta <- mean(groupB$Difference)
s1 <- sd(groupB$`Clear Bottle mL`)
s2<- sd(groupB$`Opaque Bottle mL`)
k = length(r)
powersB <- data.frame(power = rep(NA,k), r = rep(NA,k))
index <- 0
for(i in 1:length(r)) {
  index <- index + 1
  powersB[index,1] <- power_func(r[i], n, s1, s2, delta)
  powersB[index,2] <- r[i]
}
powersB


## Graph 1
r <- seq(0.1,0.95,by=0.05)
n <- 37
delta <- 8.879838
s1 <- 53.70171
s2<- 44.66042
k = length(r)
power_df <- data.frame(n = rep(NA,k), r = rep(NA,k), s1 = rep(NA,k), 
                       s2 = rep(NA,k), delta = rep(NA,k), power = rep(NA,k))
index <- 0
for(i in 1:length(r)) {
      index <- index + 1
      power_df[index,1] <- n
      power_df[index,2] <- r[i]
      power_df[index,3] <- s1
      power_df[index,4] = s2
      power_df[index,5] = delta
      power_df[index,6] <- power_func(r[i], n, s1, s2, delta)
    }
power_df

ggplot(power_df, aes(x = r, y = power, col = "red")) + geom_line() + labs(title = "Power vs. Correlation", x = "Correlation", y = "Power") + theme(legend.position="none") + theme(plot.title = element_text(hjust = 0.5))


## Graph 2
r <- seq(0.1,0.95,by=0.05)
n <- c(20, 30, 40, 50)
delta <- 8.879838
s1 <- 53.70171
s2<- 44.66042
k = length(r)*length(n)
power_df <- data.frame(n = rep(NA,k), r = rep(NA,k), s1 = rep(NA,k), 
                       s2 = rep(NA,k), delta = rep(NA,k), power = rep(NA,k))
index <- 0
for(i in 1:length(r)) {
  for(j in 1:length(n)){
  index <- index + 1
  power_df[index,1] <- n[j]
  power_df[index,2] <- r[i]
  power_df[index,3] <- s1
  power_df[index,4] = s2
  power_df[index,5] = delta
  power_df[index,6] <- power_func(r[i], n[j], s1, s2, delta)
  }
}  
power_df

ggplot(power_df, aes(x = r, y = power, col = as.factor(n))) + geom_line() + labs(title = "Power vs. Correlation", x = "Correlation", y = "Power") + labs(col = "Sample Size") + theme(plot.title = element_text(hjust = 0.5))

## Graph 3
r <- seq(0.1,0.95,by=0.05)
n <- c(20, 30, 40, 50)
delta <- c(10, 20, 30, 40)
s1 <- 53.70171
s2<- 44.66042
k = length(r)*length(n)*length(delta)
power_df <- data.frame(n = rep(NA,k), r = rep(NA,k), s1 = rep(NA,k), 
                       s2 = rep(NA,k), delta = rep(NA,k), power = rep(NA,k))
index <- 0
for(i in 1:length(r)) {
  for(j in 1:length(n)){
    for(h in 1:length(delta)){
    index <- index + 1
    power_df[index,1] <- n[j]
    power_df[index,2] <- r[i]
    power_df[index,3] <- s1
    power_df[index,4] = s2
    power_df[index,5] = delta[h]
    power_df[index,6] <- power_func(r[i], n[j], s1, s2, delta[h])
    }
  }  
}  
power_df

ggplot(power_df, aes(x = r, y = power, col = as.factor(n))) + geom_line() + facet_wrap( ~ delta, ncol = 2, labeller = label_both) + labs(title = "Power vs. Correlation", x = "Correlation", y = "Power") + labs(col = "Sample Size") + theme(plot.title = element_text(hjust = 0.5))



# Power using given data 
# Group A - Clear Bottle First, Opaque Bottle Second
power_func(.5435, 37, 53.70171, 44.66042, 8.879838)
# .1958054
power_func(.5435, 100, 53.70171, 44.66042, 8.879838)
# If we increase sample size to 100, assuming everything else stays the same, power 
# increases to 0.4542762
power_func(0.815, 37, 53.70171, 44.66042, 8.879838)
# If we keep sample size the same, but increase the correlation between visit 1 and
# visit 2 to 0.8, then power increases to 0.3704901
power_func(0.951, 37, 53.70171, 44.66042, 8.879838)

# Group B - Opaque Bottle First, Clear Bottle Second
power_func(0.5276, 38, 49.44655, 44.46599, -6.974264)
# .1476795
power_func(0.7914, 38, 49.44655, 44.46599, -6.974264)

