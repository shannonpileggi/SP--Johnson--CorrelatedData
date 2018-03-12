data <- read.csv("/Users/emilyjohnson/Desktop/Stats_Senior_Project_Data.csv", header = TRUE)
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
names(groupA) <- c("Study ID", "Clear Bottle mL", "Opaque Bottle mL")
groupA$Difference <- groupA$`Clear Bottle mL` - groupA$`Opaque Bottle mL`
mean(groupA$`Clear Bottle mL`)
mean(groupA$`Opaque Bottle mL`)
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
t.test(groupA$`Clear Bottle mL`, groupA$`Opaque Bottle mL`, paired = TRUE)
powers <- seq(from = .7, to = .9, by = .02)
samplesizes <- rep(NA,length(powers))
for(i in 1:length(powers))
{
  power <- power.t.test(delta = mean(groupA$Difference), sd = sd(groupA$Difference), power = powers[i], type = "one.sample")
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
t.test(groupB$`Clear Bottle mL`, groupB$`Opaque Bottle mL`, paired = TRUE)
powers <- seq(from = .7, to = .9, by = .02)
samplesizes <- rep(NA,length(powers))
for(i in 1:length(powers))
{
  power <- power.t.test(delta = mean(groupB$Difference), sd = sd(groupB$Difference), power = powers[i], 
                        type = "one.sample")
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

r <- seq(0.1,0.9,by=0.05)
n <- c(35,40,45)
delta <- c(5, 10, 15, 20)
s1 <- c(35, 40, 45, 50)
k = length(r)*length(n)*length(s1)
power_df <- data.frame(n = rep(NA,k), r = rep(NA,k), s1 = rep(NA,k), power = rep(NA,k))
index <- 0
for(i in 1:length(n)) {
  for(j in 1:length(r)) {
    for(m in 1:length(s1)) {
      index <- index + 1
      power_df[index,1] <- n[i]
      power_df[index,2] <- r[j]
      power_df[index,3] <- s1[m]
      power_df[index,4] <- power_func(r[j], n[i], s1[m], 
                                      .75*s1[m], delta[2])
      }
    } 
  }
power_df
# sd(groupB$`Clear Bottle mL`) # 49.44655
# sd(groupB$`Opaque Bottle mL`) # 44.46599
# mean(groupB$Difference) # 6.974264
ggplot(power_df, aes(x = r, y = power, col = as.factor(n))) + geom_line() + facet_wrap( ~ s1, ncol = 2) + xlab("Correlation") + ylab("Power")  + labs(col = "Sample Size")




