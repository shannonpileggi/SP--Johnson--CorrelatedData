data <- read.csv("/Users/emilyjohnson/Desktop/Stats_Senior_Project_Data.csv", header = TRUE)
library(ggplot2)
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
mu_dc <- clear_visit2 - clear_visit1

# Simulate data 
n = 1000
clear_sample <- data.frame(visit1 = rnorm(n,mu_1c,sd_1c), visit2 = rnorm(n,mu_2c,sd_2c))
clear_sample$diff <- clear_sample$visit1 - clear_sample$visit2

# sample 2: opaque
mu_1o <- mean(groupB$`Opaque Bottle mL`)
sd_1o <- sd(groupB$`Opaque Bottle mL`)
mu_2o <- mean(groupA$`Opaque Bottle mL`)
sd_2o <- sd(groupA$`Opaque Bottle mL`)
mu_do <- opaque_visit2 - opaque_visit1

# Simulate data
n = 1000
opaque_sample <- data.frame(visit1 = rnorm(n,mu_1o,sd_1o), visit2 = rnorm(n,mu_2o,sd_2o))
opaque_sample$diff <- opaque_sample$visit1 - opaque_sample$visit2

boxplot(clear_sample$diff, opaque_sample$diff, names = c("Clear Differences","Opaque Differences"), 
        main = "Comparing Differences of 2 Samples", col = c(2,4))

t.test(clear_sample$diff, opaque_sample$diff, alternative = "two.sided")
