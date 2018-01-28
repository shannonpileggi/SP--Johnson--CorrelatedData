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
sd(groupA$`Clear Bottle mL`)
sd(groupA$`Opaque Bottle mL`)
cor(groupA$`Clear Bottle mL`, groupA$`Opaque Bottle mL`)
# mean difference = 8.879838
# mean clear bottle = 98.37078 mL
# mean opaque bottle = 89.49094 mL
# sd clear bottle = 53.70171 mL
# sd opaque bottle = 44.66042 mL
# correlation = 0.5435
t.test(groupA$`Clear Bottle mL`, groupA$`Opaque Bottle mL`, paired = TRUE)

# Group B: opaue bottle on 1st visit, clear bottle on 2nd visit
groupB <- subset(data_sub, type == 'B')
groupB_clear <- subset(groupB, bottype == 1)
groupB_opaque <- subset(groupB, bottype == 2)
groupB <- cbind(groupB_clear, groupB_opaque$tfamtml)
groupB <- subset(groupB[,c(1,4,6)])
names(groupB) <- c("Study ID", "Clear Bottle mL", "Opaque Bottle mL")
groupB$Difference <- groupB$`Clear Bottle mL` - groupB$`Opaque Bottle mL`
mean(groupB$Difference)
mean(groupB$`Clear Bottle mL`)
mean(groupB$`Opaque Bottle mL`)
sd(groupB$`Clear Bottle mL`)
sd(groupB$`Opaque Bottle mL`)
cor(groupB$`Clear Bottle mL`, groupB$`Opaque Bottle mL`)
# mean difference = 6.974264
# mean clear bottle = 109.7927
# mean opaque bottle = 102.8184
# sd clear bottle = 49.44655
# sd opaque bottle = 44.46599
# correlation = 0.5276
t.test(groupB$`Clear Bottle mL`, groupB$`Opaque Bottle mL`, paired = TRUE)

