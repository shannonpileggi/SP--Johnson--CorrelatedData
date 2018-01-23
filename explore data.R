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

for(i in 1:nrow(data_sub)){
  
  if((data_sub$VISIT[i] == 1 & data_sub$bottype[i] == 1) | 
     (data_sub$VISIT[i] == 2 & data_sub$bottype[i] == 2)){
    data_sub$type[i] == 'A'
  } else {
    data_sub$type[i] == 'B'
  }
  
}

data_sub

if(identical(data_sub$VISIT[1], 1)) {
  print ("YES")
} else {
  print ("NO")
}
