## Reading the Auto Dataset
Auto <- read.table("Auto.data.txt", header = TRUE, na.strings = "?")
dim(Auto)

## Removing the rows containing missing values
Auto <- na.omit(Auto)
dim(Auto)

high <- rep(0, ncol(Auto))

for(i in 1:nrow(Auto)){
  if (Auto$mpg[i] >= 23){
    high[i] = 1
  }else{
    high[i] = 0
  }
}

Auto$high = high

# Creating Dummy Variables for origin
origin2 <- ifelse(Auto$origin == 2, 1, 0)
origin3 <- ifelse(Auto$origin == 3, 1, 0)

processed_data <- as.data.frame(lapply((Auto[c('horsepower', 'weight', 'year')]), normalize))
processed_data$origin2 <- origin2
processed_data$origin3 <- origin3

X <- as.matrix(processed_data)
X <- cbind(Intercept = rep(1, nrow(Auto)), X)

y <- high

