#1.
#a.
x <- c(2,4,6,8)
y <-c(3,5,7,9)
length(x)
#b
5:15
seq(4,20)
seq(12,30,3)
seq(1.1,2,length=10)
rep(8,4)
rep(1:3,2)
#c
x<-1:10
if(any(x>8)) print('yes')
if(any(x)>88) print('yes')
if(all(x)>0) print('yes')
#d
k <-c(1.2,3.4,0.4,0.12)
k[c(1,3)]
#e
x<-c(2,4,6,8)
x+y
#f
mean(x)
sd(x)
#g
hist(k)
plot(k)
#h
vec = c(3,5,7,5,3,2,6) #Creating vector
cumsum(vec)
cummax(vec)
cummin(vec)
cumprod(vec)
#i
quiz <- data.frame("q1" = c(0, 0, 0, 0, 1),
                   "q2" = c(0, 1, 1, 0, 1),
                   "q3" = c(0, 0, 0, 1, 1),
                   "q4" = c(1, 1, 1, 1, 1),
                   "q5" = c(1, 0, 1, 0, 1))
rowMeans(quiz)
rowSums(quiz)
colMeans(quiz)
colSums(quiz)
#j
apply(quiz, 1, mean, na.rm = TRUE)


#2.)
#a
int_vec <- c(1,2,3)
char_vec <- c("a", "b", "c")
bool_vec <- c(TRUE, TRUE, FALSE)
data_frame <- data.frame(int_vec, char_vec, bool_vec)
#b
employee_data <- data.frame(
  employee_id = c(1:5),
  employee_name = c("James","Harry","Shinji","Jim","Oliver"),
  sal = c(642.3,535.2,681.0,739.0,925.26),
  join_date = as.Date(c("2013-02-04", "2017-06-21", "2012-11-14", "2018-05-
                        19","2016-03-25")), stringsAsFactors = FALSE)
#c
str(employee_data)
#d
output <- data.frame(employee_data$employee_name,employee_data$employee_id)
print(output)
#e
output <- employee_data[1:2,]
print(output)
#f
result <- employee_data[1:2,3:4]
result
#g
employee_data$dept <- c("IT","Finance","Operations","HR","Administration")
out <- employee_data
print(out)
#h
employee_new_data <- data.frame(
  employee_id = c (6:8),
  employee_name = c("Aman", "Piyush", "Aakash"),
  sal = c(523.0,721.3,622.8),
  join_date = as.Date(c("2015-06-22","2016-04-30","2011-03-17")),
  dept=c("IT","Finance","Operations"),
  stringsAsFactors = FALSE)
employee_out_data <- rbind(employee_data,employee_new_data)
employee_out_data
#i
x <- data.frame("SN" = 1:2, "Age" = c(21,15), "Name" = c("John","Dora"))
str(x)
x$State <- NULL
x
#j
id <- c(1,2,3)
name <- c("John", "Kirk", "AJ")
age <- c(21,27,18)
employees <- data.frame(ID=id, Name=name, Age=age)
employees

employees[3,"Age"] <- 29
employees[order(employees$Age),]
employees[order(employees$Age, decreasing=T),]
aggregate(employees[,2], list(Age=employees$Age), FUN=length)

#k
x = c(4,3,4,5,2,3,4,5)
y = c(4,4,5,5,4,5,4,4)
z = c(3,4,2,4,5,5,4,4)
scores = data.frame(x,y,z)
boxplot(scores)

#3
#a
dataset1<-data.frame(Make=c("Honda","BMW"),Num_Models=c(63,10))
dataset2<-data.frame(Make=c("Ford","Tesla"),Num_Models=c(26,4))
reordered_dataset1 <- rbind(dataset1, dataset2)
reordered_dataset2<- cbind(dataset1, dataset2)
reordered_dataset1
reordered_dataset2

#b
data<-read.table("D:/web/Book2.txt",header=TRUE)
head(data,6)
anova(lm(Weight~trtmt+block,data))


#4
#a
recursive.factorial <- function(x) { 
  if (x == 0) return (1)
  else return (x * recursive.factorial(x-1))
}
recursive.factorial(0) 
recursive.factorial(5) 
recursive.factorial(7)

#B
for (i in 1:100)
{
  print(i)
}
i<-0
while (i <=100)
{
  print(i) 
  i=i+1
}
#c

convert_to_binary <- function(n) {
  
  if(n > 1) {
    convert_to_binary(as.integer(n/2))
  }
  cat(n %% 2)
}

convert_to_binary(43)

#d
recurse_fibonacci <- function(n) {
  if(n <= 1) {
    return(n)
  } else {
    return(recurse_fibonacci(n-1) + recurse_fibonacci(n-2))
  }
}

nterms =7
if(nterms <= 0) {
  print("Plese enter a positive integer")
} else {
  print("Fibonacci sequence:")
  for(i in 0:(nterms-1)) {
    print(recurse_fibonacci(i))
  }
}

#e
calculate_sum <- function(n) {
  if(n <= 1) {
    return(n)
  } else {
    return(n + calculate_sum(n-1))
  }
}
calculate_sum(5)

#f 
Sum.Series <- function(number)
{
  if(number == 0) {
    return (0)
  } else {
    return ((number * number ) + Sum.Series(number - 1))
  }
}
Sum.Series(3)

#5
mergesort <- function(m)
{
  merge_ <- function(left, right)
  {
    result <- c()
    while(length(left) > 0 && length(right) > 0)
    {
      if(left[1] <= right[1])
      {
        result <- c(result, left[1])
        left <- left[-1]
      } else
      {
        result <- c(result, right[1])
        right <- right[-1]
      }
    }
    if(length(left) > 0) result <- c(result, left)
    if(length(right) > 0) result <- c(result, right)
    result
  }
  len <- length(m)
  if(len <= 1) m 
  else
  {
    middle <- length(m) / 2
    left <- m[1:floor(middle)]
    right <- m[floor(middle+1):len]
    left <- mergesort(left)
    right <- mergesort(right)
    merge_(left, right)

  }
}
mergesort(c(4, 65, 2, -31, 0, 99, 83, 782, 1))



#6
#a
str(airquality)
boxplot(airquality$Ozone)
boxplot(airquality$Ozone,
        main = "Mean ozone in parts per billion at Roosevelt Island",
        xlab = "Parts Per Billion",
        ylab = "Ozone",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)
b <- boxplot(airquality$Ozone)
barplot(airquality$Ozone)
b
#b
ozone <- airquality$Ozone
temp <- airquality$Temp
# gererate normal distribution with same mean and sd
ozone_norm <- rnorm(200,mean=mean(ozone, na.rm=TRUE), sd=sd(ozone,
                                                            na.rm=TRUE))
temp_norm <- rnorm(200,mean=mean(temp, na.rm=TRUE), sd=sd(temp,
                                                          na.rm=TRUE))
boxplot(ozone, ozone_norm, temp, temp_norm,
        main = "Multiple boxplots for comparision",
        names = c("ozone", "normal", "temp", "normal"),
        col = c("orange","red")
)
#c
boxplot(Temp~Month,
        data=airquality,
        main="Different boxplots for each month",
        xlab="Month Number",
        ylab="Degree Fahrenheit",
        col="orange",
        border="brown"
)
#d
age <- c(17,18,18,17,18,19,18,16,18,18)
barplot(table(age),
        main="Age Count of 10 Students",
        xlab="Age",
        ylab="Count"
)



#7
#1

sex<-c("Female","Female","Male","Male")
time<-c("Lunch","Dinner","Lunch","Dinner")
total_bill<-c(13.53,16.81,16.24,17.42)
restaurant<-data.frame(sex,time,total_bill)
df1 <- restaurant
head(df1)

ggplot(data=df1, aes(x=time, y=total_bill,fill=sex))+geom_bar(stat="identity")


# Use position=position_dodge()

p<-ggplot(data=df1, aes(x=time, y=total_bill,fill=sex))+geom_bar(stat="identity",color="black",position=position_dodge())
p

#2
temp <- c(25,30,22,24,27,29,23)
barplot(temp,
        main = "Maximum Temperatures in a Week",
        ylab = "Degree Celsius",xlab = "Day",
        names.arg = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),
        col = "darkred")

#3
#box
box <- ggplot(data=iris, aes(x=Species, y=Sepal.Length))
box + geom_boxplot(aes(fill=Species))
#scat
scatter <- ggplot(data=iris, aes(x = Sepal.Length, y = Sepal.Width)) 
scatter + geom_point(aes(color=Species, shape=Species))+ggtitle("Scatterplot of iris")

#4
list_data <- list(c("Jan","Feb","Mar"), matrix(c(3,9,5,1,-2,8), nrow = 2),
                  list("green",12.3))
head(list_data)
tail(list_data)
list_data[2]
list_data[c(2, 4)] 
x = list(c(1:5),6,7,letters[1:3],"x",LETTERS[1:5])
x         
head(x,2)
tail(x,2)
sum(x[[1]])
mean(x[[1]])
rm(list_data)

#8
#a
set.seed(23)
#Generate x as 100 integers using seq function
x<-seq(0,100,1)
#Generate y as a*e^(bx)+c
y<-runif(1,0,20)*exp(runif(1,0.005,0.075)*x)+runif(101,0,5)
#How does our data look like? Lets plot it
plot(x,y)
#Linear model
lin_mod=lm(y~x)
#Plotting the model
plot(x,y)
abline(lin_mod)
nonlin_mod=nls(y~a*exp(b*x),start=list(a=13,b=0.1)) 
plot(x,y)
lines(x,predict(nonlin_mod),col="Red")
#Error calculation
error <- lin_mod$residuals
error
lm_error <- sqrt(mean(error^2))
lm_error#5.960544
error2=y-predict(nonlin_mod)
error2
nlm_error <- sqrt(mean(error2^2))
nlm_error
#B1
install.packages("deSolve")
attach(Puromycin)
plot(Puromycin$conc,Puromycin$rate)
library(deSolve)
log_growth<- function(Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    dN <- R*N*(1-N/K)
    return(list(c(dN)))
  })
}
pars <- c(R=0.2,K=1000)
pars
N_ini <- c(N=1)
N_ini
times <- seq(0, 50, by = 1)
times
out <- ode(N_ini, times, log_growth, pars)
out
N_obs<-out[,2]+rnorm(51,0,50)
N_obs
N_obs<-ifelse(N_obs<1,1,N_obs)
N_obs
plot(times,N_obs)
#c
y<-runif(1,5,15)*exp(-runif(1,0.01,0.05)*x)+rnorm(51,0,0.5)
plot(x,y)
a_start<-8 
b_start<-2*log(2)/a_start 
m<-nls(y~a*exp(-b*x),start=list(a=a_start,b=b_start))
cor(y,predict(m))
lines(x,predict(m),col="red",lty=2,lwd=3)
#D
install.packages("pracma")
library(pracma)
deer<-read.table("D:/web/jaws.txt",header=T)
model<-nls(bone~a-b*exp(-c*age),data=deer,start=list(a=120,b=110,c=0.064))
summary(model)

#y=a(1???e???cx)
model2<-nls(bone~a*(1-exp(-c*age)),data=deer,start=list(a=120,c=0.064))
anova(model,model2)

summary(model2)
#e
install.packages("earth")
library(earth)
# load data
data(longley)
head(data)
fit <- earth(Employed~., longley)
# summarize the fit
summary(fit)
# summarize the importance of input variables
evimp(fit)
# make predictions
predictions <- predict(fit, longley)
# summarize accuracy
mse <- mean((longley$Employed - predictions)^2)
print(mse)
#9
install.packages("e1071")
library(e1071) 
par(mfrow=c(1, 2))  
sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)) 
sub
plot(cars$speed)
polygon(density(cars$speed), col="red")
plot(density(cars$dist), main="Density Plot: Distance", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$dist), 2))) 
polygon(density(cars$dist), col="red")
cor(cars$speed, cars$dist)
linearMod <- lm(dist ~ speed, data=cars) # build linear regression model on full 
data
print(linearMod)
summary(linearMod)
modelSummary <- summary(linearMod) 
modelCoeffs <- modelSummary$coefficients 
beta.estimate <- modelCoeffs["speed", "Estimate"]
std.error <- modelCoeffs["speed", "Std. Error"] 
t_value <- beta.estimate/std.error 
p_value <- 2*pt(-abs(t_value), df=nrow(cars)-ncol(cars)) 
f_statistic <- linearMod$fstatistic[1] 
# parameters for model p-value calc
f <- summary(linearMod)$fstatistic
model_p <- pf(f[1], f[2], f[3], lower=FALSE)
t_value
p_value
f_statistic
model_p

#10
data = read.csv("D:/web/house.csv") 
dataset=na.omit(data)
dataset$State = factor(dataset$State,levels = c('New York', 'California', 'Florida'), labels = c(1, 2, 3)) 
dataset$State

dataset = read.csv('D:\\3-2\\r\\house.csv') 
dataset$State = factor(dataset$State,levels = c('New York', 'California', 'Florida'), labels = c(1, 2, 3)) 
dataset$State

#11
install.packages("mlbench")
install.packages("caret")
library(caret)
library(mlbench)
data(BreastCancer, package="mlbench")
bc <- BreastCancer[complete.cases(BreastCancer), ] 
str(bc)
glm(Class ~ Cell.shape, family="binomial", data = bc)
# remove id column
bc <- bc[,-1]
# convert factors to numeric
for(i in 1:9) {
  bc[, i] <- as.numeric(as.character(bc[, i]))
}
bc$Class <- ifelse(bc$Class == "malignant", 1, 0)
bc$Class <- factor(bc$Class, levels = c(0, 1))
table(bc$Class)

'%ni%' <- Negate('%in%') 
options(scipen=999) 
# Prep Training and Test data.
set.seed(100)
trainDataIndex <- createDataPartition(bc$Class, p=0.7, list = F) # 70% 
trainData <- bc[trainDataIndex, ]
testData <- bc[-trainDataIndex, ]
table(trainData$Class)
# Down Sample
set.seed(100)
down_train <- downSample(x = trainData[, colnames(trainData) %ni% 
                                         "Class"], y = trainData$Class)
table(down_train$Class)
# Up Sample.
set.seed(100)
up_train <- upSample(x = trainData[, colnames(trainData) %ni% "Class"],
                     y = trainData$Class)
table(up_train$Class)
# Build Logistic Model
logitmod <- glm(Class ~ Cl.thickness + Cell.size + Cell.shape, family = "binomial", data=down_train)
summary(logitmod)
pred <- predict(logitmod, newdata = testData, type = "response")
y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- testData$Class
mean(y_pred == y_act)
#12
install.packages('Amelia')
install.packages("corrplot")
library(ISLR)
require(ISLR)
names(Smarket)
head(Smarket)
summary(Smarket)
for(i in 1:8) {
  hist(Smarket[,i], main=names(Smarket)[i])
}
par(mfrow=c(1,8))
for(i in 1:8) {
  boxplot(Smarket[,i], main=names(Smarket)[i])
}
library(Amelia)
library(mlbench)
missmap(Smarket, col=c("blue", "red"), legend=FALSE)
library(corrplot)
correlations <- cor(Smarket[,1:8])
corrplot(correlations, method="circle")
pairs(Smarket, col=Smarket$Direction)
library(caret)
x <- Smarket[,1:8]
y <- Smarket[,9]
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)
# Logistics Regression
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data 
               = Smarket, family = binomial)
summary(glm.fit)
#first 5 probabilities and they are very close to 50%:
glm.probs <- predict(glm.fit,type = "response")
glm.probs[1:5]
glm.pred <- ifelse(glm.probs > 0.5, "Up", "Down")
attach(Smarket)
table(glm.pred,Direction)
mean(glm.pred == Direction)
# Make training and test set
train = Year<2005
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
               data = Smarket, 
               family = binomial, 
               subset = train)
glm.probs <- predict(glm.fit, 
                     newdata = Smarket[!train,], 
                     type = "response")
glm.pred <- ifelse(glm.probs > 0.5, "Up", "Down")
#13
#a
install.packages("dplyr")
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
install.packages("ggpubr")
my_data <- ToothGrowth
set.seed(1234)
dplyr::sample_n(my_data, 10)
str(my_data)
my_data$dose <- factor(my_data$dose,
                       levels = c(0.5, 1, 2),
                       labels = c("D0.5", "D1", "D2"))
head(my_data)
table(my_data$supp, my_data$dose)
library("ggpubr")
ggline(my_data, x = "dose", y = "len", color = "supp",
       add = c("mean_se", "dotplot"),
       palette = c("#00AFBB", "#E7B800"))
library("ggpubr")
ggline(my_data, x = "dose", y = "len", color = "supp",
       add = c("mean_se", "dotplot"),
       palette = c("#00AFBB", "#E7B800"))
# Two-way ANOVA with interaction effect
# These two calls are equivalent
res.aov3 <- aov(len ~ supp * dose, data = my_data)
res.aov3 <- aov(len ~ supp + dose + supp:dose, data = my_data)
summary(res.aov3)
#b
Input = ("
         id Sex Genotype Activity
         1 male ff 1.884 
         2 male ff 2.283 
         3 male fs 2.396 
         4 female ff 2.838 
         5 male fs 2.956 
         6 female ff 4.216 
         7 female ss 3.620 
         8 female ff 2.889 
         9 female fs 3.550 
         10 male fs 3.105 
         11 female fs 4.556 12 female fs 3.087
         13 male ff 4.939 
         14 male ff 3.486 
         15 female ss 3.079 
         16 male fs 2.649 
         17 female fs 1.943 
         19 female ff 4.198 
         20 female ff 2.473 
         22 female ff 2.033 
         24 female fs 2.200 
         25 female fs 2.157 
         26 male ss 2.801 
         28 male ss 3.421
         29 female ff 1.811 
         30 female fs 4.281 
         32 female fs 4.772 
         34 female ss 3.586 
         36 female ff 3.944 
         38 female ss 2.669 
         39 female ss 3.050 
         41 male ss 4.275 
         43 female ss 2.963 
         46 female ss 3.236 
         48 female ss 3.673 
         49 male ss 3.110
         ")
Data = read.table(textConnection(Input),header=TRUE)
library(Rmisc)
sum = summarySE(Data, 
                measurevar="Activity", 
                groupvars=c("Sex","Genotype"))
sum
library(ggplot2)
pd = position_dodge(.2)
ggplot(sum, aes(x=Genotype, 
                y=Activity, 
                color=Sex)) + 
  geom_errorbar(aes(ymin=Activity-se, 
                    ymax=Activity+se), 
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(
    axis.title.y = element_text(vjust= 1.8),
    axis.title.x = element_text(vjust= -0.5),
    axis.title = element_text(face = "bold")) +
  scale_color_manual(values = c("black", "blue"))
boxplot(Activity ~ Genotype,
        data = Data,
        xlab = "Genotype",
        ylab = "MPI Activity")
boxplot(Activity ~ Genotype:Sex,
        data = Data,
        xlab = "Genotype x Sex",
        ylab = "MPI Activity")

model = lm(Activity ~ Sex + Genotype + Sex:Genotype,
           data=Data)
library(car)
Anova(model, type="III")
options(contrasts = c("contr.sum", "contr.poly"))
anova(model) # Produces type I sum of squares
summary(model) # Produces r-square, overall p-value, parameter estimates

hist(residuals(model), 
     col="darkgray")

plot(fitted(model), 
     residuals(model))
### additional model checking plots with:

plot(model)

#14
#a
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.12")
BiocManager::install("pcaMethods")
wine <- read.table("D:/web/wine.data", sep=",")
# Nae the variables
colnames(wine) <- c("Cvs","Alcohol","Malic acid","Ash","Alcalinity of ash",
                    "Magnesium", "Total phenols", "Flavanoids", "Nonflavanoid phenols",
                    "Proanthocyanins", "Color intensity", "Hue", "OD280/OD315 of diluted wines",
                    "Proline")
# The first column corresponds to the classes
wineClasses <- factor(wine$Cvs)
# Use pairs
pairs(wine[,-1], col = wineClasses, upper.panel = NULL, pch = 16, cex = 0.5)
legend("topright", bty = "n", legend = c("Cv1","Cv2","Cv3"), pch = 16, col =
         c("black","red","green"),xpd = T, cex = 2, y.intersp = 0.5)
# pairwise interactions in a set of 13 variables,
dev.off() # clear the format from the previous plot
winePCA <- prcomp(scale(wine[,-1]))
plot(winePCA$x[,1:2], col = wineClasses)
# repeat the procedure after introducing an outlier in place of the 10th observation.
wineOutlier <- wine
wineOutlier[10,] <- wineOutlier[10,]*10
# change the 10th obs. into an extreme one by multiplying its profile by 10
outlierPCA <- prcomp(scale(wineOutlier[,-1]))
plot(outlierPCA$x[,1:2], col = wineClasses)
library(pcaMethods)
winePCAmethods <- pca(wine[,-1], scale = "uv", center = T, nPcs = 2, method =
                        "svd")
slplot(winePCAmethods, scoresLoadings = c(T,T), scol = wineClasses)
#b(data.csv)
wdbc <- read.csv("D:/web/data.csv")
wdbc.pr <- prcomp(wdbc[c(3:32)], center = TRUE, scale = TRUE)
summary(wdbc.pr)
screeplot(wdbc.pr, type = "l", npcs = 15, main = "Screeplot of the first 10 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)
cumpro <- cumsum(wdbc.pr$sdev^2 / sum(wdbc.pr$sdev^2))
plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 6, col="blue", lty=5)
abline(h = 0.88759, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC6"),
       col=c("blue"), lty=5, cex=0.6)
plot(wdbc.pr$x[,1],wdbc.pr$x[,2], xlab="PC1 (44.3%)", ylab = "PC2 (19%)", main = "PC1 / PC2 - plot")

#15(train,test.csv)
install.packages("ggplot2")
install.packages("readr")
install.packages("rpart")
install.packages("rattle")
install.packages("rpart.plot")
install.packages("RColorBrewer")
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(rpart) # Loading Decision Tree Package

# Import the training set: train

train <- read.csv("D:/web/12/train.csv")

# Import the testing set: test

test <- read.csv("D:/web/12/test.csv")

# Your train and test set are still loaded in
str(train)
str(test)

# Build the decision tree
my_tree_two <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class")

# Visualize the decision tree using plot() and text()
plot(my_tree_two)
text(my_tree_two)

# Load in the packages to build a fancy plot
library(rattle)
library(rpart.plot)
library(RColorBrewer)


# Time to plot your fancy tree
fancyRpartPlot(my_tree_two)

# my_tree_two and test are available in the workspace

# Make predictions on the test set
my_prediction <- predict(my_tree_two, newdata = test, type = "class")

# Finish the data.frame() call
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)

# Use nrow() on my_solution
nrow(my_solution)
my_solution$Survived
# Finish the write.csv() call
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)



#16(car.data)
install.packages("rpart")
install.packages("caret")
install.packages("e1071")
install.packages("randomForest")
install.packages("data.table")
library(randomForest)
# Load the dataset and explore
data1 <- read.csv("D:/web/car.data", header = TRUE)
head(data1)
str(data1)
summary(data1)
set.seed(100)
train <- sample(nrow(data1), 0.7*nrow(data1), replace = FALSE)
TrainSet <- data1[train,]
ValidSet <- data1[-train,]
summary(TrainSet)
summary(ValidSet)
model1 <- randomForest(as.factor(Condition) ~ ., data = TrainSet, importance = TRUE)
model1
model2 <- randomForest(as.factor(Condition) ~ ., data = TrainSet, ntree = 500, mtry = 6, importance = TRUE)
model2
predTrain <- predict(model2, TrainSet, type = "class")
# Checking classification accuracy
table(predTrain, TrainSet$Condition)  
importance(model2)        
varImpPlot(model2) 
a=c()
i=5
for (i in 3:8) {
  model3 <- randomForest(as.factor(Condition) ~ ., data = TrainSet, ntree = 500, mtry = i, importance = TRUE)
  predValid <- predict(model3, ValidSet, type = "class")
  a[i-2] = mean(predValid == ValidSet$Condition)
}
a
plot(3:8,a)
library(rpart)
library(caret)
library(e1071)
# We will compare model 1 of Random Forest with Decision Tree model
model_dt = randomForest(as.factor(Condition) ~ ., data = TrainSet, method = "rpart")
model_dt_1 = predict(model_dt, data = TrainSet)
table(model_dt_1, TrainSet$Condition)
mean(model_dt_1 == TrainSet$Condition)