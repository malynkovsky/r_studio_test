print("This file was created within RStudio")

print("And now it lives on GitHub")
str(AirPassengers)
ap <- log10(AirPassengers) - 2
ap
AirPassengers
good_months <- c()
for (i in 2:length(AirPassengers)){
  if (AirPassengers[i] > AirPassengers[i-1])
    good_months<-c(good_months, AirPassengers[i])
}
idx <- 1

moving_average <- c()
idx <-1
for (i in 1:135){
  moving_average[idx] <- mean(AirPassengers[i:i+9])
  idx <- idx + 1
}











moving_average
AirPassengers[1:(1+9)]






airquality
t <- subset(airquality,Month %in% c(7:9))
sum(is.na(t$Ozone))
length(t$Ozone)-sum(is.na(t$Ozone))
aggregate(Ozone~Month,t,length)
library(psych)
describeBy(x=airquality[,1:4], group = airquality$Month)
describe(x=iris)['median']

my_vector <- rnorm(30)

my_vector[sample(1:30, 10)] <- NA

t <- mean(my_vector, na.rm = T)
ert <- replace(my_vector, is.na(my_vector), t)


library(ggplot2)
ggplot(airquality,aes(x=as.factor(Month),y=Ozone))+geom_boxplot()


ggplot(mtcars, aes(x=mpg,y=disp,col=hp))+geom_point()

ggplot(iris,aes(x=Sepal.Length, y=Sepal.Width, col = Species)) + geom_point(aes(size = Petal.Length))
HairEyeColor
dimnames(mydata) 
HairEyeColor[ , 'Blue','Male']
prop.table(HairEyeColor[,'Blue','Male'] )['Red']

sum(HairEyeColor[ , 'Green','Female'])
mydata <- as.data.frame(HairEyeColor)
t <- mydata[mydata$Sex == 'Female']
t <- subset(mydata, Sex == 'Female')
ggplot(data =t , aes(x =Hair , y = Freq, fill= Eye)) + geom_bar(stat="identity", position = position_dodge()) + scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))


t <- subset(mydata, Sex == 'Female' & Hair == 'Brown')
t1 <- table(eye_color = t$Freq)
chisq.test(t$Freq)



t <- diamonds 
t2 <- table(diamonds$cut,diamonds$color)
chisq.test(t2)
t3 <- chisq.test(t2)
t3[1]


factor_price <- ifelse(diamonds$price >= mean(diamonds$price),1,0)
factor_carat <- ifelse(diamonds$carat >= mean(diamonds$carat),1,0)
chisq.test(table(factor_price ,factor_carat))
t <- fisher.test(table(mtcars$am ,mtcars$vs))
t2
sum(t2 < 500)
a <- table(df)
if(sum(t2 < 500)){
  return <- fisher.test(a)[1]
} else{
  return <- c(chisq.test(a)$statistic, chisq.test(a)$parameter, chisq.test(a)$p.value) 
}



ToothGrowth

t <- subset(ToothGrowth, (dose == 0.5 & supp=='OJ') | (dose == 2.0 & supp=='VC'))
shapiro.test(t$len[t$supp=='VC'])
hist(t$len[t$supp=='VC'])
bartlett.test(len ~ supp, t)
m <- t.test(len ~ supp, t)
m$statistic


df <- read.csv('https://stepic.org/media/attachments/lesson/11504/lekarstva.csv')

t.test(df$Pressure_before,df$Pressure_after)
t.test(df$Pressure_before,df$Pressure_after,paired = T)
getwd()


df <- read.table("dataset_11504_16.txt")

bartlett.test(V1 ~ V2, df)

t.test(df$V1,df$V2,var.equal = FALSE)

wilcox.test(V1 ~ V2, df)
npk
fit <- aov(yield~N+P+K,data=npk)
summary(fit)

fit <- aov(Sepal.Width~Species,data=iris)
summary(fit)
TukeyHSD(fit)

df <- read.csv('https://stepic.org/media/attachments/lesson/11504/step6.csv')
df$patient <- as.factor(df$patient)


fit <- aov(temperature~pill*doctor+Error(patient/(pill*doctor)),data=df)
summary(fit)
install.packages("Hmisc") 
library(ggplot2,Hmisc)
obj <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, col = supp, group = supp))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2))
obj


my_vector <- c(1, 2, 3, NA, NA)
is.na(my_vector)
which(is.na(my_vector))
my_vector[is.na(my_vector)]
NA.position <- function(x){
  # put your code here  
  t <- length(which(is.na(x)))
  return(t)
}

NA.position(my_vector)

filtered.sum(c(1, -2, 3, NA, NA))

filtered.sum <- function(x){
  # put your code here  
  t <- which(is.na(x) & x > 0)
  return(t)
}
x <- c(1, -2, 3, 5, 7,-3,8,0,8,27)
sum(x[x > 0],na.rm = T)

x<- c(-3, -3, NA, 1, NA, -2, 0, 2, -7, 0, 0, 1, -2, -6, -3, -9)
x[is.numeric(x)]
IQR(x)
t<-quantile(x, probs = c(0.25, 0.75))
x[x<(t[2]+1.5*IQR(x)) | x>(t[1]-1.5*IQR(x)) ]
x




library(psych)

corr.calc <- function(x){
  # put your code here  
  m <- corr.test(x[,1:2])
  return(c(m$r[2], m$p[2]))
}

corr.calc( mtcars[, c(1,5)] ) 
corr.calc( iris[,1:2] )

m <- corr.test(iris[,1:2])

m$r[2]
m$p[2]

filtered.cor <- function(x){
  nums <- unlist(lapply(x, is.numeric))
  m <- corr.test(x[ , nums])
  diag(m$r) <- 0
  res_idx <- which.max(abs(m$r))
  return(m$r[res_idx])
}

step6 <-  read.table("https://stepic.org/media/attachments/lesson/11504/step6.csv",  header=TRUE, sep=',' )
res <-filtered.cor(step6)
res
max(abs(res))
which.max(abs(res))


smart_cor  <- function(x){
  if (is.numeric(x[,1]) & is.numeric(x[,2])){
    stat_test  <- shapiro.test(x[,1])
    stat_test1  <- shapiro.test(x[,2])
    if (stat_test$p.value > 0.05 & stat_test1$p.value > 0.05){
      t <- corr.test(x)
    } else{
      t <- corr.test(x,method = "spearman")
    }
    return(t$r[2])
  } else{
    print("X is not numeric")
  }
  
}

test_data  <- read.csv("https://stepik.org/media/attachments/course/129/test_data.csv")
smart_cor(test_data)



mrdir()

t <- read.csv("dataset_11508_12.txt", sep = "", head = F, dec = '.') 
fit <- lm(V1~V2,t)
fit
library(ggplot2)
diamonds

fit <- lm(price ~ depth,t )
fit$coefficients # коэффициенты модели
t <- subset(x = diamonds,carat==0.46 & cut == "Ideal")

regr.calc <- function(x){
  t<-psych::corr.test(x[,1:2])
  if (t$p[2]< 0.05){
    fit <- lm(x[,1] ~ x[,2],x )
    x$fit <-fit$fitted.values
    return(x)
  }
  else{
    return("There is no sense in prediction")
  }
}


my_plot <- ggplot(iris, aes(Sepal.Width,Petal.Width, col=Species, xlab = "Sepal.Width", ylab = "Petal.Width"))+
  geom_point()+
  geom_smooth(method = "lm")
my_plot





regr.calc(iris[,c(1,4)])


t<-psych::corr.test(iris[,c(1,4)])
t$p[2]



fill_na <- function(x){
  
  fit <- lm(x[[3]]~x[[1]]+x[[2]],x,na.action = "na.omit")
  res <-predict(fit,test_data)
  x$fit <- ifelse(is.na(test_data$y),res,test_data$y)
  return(x)
}
test_data <- read.csv("https://stepic.org/media/attachments/course/129/fill_na_test.csv")

fit <- lm(y~x_1+x_2,test_data,na.action = "na.omit")
fill_na(test_data)



install.packages("rmarkdown")
