setwd('~/Desktop/YZ/')
# load the package 
library(tables)
library(grDevices)
library(MASS)
library(pwr)  
library(car)     

Data<-read.table("Data.txt",header=TRUE)
head(Data)
dim(Data)

## average grain yield  
mean(Data$GY,na.rm=TRUE)

## average for all traits
Mean.rm<-function(x){
a<-mean(x,na.rm=TRUE)
}
apply(Data[,2:12],2,Mean.rm)

## variance and sd
var(Data[,2],na.rm=TRUE)
sd(Data[,2],na.rm=TRUE)

VVV<-Data[-which(is.na(Data[,2])==TRUE),2]
var(VVV)
sqrt(var(VVV))
sum((VVV-mean(VVV))^2)/(length(VVV)-1)

## Histogram show mean and sd
layout(matrix(c(1:12),2,6,byrow=TRUE))
  for(i in 2:13){
    hist(Data[,i],col="lightgreen",main=paste(colnames(Data)[i]))
  abline(v=mean(Data[,i],na.rm=TRUE),lwd=3,col="red")
    }

##
max(Data[,2],na.rm=TRUE)
min(Data[,2],na.rm=TRUE)
range(Data[,2],na.rm=TRUE)
quantile(Data[,2],na.rm=TRUE)

# box plot
boxplot(Data[,2],col="blue",boxwex =0.5,lwd=1.5)
# box plot for table
boxplot(Data[,-c(1,3,4,7,8,9,13)],col=rainbow(7, start = 0, end = 1))
par(las=1)# all axis labels horizontal
boxplot(Data[,-c(1,3,4,7,8,9,13)],col=rainbow(7, start = 0, end = 1),
        horizontal = TRUE)

## QQ-plot
y <- rnorm(500,0,5)
qqnorm(y); 
qqline(y, col = "red",lwd=2)

layout(matrix(c(1:2),2,1))
x<-rchisq(200, 2,1)-5
qqplot(x,y,col="blue")
x<-rnorm(200, 2,1)
qqplot(x,y,col="green")


# QQ-plot and histogram
layout(matrix(c(1:6),2,3))
for(i in c(2,12,13)){
qqnorm(Data[,i], ylab = paste(colnames(Data)[i]),
main=paste("QQ-plot for",colnames(Data)[i]),pch=15,col="blue")
qqline(Data[,i], col = "red",lwd=2)
hist(Data[,i],col="lightblue",main=paste("Histogram of",colnames(Data)[i]))
}


##Categorical variables, barplot, pie chart
require(grDevices) # for colours
Data[seq(1,80,10),c(1,12,13)]
barplot(table(Data[,1]),col=rainbow(4))

# Simple Pie chart
layout(matrix(c(1,2),2,1))
pie(table(Data[,1]), main="Pie Chart of Countries",col=rainbow(4))

# add pecentage to Pie chart
Coun<-table(Data[,1])
pct <- round(Coun/sum(Coun)*100)
lb <- paste(names(table(Data[,1]))," ", pct,"%",sep="") # add percents to labels
pie(table(Data[,1]),labels = lb, main="Pie Chart of Countries",col=rainbow(4))

## boxplot for groups

boxplot(GY ~ Country,data=Data, col=rainbow(4))

##Inferential Statistics: Correlation and Regression

## Correlation, scatter plot
# simple scatter plot
cor(Data$GY,Data$TKW)
cor(Data$GY,Data$TKW,"pairwise.complete.obs")
plot(Data$GY,Data$TKW,col="blue",pch=15)
COR<-cor(Data$GY,Data$TKW,"pairwise.complete.obs")
text(2,40,paste("Cor=",round(COR,2)),cex=1.5,col="red")

plot(rep(seq(1:10),2),c(rep(4,10),rep(2,10)),pch=seq(1:20),ylim=c(0,5),cex=1.5)
text(rep(seq(1:10),2),c(rep(4.5,10),rep(2.5,10)),paste("pch",seq(1:20)),col="blue")

# Add color to the plot according to the group

# create a function with own defined color 
table(Data$Country)
col_func <- colorRampPalette(
  colors = c("red","green","pink", "blue"),
  space = "Lab"
  )
Own_col <- col_func(nlevels(Data$Country))

plot( x = Data$GY,y = Data$TKW,
  xlab = "Grain yield",
  ylab = "Thousand kernel weight",
  cex=2,
  main="Scatter plot of GY and TKW",
  pch = 20, # choose a type of dots 
  col = Own_col[Data$Country]
)
## add legend
legend(
  x ="bottomright",#, "bottom","bottomleft","left", "topleft", "top", "topright", "right" and "center"
  legend = levels(Data$Country), # for text of legend
  col = Own_col,
  pch = 20,
  cex = 1.2 # size of the legend
)

## pairwise correlation
cor(Data[,11:13])
cor(Data[,11:13],use="pairwise.complete.obs")

COR<-cor(Data[,2:4],use="pairwise.complete.obs")
round(COR,2)
pairs(Data[,2:4],pch=15,col="green")
 
## one sample t test

t.test(Data$Leaf,mu=4)

## two sample t test
Y1<-Data$GY[which(Data$Country=="Germany")]
Y2<-Data$GY[which(Data$Country=="France")]
t.test(Y1,Y2)

## paired t test
mean(Data$SDW,na.rm=TRUE);mean(Data$SDW_M,na.rm=TRUE)
t.test(Data$SDW,Data$SDW_M,paired=TRUE)
t.test(Data$SDW,Data$SDW_M,paired=TRUE,alt="less")
t.test(Data$SDW,Data$SDW_M,paired=TRUE,alt="greater")

## F test one tailed
Y1<-Data$GY[which(Data$Country=="Germany")]
Y2<-Data$GY[which(Data$Country=="France")]
var(Y1,na.rm=TRUE);var(Y2,na.rm=TRUE)
var.test(Y1,Y2)

## F test two tailed
var(Data$SDW,na.rm=TRUE);var(Data$SDW_M,na.rm=TRUE)
var.test(Data$SDW,Data$SDW_M, alternative = "less")#, "greater","two.sided"

## correlation test
cor.test(Data$GY,Data$TKW)

#Power analysis for correlation
pwr.r.test(n = NULL,r = 0.52, sig.level = 0.05,  power = 0.80, 
alternative = "two.sided")
pwr.r.test(n = NULL, r = 0.52, sig.level = 0.01,  power = 0.80, 
alternative = "two.sided")


## Chi square test for ?Goodness-of-Fit ?
observed = c(30, 50)        # observed frequencies
expected = c(0.25, 0.75)      # expected proportions
chisq.test(x = observed,p = expected)

## Chi square test of independence
tabular(Country~Factor(Leaf,"Leaf number")+1,data=Data)
tabular(Country~Factor(Tiller>4,"Tiller number")+1,data=Data)

tbl<-tabular(Country~Factor(Tiller>4,"Leaf number")+1,data=Data)
Tiller<-data.frame(matrix(unlist(tbl),4,3),row.names=names(table(Data$Country)))
Tiller
chisq.test(Tiller[,1:2])
Tiller[3,2]<-16
chisq.test(Tiller[,1:2])

# example for combine data 
Tiller<-data.frame(matrix(c(37,19,18,6),2,2),row.names=c("Europe","Others"))
chisq.test(Tiller) 

# fisher exact test
tbl<-tabular(Country~Factor(Tiller>4,"Leaf number")+1,data=Data)
Tiller<-data.frame(matrix(unlist(tbl),4,3),row.names=names(table(Data$Country)))
fisher.test(Tiller[,1:2],alternative="two.sided")
Tiller[3,2]<-16
fisher.test(Tiller[,1:2],alternative="two.sided")

# Example code for linear regression, one variable
X<-c(5,3,2,7,4,9,8,6)
Y<-c(5.6,3.1,4.7,7.7,8.1,7.0,7.2,6.7)
lm(Y~X)

# Example code for linear regression, two variable
X1<-c(5,3,2,7,4,9,8,6)
X2<-c(6,7,3,2,4,9,8,1)
Y<-c(5.1,4.0,2.6,6.2,4.9,4.8,3.7,5.4)
lm(Y~X1+X2)

# Example code for linear regression, multiple variable
lm(Data[,-1])

# alternative code for linear regression
R_lm<-lm(GY~SY+TKW+SDW+SDL+STM+SPK+TKW_M+SDW_M+SDL_M+Tiller+Leaf,Data)
#R_lm<-lm(GY~SY,Data)
summary(R_lm)

# diagnostic plots
layout(matrix(c(1:4),2,2,byrow=TRUE))
plot(R_lm)
dev.off()

# Residuals vs Fitted plot
plot(R_lm,which=1,col="blue",pch=15)
# the red lines in the middle is a smooth curve fitted by loess
scatter.smooth(R_lm$residuals~R_lm$fitted.values , span = 2/3, 
      degree = 1,pch=15, col="Green",      
      ylab = "Residuals", xlab = "Fitted values", 
      main = "Myself Residuals vs Fitted plot")

# Assessing Outliers, Bonferonni p-value for observations with most extreme residuals
outlierTest(R_lm) 
rstudent unadjusted p-value Bonferonni p
Gen_80  4.747541         1.4282e-05   0.00099976
Gen_39 -3.978719         1.9799e-04   0.01386000

layout(matrix(c(1:2),1,2))
plot(R_lm,which=2,col="blue",pch=15)
qqnorm(R_lm$residual,col="green",pch=15); 
qqline(R_lm$residual, col = "red",lwd=2)

layout(matrix(c(1:2),1,2))
plot(R_lm,which=3,col="blue",pch=15)
OrderFit<-order(R_lm$fitted.values)
plot(R_lm$fitted.values[OrderFit],sqrt(abs(scale(R_lm$residual[OrderFit]))),pch=15,ylim=c(0,2))

#  Residual Vs leverage
plot(R_lm,which=5,col="blue",pch=15)
lm(Data[,-1])
lm(Data[-80,-1])

# limit of regression
 lm(Data[1:10,-1])

# Analysis of variance(ANOVA)
Data2<-read.table("Data treatments.txt",header=TRUE)
Anova_GY<-aov(GY~Treatment,Data2)
Anova_GY
summary(Anova_GY)

# boxplot for Data 2 grain yield
boxplot(GY ~ Treatment,data=Data2, col=rainbow(3),ylim=c(0,15))
Mean<-function(x){mean(x,na.rm=TRUE)}
Max<-function(x){max(x,na.rm=TRUE)}
Min<-function(x){min(x,na.rm=TRUE)}
tabular(Treatment~GY*(Mean+Max+Min)+1,data=Data2)

# One way anova for all 8 traits
Traits<-colnames(Data2)[-c(1:3)]
Pv<-NULL
for(i in 1:8){
Anova_T<-aov(as.formula(paste(Traits[i],"~Treatment")),Data2)
Pv<-rbind(Pv,cbind(Traits[i],as.numeric(summary(Anova_T)[[1]][["Pr(>F)"]][1])))
}
Pv

## Post hoc test after one way anova 
# Tukey's HSD (honest significant difference) test
i<-1
#Anova_T<-aov(as.formula(paste(Traits[i],"~Genotypen+Treatment")),Data2)
Anova_T<-aov(as.formula(paste(Traits[i],"~Treatment")),Data2)
posthoc <- TukeyHSD(x=Anova_T, 'Treatment', conf.level=0.95)
posthoc
#Figure to interpret Tukey's HSD test 
round(posthoc$Treatment,4)
plot(posthoc)

## two way anova

Anova_Two<-aov(as.formula(paste(Traits[i],"~Country+Treatment+Country*Treatment")),Data2)
summary(Anova_Two)

## Anova and linear regression
Lm_1<-lm(as.formula(paste(Traits[i],"~Treatment")),Data2)
summary(Lm_1)
anova(Lm_1)

Lm_2<-lm(as.formula(paste(Traits[i],"~Country+Treatment+Country*Treatment")),Data2)
summary(Lm_2)
anova(Lm_2)




