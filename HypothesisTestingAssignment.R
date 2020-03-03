#######################1st Assignment##########################
cutlets<-read.csv(file.choose())
View(cutlets)
attach(cutlets)
colnames(cutlets)
attach(cutlets)

#############Normality test###############

shapiro.test(Unit.A)
#data:  Unit.A
#W = 0.96495, p-value = 0.32
## p-value = 0.32 > 0.05 so p high null fly => It follows normal distribution
shapiro.test(Unit.B)

#data:  Unit.B
#W = 0.97273, p-value = 0.5225

## p-value = 0.5225 > 0.05 so p high null fly => It follows normal distribution

#############Variance test###############

var.test(Unit.A,Unit.B)#variance test
#p-value = 0.3136 > 0.05 so p high null Fly => 

############2 sample T Test ##################



?t.test
t.test(Unit.A,Unit.B,alternative = "greater")

# alternative = "greater means true difference is greater than 0
# Null Hypothesis -> (UnitA-UnitB) < 0
# Alternative Hypothesis -> (StandardPromotion - InterestRateWaiver) > 0
#p-value = 0.2362 > 0.05 => p High null fly => accept null hypothesis
# sample estimates:
#mean of x mean of y 
#7.019091  6.964297

#*************************************2nd Assignment Example***************************

labtat<-read.csv(file.choose())
View(labtat)
attach(labtat)
colnames(labtat)
sum(Laboratory.1)
#21403.39
sum(Laboratory.2)
#21468.35
sum(Laboratory.3)
#23989.59
sum(Laboratory.4)
#19641.93
#Null hypothesis:H0:u1=u2=u3=u4
#5% significance level=-1.96 to +1.96

#############Normality test###############

shapiro.test(Laboratory.1)
#data:  Laboratory.1
#W = 0.99018, p-value = 0.5508
## p-value = 0.5508 > 0.05 so p high null fly => It follows normal distribution
shapiro.test(Laboratory.2)

#data:  Laboratory 2
#W = 0.99363, p-value = 0.8637

## p-value = 0.8637 > 0.05 so p high null fly => It follows normal distribution

shapiro.test(Laboratory.3)

#data:  Laboratory 3
#W = 0.98863, p-value = 0.4205

## p-value = 0.4205 > 0.05 so p high null fly => It follows normal distribution

shapiro.test(Laboratory.4)

#data:  Laboratory 4
#W = 0.99138, p-value = 0.6619

## p-value = 0.6619 > 0.05 so p high null fly => It follows normal distribution
#############Variance test###############

var.test(Laboratory.1,Laboratory.2)#variance test
# p-value = 0.1675 > 0.05 so p high null fly => Equal 

var.test(Laboratory.1,Laboratory.3)#variance test
# p-value = 0.01366 < 0.05 so p low null go => un Equal variances

var.test(Laboratory.1,Laboratory.4)#variance test
# p-value = 0.1408 > 0.05 so p high null fly =>  Equal variances

var.test(Laboratory.2,Laboratory.3)#variance test
# p-value = 0.2742 > 0.05 so p high null fly =>  Equal variances

var.test(Laboratory.2,Laboratory.4)#variance test
# p-value = 0.9261 > 0.05 so p high null fly =>  Equal variances

var.test(Laboratory.3,Laboratory.4)#variance test
# p-value = 0.3168 > 0.05 so p high null fly =>  Equal variances


#############Anova Test#########

Anova_results<-aov(Laboratory.1~Laboratory.2*Laboratory.3*Laboratory.4,data = labtat)
summary(Anova_results)
# p-value is greater than 0.05 accept null hypothesis 
# All Proportions are  equal

#*****************************3rd example************************************


#########Chi Square(Buyers Ratio)#################
buyersRatio<-read.csv(file.choose())
View(buyersRatio)
attach(buyersRatio)
datatableBR<-matrix(c(50,435,142,1523,131,1356,70,750),nrow=2,ncol=4)
colnames(datatableBR)<-c("East","West","North","South")
rownames(datatableBR)<-c("Males","Females")
chisq.test(datatableBR)
# p-value = 0.6603 > 0.05  => Accept null hypothesis
# => Both Male and Female  have equal proportions 



#***************************$4th example customerorderform****************************

#########Chi Square(customerOrderForm)#################
cof<-read.csv(file.choose())
View(cof)
attach(customerOrderForm)
str(cof)
table(cof)
datatable1<-matrix(c(cof$Phillippines,cof$Indonesia,cof$Malta,cof$India),nrow=300,ncol = 4)
colnames(datatable1)<-c("Phillippines","Indonesia","Malta","India")

table<-summary(datatable1)

datatable2<-matrix(c(29,271,33,267,31,269,20,280),nrow = 2,ncol = 4)
colnames(datatable2)<-c("Phillippines","Indonesia","Malta","India")
rownames(datatable2)<-c("Defective","Error Free")
datatable2
chisq.test(datatable2)

#Pearson's Chi-squared test

#data:  datatable2
#X-squared = 3.859, df = 3, p-value = 0.2771>0.05
#Accept null hypothesis..





#*********************5thexample faltoons********************
faltoons<-read.csv(file.choose())
View(faltoons)
attach(faltoons)
chisq.test(Weekend,Weekdays)

#data:  Weekend and Weekdays
#X-squared = 2.2781e-30, df = 1, p-value = 1
#p value is high accept null hypothesis.



