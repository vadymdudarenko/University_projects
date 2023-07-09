
###########################################################################
#		Advanced Econometrics                                                 #
#   Spring semester                                                       #
#   Vadym Dudarenko, Vladimir Shargin                                     #
#   University of Warsaw, Faculty of Economic Sciences                    #
#                                                                         #
#                                                                         #
#                 School Alcohol Consumption                              #
#                                                                         #
###########################################################################

##########################################################
# Ordered choice Models
#########################################################

install.packages("pscl")
install.packages("ucminf")
install.packages("ordinal")
install.packages("reshape")
install.packages("generalhoslem")
install.packages("oglmx")
install.packages("brant")
install.packages("aod")


library("sandwich")
library("zoo")
library("lmtest")
library("MASS")
library("pscl")
library("LogisticDx")
library("ucminf")
library("ordinal")
library("reshape")
library("generalhoslem")
library("oglmx")
library("aod")
library("brant")


# Estimate ordered logit for Dalc(daily alcohol consumption)
ologit = ologit.reg(as.numeric(Dalc)~ sex +studytime + G3+ famsize + goout + famrel
                    + freetime + health + as.factor(Mjob) +
                      traveltime + absences, data=student.mat)
summary(ologit)


# Step 1
# general model
reg1 = lm(as.numeric(Dalc)~ sex +studytime + G3+ famsize + goout + famrel
          + freetime + health + as.factor(Mjob)+
            traveltime + absences, data=student.mat)
summary(reg1)

# test whether all insignificant variables all jointly insignificant
reg1a = lm(as.numeric(Dalc)~ sex +goout + famrel + freetime + 
             traveltime + absences, data=student.mat)
anova(reg1, reg1a)
# all insignificant variables are jointly insignificant
# we can drop of all insignificant variable in one step

#New specific model 
marg = ologit.reg(as.numeric(Dalc)~ sex +goout + famrel + freetime + 
                    traveltime + absences, data=student.mat)
summary(marg)
# marginal effects(E)
options(scipen=999)
margins.oglmx(marg)


# Pseudo-R2 statistics
pR2(marg)
# does not work after ologit.reg
McFaddensR2.oglmx(marg)

# Joint significance
# Likelihood ratio test
ologit.unrestricted = polr(as.factor(Dalc)~sex +goout + famrel + freetime + 
                             traveltime + absences, data=student.mat)

ologit.restricted = polr(as.factor(Dalc)~1, data=student.mat)
lrtest(ologit.unrestricted, ologit.restricted)

# Small value of the parameter
coeftest(marg)

# goodness-of-fit tests
logitgof(student.mat$Dalc, fitted(ologit.unrestricted), g = 10, ord = TRUE)
pulkrob.chisq(ologit.unrestricted, c("sex"))
ologit.unrestricted = ologit.reg(Dalc~sex +goout + famrel + freetime + 
                                   traveltime + absences, data=student.mat)
lipsitz.test(ologit.unrestricted)
# for Lipsitz et al. test
# lipsitz.test works after polr function
student.mat$Dalc = as.factor(student.mat$Dalc)
ologit.unrestricted = polr(Dalc~sex +goout + famrel + freetime + 
                             traveltime + absences, data=student.mat)
lipsitz.test(ologit.unrestricted)

#check the proportional odds assumption
odd = polr(as.factor(Dalc)~ sex +goout + famrel + freetime + 
             traveltime + absences, data=student.mat)
summary(odd)


library(dplyr)
library(ggplot2)

#barplot for Dependence of health condition to the alcohol consumption
plot_data <-student.mat %>% group_by(health) %>% count(ConsLevel=Dalc)

ggplot(plot_data, aes(fill=ConsLevel, y=n, x=health)) + 
  geom_bar(position="dodge", stat="identity")+
ggtitle("Dependence of health condition to the alcohol consumption") +
  xlab("Health") + ylab("Consumtion of alcohol") 

#barplot for Level of alcohol consumption by gende
plot_data1 <-student.mat %>% group_by(Dalc) %>% count(Quantity=sex)

ggplot(plot_data1, aes(fill=Quantity, y=n, x=Dalc)) + 
  geom_bar(position="dodge", stat="identity")+
  ggtitle("Level of alcohol consumption by gender") +
  xlab("Level") + ylab("Quantity") 



