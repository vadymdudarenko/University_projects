df[1] <- NULL
df[1] <- NULL
df[1] <- NULL
summary(df)

library(ggplot2)
library(Hmisc)
hist.data.frame(df)

dim(df)

install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
library(caret)
preproc <- preProcess(df, method=c("center", "scale"))
df_norm <- predict(preproc, df)

library(GGally)
ggpairs(df_norm)


install.packages("corrplot")
install.packages("xlsx")

library(corrplot)
library(xlsx)
cor<-cor(df_norm, method="pearson") 
corrplot(cor)



pca <- prcomp(df_norm, center=FALSE, scale=FALSE)

pca$rotation


df_norm.cov<-cov(df_norm)
df_norm.eigen<-eigen(df_norm.cov)
df_norm.eigen$values

library("factoextra")
fviz_eig(pca, choice='eigenvalue')



summary(pca)


fviz_eig(pca)


fviz_pca_ind(pca, col.ind="cos2", geom = "point", gradient.cols = c("green", "yellow", "red" ))

fviz_pca_var(pca, col.var = "red")

library(gridExtra)
PC1 <- fviz_contrib(pca, choice = "var", axes = 1)
PC2 <- fviz_contrib(pca, choice = "var", axes = 2)
grid.arrange(PC1, PC2)

