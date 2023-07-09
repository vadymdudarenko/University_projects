install.packages("arules")
install.packages("arulesViz")
install.packages("arulesCBA")

library(arules)
library(arulesViz)
library(arulesCBA)




trans1<-read.transactions("C:/Users/Vadym/Desktop/trans1.csv", format="basket", sep=",", skip=0)
inspect(trans1)
size(trans1)
length(trans1)
LIST(head(trans1))



trans2<-read.transactions("C:/Users/Vadym/Desktop/trans2.csv", format="single", sep=";", cols=c("TRANS","ITEM"), header=TRUE)
trans2



size(trans2)

LIST(head(trans2))
round(itemFrequency(trans1),3)

itemFrequency(trans1, type="absolute")



ctab<-crossTable(trans1, sort=TRUE) 
ctab<-crossTable(trans1, measure="count", sort=TRUE) 
ctab


stab<-crossTable(trans1, measure="support", sort=TRUE)
round(stab, 3)

ptab<-crossTable(trans1, measure="probability", sort=TRUE) # jak support
round(ptab,3)

ltab<-crossTable(trans1, measure="lift", sort=TRUE)
round(ltab,2)


chi2tab<-crossTable(trans1, measure="chiSquared", sort=TRUE)
round(chi2tab,2)


freq.items<-eclat(trans1, parameter=list(supp=0.25, maxlen=15))
inspect(freq.items)
round(support(items(freq.items), trans1) , 2)
freq.rules<-ruleInduction(freq.items, trans1, confidence=0.9) 
freq.rules


inspect(freq.rules)
rules.trans1<-apriori(trans1, parameter=list(supp=0.1, conf=0.5))

rules.by.conf<-sort(rules.trans1, by="confidence", decreasing=TRUE)
inspect(head(rules.by.conf))


rules.by.lift<-sort(rules.trans1, by="lift", decreasing=TRUE) # sorting by lift
inspect(head(rules.by.lift))


rules.by.count<- sort(rules.trans1, by="count", decreasing=TRUE) # sorting by count
inspect(head(rules.by.count))


rules.by.supp<-sort(rules.trans1, by="support", decreasing=TRUE) 
inspect(head(rules.by.supp))


rules.bread<-apriori(data=trans1, parameter=list(supp=0.001,conf = 0.08), 
                      appearance=list(default="lhs", rhs="BREAD"), control=list(verbose=F)) 

# sorting and displaying the rules
rules.bread.byconf<-sort(rules.bread, by="confidence", decreasing=TRUE)
inspect(head(rules.bread.byconf))

rules.bread<-apriori(data=trans1, parameter=list(supp=0.001,conf = 0.08), 
                      appearance=list(default="rhs",lhs="BREAD"), control=list(verbose=F)) 

rules.bread.byconf<-sort(rules.bread, by="confidence", decreasing=TRUE)
inspect(head(rules.bread.byconf))




trans1.closed<-apriori(trans1, parameter=list(target="closed frequent itemsets", support=0.25))

inspect(trans1.closed)
class(trans1.closed)
is.closed(trans1.closed)
freq.closed<-eclat(trans1, parameter=list(supp=0.15, maxlen=15, target="closed frequent itemsets"))
inspect(freq.closed)
is.closed(freq.closed)
freq.max<-eclat(trans1, parameter=list(supp=0.15, maxlen=15, target="maximally frequent itemsets"))



inspect(freq.max) # not clear output, ham is not more frequent than individual baskets

is.significant(rules.bread, trans1)

is.maximal(rules.bread)

inspect(rules.bread[is.maximal(rules.bread)==TRUE])


is.redundant(rules.bread)
inspect(rules.bread[is.redundant(rules.bread)==FALSE])


is.superset(rules.bread)
is.subset(rules.bread)
is.superset(rules.bread, sparse=FALSE)
supportingTransactions(rules.bread, trans1)






inspect(supportingTransactions(rules.bread, trans1))
trans.sel<-trans1[,itemFrequency(trans1)>0.05] # selected transations
d.jac.i<-dissimilarity(trans.sel, which="items") # Jaccard as default
round(d.jac.i,2)



trans.sel<-trans1[,itemFrequency(trans1)>0.05] # selected transactions

# Jaccard by default
d_jac.t<-dissimilarity(trans.sel, which="transactions") 
round(d_jac.t,2)


plot(hclust(d_jac.t, method="ward.D2"), main="Dendrogram for trans")
plot(hclust(d.jac.i, method="ward.D2"), main="Dendrogram for items")


itemFrequencyPlot(trans1, topN=10, type="absolute", main="Item Frequency") 
itemFrequencyPlot(trans1, topN=10, type="relative", main="Item Frequency")


image(trans1)
plot(rules.trans1, method="matrix", measure="lift")

plot(rules.trans1) 
plot(rules.trans1, measure=c("support","lift"), shading="confidence")

plot(rules.trans1, method="grouped") 
plot(rules.trans1, method="graph", control=list(type="items"))

plot(rules.trans1, method="paracoord", control=list(reorder=TRUE))
plot(rules.trans1, shading="order", control=list(main="Two-key plot"))




names(itemFrequency(trans1)) # info on product categories

names.real<-c("BISCUIT", "BOURNVITA" , "BREAD", "COCK", "COFFEE", "CORNFLAKES", "JAM",       
               "MAGGI", "MILK", "SUGER", "TEA") # old names

names.level1<-c("cake", "cake", "breakfast", "breakfast", "drink", 
                "breakfast", "breakfast","cake", "drink", "cake", "drink") # new names


itemInfo(trans1)<-data.frame(labels = names.real, level1 = names.level1)
itemInfo(trans1)
trans1_level2<-aggregate(trans1, by="level1")
trans1
inspect(trans1) # transactions with old names

inspect(trans1_level2) # transactions with new names
itemInfo(trans1_level2) ## labels sorted alphabetically


rules.trans1_lev2<-apriori(trans1_level2, parameter=list(supp=0.1, conf=0.5))
rules.by.conf<-sort(rules.trans1_lev2, by="confidence", decreasing=TRUE) 
inspect(head(rules.by.conf))
trans<-random.transactions(nItems=10, nTrans=20, method="independent", verbose=FALSE)
image(trans)
inspect(trans)


rules.random<-apriori(trans, parameter=list(supp=0.05, conf=0.3)) 
inspect(rules.random)
rules.by.conf<-sort(rules.random, by="confidence", decreasing=TRUE) 
inspect(rules.by.conf)
size(rules.by.conf)
length(rules.by.conf)
freq.items<-eclat(trans, parameter=list(supp=0.25, maxlen=15)) # basic eclat
inspect(freq.items)











a<- data.tabel(1,2)
