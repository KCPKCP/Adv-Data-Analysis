
setwd("C:/Users/Home/Desktop/DePaul/DSC-424-AdvancedDataAnalysis/week-5/Homework")

ds = read.table("Survey.csv", sep=",", header=T)

head(ds)
ncol(ds)
nrow(ds)

# Let's look at the corrplot 
library(corrplot)
c = cor(ds, method="pearson")
print(c)
corrplot(c, method="ellipse")  #default is Pearson

cS = cor(ds, method = "spearman") # Now Spearman corrplot
print(cS)
corrplot(cS, method="ellipse")



#Now with Kendall Tau
k = cor(ds, method = "kendall") 
print(k)
corrplot(k, method="ellipse")

# The two show fairly small differences..
max(cS - c)
min(cS - c)
max(k - c)
min(k - c)

range(c)
range(cS)
range(k)

p = prcomp(ds)
summary(p)
plot(p)

p = prcomp(cor(ds, method = "spearman"))
summary(p)
plot(p)
abline(1, 0, col="red")

f = factanal(covmat=c, factors=4 )
f.fa <- factanal(c, factors=4)
f.fa
print(f$loadings, cutoff=.4)


library(psych)
p2 = principal(cor(ds, method = "spearman"), nfactors=4)
summary(p2)
print(p2$loadings, cutoff=.4)


KMO(c)


library(foreign)


bartlett.test(ds)

fit = factanal(ds, 4, scores="regression")
print(fit$loadings, cutoff=.4, sort=T)
print(fit)
fit$correlation

library(psych)
library(lavaan)


hw.model = 'QU =~ Qu1 + Qu2 + Qu3 + Qu4 + Qu5
            Vis =~ Vis1 + Vis2 + Vis3 
            IM =~ Im1 + Im2 + Im3'
fit = cfa(hw.model, data=ds)
summary(fit, fit.measures=TRUE)


library(polycor)

het = hetcor(ds)

ds$Qu1 = factor(ds$Qu1, levels = c(1,2,3,4,5), ordered = T)
ds$Qu2 = factor(ds$Qu2, levels = c(1,2,3,4,5), ordered = T)
ds$Qu3 = factor(ds$Qu3, levels = c(1,2,3,4,5), ordered = T)
ds$Qu4 = factor(ds$Qu4, levels = c(1,2,3,4,5), ordered = T)
ds$Qu5 = factor(ds$Qu5, levels = c(1,2,3,4,5), ordered = T)
ds$Vis1 = factor(ds$Vis1, levels = c(1,2,3,4,5), ordered = T)
ds$Vis2 = factor(ds$Vis2, levels = c(1,2,3,4,5), ordered = T)
ds$Vis3 = factor(ds$Vis3, levels = c(1,2,3,4,5), ordered = T)
ds$Im1 = factor(ds$Im1, levels = c(1,2,3,4,5) , ordered = T)
ds$Im2 = factor(ds$Im2, levels = c(1,2,3,4,5), ordered = T)
ds$Im3 = factor(ds$Im3, levels = c(1,2,3,4,5), ordered = T)

het = hetcor(ds)
summary(het)


hetCor = het$correlations
hetCor

phet = princomp(covmat = hetCor, cor=T)
summary(phet)

phet2 = principal(hetCor, nfactors=4)
summary(phet2)
print(phet2$loadings, cutoff=.4)

View(ds)



library(ca)
data = read.table("StoresAndAges.csv", sep=",", header=T)
data
head(data)
storesName = substr(data$X, 1,1) 
stores = paste(storesName)
stores
data = data[,c(2:5)]
rownames(data) = stores
names(data) <- c( "16To24", "25To34", "35To49", "50Up")
head(data)

mosaicplot(data, shade=T, main="")

c = ca(data)
summary(c)

plot(c)

plot(c, what=c("all", "none"))

c$colcoord
colC = c$colcoord[,1:2]
colC[order(colC[,1]),]

plot(c, mass=T, arrows=c(F,T))

library(car)
library(MASS)
library(rio)
library(g.data)

DA <- import_list("C:/Users/Home/Desktop/DePaul/DSC-424-AdvancedDataAnalysis/week-5/Homework/BondRating.xls")
DATrain = DA$training
DATrain
head(DATrain)

names(DATrain) <- lapply(DATrain[1,], as.character)

DATrain <- DATrain[-1]
DATrain1 <- apply(DATrain[4:13], 2, as.numeric)
DATrain1
DATrain2 <-data.frame(DATrain[1:3], DATrain1)
DATrain2


LDAModel <- lda(CODERTG ~ LOPMAR + LFIXCHAR + LGEARRAT + LTDCAP + LLEVER + LCASHLTD + LACIDRAT +
                  LCURRAT + LRECTURN + LASSLTD , data=DATrain)
LDAModel

pred <- predict(LDAModel, newdata=DATrain[,4:13])$class
pred

table(pred, DATrain$CODERTG)



DAValidate = DA$validation
DAValidate
head(DAValidate)

LDAModeVall <- lda(CODERTG ~ LOPMAR + LFIXCHAR + LGEARRAT + LTDCAP + LLEVER + LCASHLTD + LACIDRAT +
                     LCURRAT + LRECTURN + LASSLTD , data=DAValidate)
LDAModeVall
pred2 <- predict(LDAModeVall, newdata=DAValidate[,4:13])$class
pred2

table(pred2,DAValidate$CODERTG)
