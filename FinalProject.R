###################FinalProject################################

data=read.csv("C:/Users/adam.aslam/OneDrive - Danaher/Desktop/MATH 267/recipeData.csv")
data[,13]=as.numeric(data[,13])

#HERE        put in code to remove N/As and bad data. call it data


tbl=table(data[,5])
top20=as.numeric(names(head(sort(tbl,decreasing=TRUE), n = 20)))
data=(data[data$StyleID %in% top20,])
#View(data)
style=data$Style
n=nrow(data)

#install.packages("Mclust")
library(mclust)

#result <- MclustDA(data[,c(6:12,15)], data[,4], modelType = "EDDA", modelNames = "EEE")

#result=Mclust(data[,c(6:12,15)],3,modelNames = "EDDA") #incorporate N/A cols and categorical later
#result$loglik #smart starting points so dont need to change start and compare this number to find best start (bigger loglikelihood)
#result$parameters #pro are proportions (pi_g's) #mean are mu's for each cluster

#var=result$parameters$variance
#cov2cor(var$sigma[,,2]) #cov matrix for cluster 2
#labels= result$classification #labels
#plot(data,col=style)





############LDA TRY####################
#install.packages("klaR")
#install.packages("psych")
#install.packages("MASS")
#install.packages("devtools")
#install.packages("ggplot2")

library(klaR)
library(psych)
library(MASS)
library(devtools)
library(ggplot2)



LDAall = lda(data$Style~., data[,c(6:13,15)])
LDAall #Top 2 LDA variables account for 97.1% of variation
#ggord(linear, train$Stlye)
LDA1Proportions=abs(LDAall$scaling[,1])/sum(abs(LDAall$scaling[,1]))
LDA2Proportions=abs(LDAall$scaling[,2])/sum(abs(LDAall$scaling[,2]))

topLDA1=(head(sort(LDA1Proportions,decreasing=TRUE), n = 3)) #top 3 in LDA1 are FG, color, and ABV. accounts for 91.2% of variation
topLDA2=(head(sort(LDA2Proportions,decreasing=TRUE), n = 2)) #top 2 in LDA1 are ABV and FG. accounts for 94.1% of variation

#from this, we can reduce model to most important variables for separating classes (FG, color, and ABV)
predictLDA = predict(LDAall)

newdata = data.frame(type = data[,4], lda = predictLDA$x)
ggplot(newdata) + geom_point(aes(lda.LD1, lda.LD2, colour = type), size = .5) #can't easily separate 20 groups visually in 2D, but can definitely see some clusters 
ggplot(newdata) + geom_point(aes(lda.LD1, lda.LD2, colour = type), size = .5)+xlim(-10,7)+ylim(-5,10) #can't easily separate 20 groups visually in 2D, but can definitely see some clusters 

set.seed(21324)
trainSample=sample(1:n,ceiling(.8*n))
train=data[sample(1:n,ceiling(.8*n)),]
indicesTest=c(1:n)[-trainSample]
test=data[indicesTest,]

#HERE   RUN EM ON TRAINING AND TEST SET WITH TEST SET LABELS REMOVED
#EMData= data[,c(4,8,9,11)]




