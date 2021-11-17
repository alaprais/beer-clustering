###################FinalProject#################################

### Personal Path ##################
personal_path <- "C:/Users/Arnaud Laprais/Desktop/SJSU/267 computational stats/beer_project/recipeData.csv"
##################################
####### Libraries ################
library(dplyr)
library(tidyr)
library(mclust)
library(klaR)
library(psych)
library(MASS)
library(devtools)
library(ggplot2)
##################################
###################################################### START DATA CLEANING ############################################
data <- read.csv(personal_path)


# Keeping numeric/useful columns
data <- data[,c(4, 7:13, 15)]


#NAs are listed as "N/A" characters - convert to NA:
data[data=="N/A"] = NA 


# Percentage of NAs by column
round(colSums(is.na(data))/nrow(data), 3)
# There are just a few in Style so get rid of them
data <- drop_na(data)


# Water has a specific gravity of 1.000 and When grains for the wort are added, the density increases 
# There should not be any beers with an original gravity of 1.000. 
# High gravity beers have an OG of 1.075 so anything greater than 1.1 seems pretty unreasonable
data <- data[data$OG>1,]
data <- data[data$OG<1.1,]


# FG can be less than one due to alcohol content - there's one listed with FG<0.5 that doesn't make sense so remove it:
data <- data[data$FG>.5,]



# Focus just on Alcoholic beers - remove all beers with ABV < 0.5
data <- data[data$ABV>.5,]


# IBU is kinda murky - almost all beers fall between 5 and 120, but can get up into the hundreds (and apparently a
# couple in the thousands - but since these are homebrews not sure what's reasonable)
data <- data[data$IBU<500,]


# Color is generally between 0 - 40 but can get higher. Like IBU, not sure where we should draw the line (theres only 1 over 100 and it's
# pretty suspect so removed it).
data <- data[data$Color<100,]
summary(data)

# Keep 10 styles only
to_keep <- c("American Pale Ale","Saison","American Amber Ale", "Blonde Ale", 
             "Irish Red Ale", "Witbier", "Russian Imperial Stout", "Robust Porter",
             "Kölsch", "Cream Ale")

data <- data[data$Style %in% to_keep,]

# count of each style in the data
sort(table(data$Style), decreasing = TRUE) 

#########################################  END DATA CLEANING ###############################################################

######################################## START LDA/VARIABLE SELECTION ######################################################

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


LDAall = lda(data$Style~., data[,c(2:9)])
LDAall #Top 2 LDA variables account for 95.4% of variation
#ggord(linear, train$Stlye)
LDA1Proportions=abs(LDAall$scaling[,1])/sum(abs(LDAall$scaling[,1]))
LDA2Proportions=abs(LDAall$scaling[,2])/sum(abs(LDAall$scaling[,2]))

topLDA1=(head(sort(LDA1Proportions,decreasing=TRUE), n = 2)) #top 2 in LDA1 are OG, and FG. accounts for 99.4% of variation
topLDA2=(head(sort(LDA2Proportions,decreasing=TRUE), n = 2)) #top 2 in LDA1 are OG and FG. accounts for 98.2% of variation

#from this, we can reduce model to most important variables for separating classes (FG, color, and ABV)
predictLDA = predict(LDAall)

newdata = data.frame(type = data[,1], lda = predictLDA$x)
ggplot(newdata) + geom_point(aes(lda.LD1, lda.LD2, colour = type), size = .5) #can't easily separate 10 groups visually in 2D, but can definitely see some clusters 
#ggplot(newdata) + geom_point(aes(lda.LD1, lda.LD2, colour = type), size = .5)+xlim(-12,7)+ylim(-10,5) #can't easily separate 10 groups visually in 2D, but can definitely see some clusters 

set.seed(21324)
trainSample=sample(1:n,ceiling(.8*n))
train=data[sample(1:n,ceiling(.8*n)),]
indicesTest=c(1:n)[-trainSample]
test=data[indicesTest,]

#################################### END LDA/ VARIABLE SELECTION ########################################################

