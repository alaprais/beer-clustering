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
library(mclust)
library(MixGHD)
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



beers <- names(sort(table(data$Style)))[sort(table(data$Style)) >= 300]  # keep styles that have over 300 observations (59 styles)
data <- data[data$Style %in% beers,]

summary(data)

#########################################################################################
#### CODE BLOCK OUTPUTS "pairs_4.txt" FILE TO CURRENT DIRECTORY
#### for loop to check every single relationship and note the distinct ones
#### only compare between beers with 300 or more observations, otherwise not very meaningful

# pairs <- c("beer_1","beer_2", "difference")
# beers <- names(sort(table(data$Style)))[sort(table(data$Style)) >= 300]
# i <- 0
# for (beer_1 in beers[1:63]){ #64 causes error
#   for (beer_2 in beers){
#     print(beer_2)
#     if (beer_1 == beer_2){
#       #pass
#     }
#     else{
#       to_keep <- c(beer_1, beer_2) # relationship of interest
#       cur_data <- data[data$Style %in% to_keep,] # only keep beer_1 and beer_2
#       
#       res <- Mclust(cur_data[,c("OG","FG","ABV","IBU","Color","BoilSize","BoilTime","Efficiency")], G=2) # cluster
#       
#       compare <- cbind(cur_data$Style, res$classification)
#       
#       # cluster 1 has x % of the totals for each beer
#       perc_beer_1 <- sum(compare[compare[,2] == 1,][,1] == beer_1)/sum(cur_data$Style == beer_1)
#       perc_beer_2 <- sum(compare[compare[,2] == 1,][,1] == beer_2)/sum(cur_data$Style == beer_2)
#       
#       # cluster 2 has x % of the totals for each beer (redundant since it will be 1-cluster 1)
#       #perc_beer_1 <- sum(compare[compare[,2] == 2,][,1] == beer_1)/sum(data$Style == beer_1)
#       #perc_beer_2 <- sum(compare[compare[,2] == 2,][,1] == beer_2)/sum(data$Style == beer_2)
#       
#       
#       #(perc_beer_1 > .9)&(perc_beer_2 < .1) | (perc_beer_2 > .9)&(perc_beer_1 < .1)
#       if ( abs(perc_beer_2 - perc_beer_1) > .85 ) {
#         pairs <- rbind(pairs, c(beer_1, beer_2, abs(perc_beer_2 - perc_beer_1)))
#       }
#       i <- i + 1
#       print(paste(paste("pair ", i), "of 3481"))
#       print(paste(100 - round((i/3481 * 100),0.2), "% remaining"))
#     }
#   }
# }
# 
# length(unique(pairs[,1])) # error is ok, since we covered everything
#                           # also missing a few relationships is fine, since exploratory
# 
# # writes pairs to file
# write.table(pairs, file="pairs_2.txt", row.names=FALSE, col.names=FALSE)
# 
# sort(table(pairs[,1]))
#########################################################################################

# subset data from highly distinct pairs
# NOTE: HAVE THE "pairs_2.txt" FILE IN THE WORKING DIRECTORY
pairs <- read.table("pairs_4.txt", header = T)
length(unique(pairs$beer_1))  # 
high_distinct_beers <- sort(table(pairs$beer_1)[table(pairs$beer_1) >= 30]) # keep only beers with 30 or more distinct relatiionships

length(high_distinct_beers)  # narrowed to highly distinct beers
high_distinct_beers <- names(high_distinct_beers) # labels for beers

data <- data[data$Style %in% high_distinct_beers,] # keep only high-distinct beers
row.names(data) <- NULL # reset row indices

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

#THIS SECTION SHOULD BE ON ALL DATA, NOT JUST TOP BEERS
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
n <- dim(newdata)[1]
trainSample=sample(1:n,ceiling(.8*n))
train=data[sample(1:n,ceiling(.8*n)),]
indicesTest=c(1:n)[-trainSample]
test=data[indicesTest,]

#################################### END LDA/ VARIABLE SELECTION ########################################################



########################################## LD COLUMN CREATION ####################################################################

#below repeats some code from above but i wanted to make sure it uses the right subset.
#this creates new LD combination columns to be fed into EM


pairs <- read.table("pairs_2.txt", header = T)
length(unique(pairs$beer_1))  # 58 beers
high_distinct_beers <- sort(table(pairs$beer_1)[table(pairs$beer_1) >= 30])

length(high_distinct_beers)  # narrowed to 13 highly distinct beers
high_distinct_beers <- names(high_distinct_beers) # labels for beers

data <- data[data$Style %in% high_distinct_beers,] # keep only high-distinct beers
row.names(data) <- NULL # reset row indices

# count of each style in the data
sort(table(data$Style), decreasing = TRUE) 

#this block can maybe be deleted depending on where we put this section.
#basically just making sure the right data is going in (not the full set)
data=read.csv("C:/Users/adam.aslam/OneDrive - Danaher/Desktop/MATH 267/recipeData.csv")
data <- data[,c(4, 7:13, 15)]
data[data=="N/A"] = NA 
round(colSums(is.na(data))/nrow(data), 3)
data <- drop_na(data)
data <- data[data$OG>1,]
data <- data[data$OG<1.1,]
data <- data[data$FG>.5,]
data <- data[data$ABV>.5,]
data <- data[data$IBU<500,]
data <- data[data$Color<100,]

LDAdistinct = lda(data$Style~., data[,c(2:9)])
LDAdistinct #Top 2 LDA variables account for 90.5% of variation
data[,10]=0
data[,11]=0
colnames(data)[10]="LDA1Combination"
colnames(data)[11]="LDA2Combination"

for (i in 1:nrow(data)){
data[i,10]=sum( ( (data[i,c(2:9)])*as.matrix(as.numeric(LDAdistinct$scaling[,1])) ))
data[i,11]= sum(( (data[i,c(2:9)])*as.matrix(as.numeric(LDAdistinct$scaling[,2])) ))
}


ggplot(data, aes(LDA1Combination,LDA2Combination)) + geom_point(aes(colour = Style), size = .5) 




########################################## END LD COLUMN CREATION SECTION ####################################################################


################################################################ MODEL-BASED CLASSIFICATION ##################################################################
set.seed(8734)
subset_data <- data # copy of data for this section


labels_word <- subset_data$Style # true labels in words
labels_factor <- as.numeric(factor(subset_data$Style)) # true labels as factor

unique(labels_word)  #sanity check
unique(labels_factor)

# test set (and train set)
n <- dim(subset_data)[1]
testSample <- sample(1:n,ceiling(.2*n))
true_test <- labels_factor[testSample]
labels_factor[testSample] <- 0    # artificially erase labels for test set

# checking labels_factor var
table(labels_factor)
sum(labels_factor == 0)/length(labels_factor) # should be 20%

# MGHD
LDA_cols <- c("add in cols")
#res_MGHD <- MGHD(data= subset_data[,LDA_cols], G= 13, label= labels_factor)

predicted_labels <- res_MGHD@map
predicted_test <- predicted_labels[testSample]

sum(predicted_test == true_test)/"add in denom" # accuracy


############################################################## END MODEL-BASED CLASSIFICATION ##################################################################









#################################################################################################LDA on 6 styles
#MOVE THIS SECTION LATER

data <- read.csv("recipeData.csv")
data <- data[,c(4, 7:13, 15)]
data[data=="N/A"] = NA 
round(colSums(is.na(data))/nrow(data), 3)
data <- drop_na(data)
data <- data[data$OG>1,]
data <- data[data$OG<1.1,]
data <- data[data$FG>.5,]
data <- data[data$ABV>.5,]
data <- data[data$IBU<500,]
data <- data[data$Color<100,]
beers <- c("Double IPA", "Foreign Extra Stout", "German Pilsner (Pils)", "Imperial Stout", "Russian Imperial Stout",  "Berliner Weisse")
data6 <- data[data$Style %in% beers,]
LDA6distinct = lda(data6$Style~., data6[,c(2:9)])
LDA6distinct #Top 2 LDA variables account for 90.5% of variation
data[,10]=0
data[,11]=0
colnames(data)[10]="LDA1Combination"
colnames(data)[11]="LDA2Combination"

for (i in 1:nrow(data)){
  data6[i,10]=sum( ( (data6[i,c(2:9)])*as.matrix(as.numeric(LDA6distinct$scaling[,1])) ))
  data6[i,11]= sum(( (data6[i,c(2:9)])*as.matrix(as.numeric(LDA6distinct$scaling[,2])) ))
}


ggplot(data6, aes(LDA1Combination,LDA2Combination)) + geom_point(aes(colour = Style), size = .5) 






