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
library(caret)
library(ks)
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
#########################################  END DATA CLEANING ###############################################################
lda_try <- TRUE
if (lda_try == TRUE){
######################################## START LDA/VARIABLE SELECTION ######################################################

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
}
#################################### END LDA/ VARIABLE SELECTION ########################################################


###################################### BEER STYLE SELECTION ##############################################################
#### CODE BLOCK OUTPUTS "pairs_4.txt" FILE TO CURRENT DIRECTORY
#### for loop to check every single relationship and note the distinct ones
#### only compare between beers with 300 or more observations, otherwise not very meaningful
#### NOTE: TIME CONSUMING - checks (59*59 - 59) mclust models

create_txt_file <- FALSE
if (create_txt_file == TRUE){
  pairs <- c("beer_1","beer_2", "difference")
  beers <- names(sort(table(data$Style)))[sort(table(data$Style)) >= 300] # 59 beers
  i <- 0
  for (beer_1 in beers){ 
    for (beer_2 in beers){
      print(paste(paste(beer_1," vs "),beer_2))
      if (beer_1 == beer_2){
        #pass
      }
      else{
        to_keep <- c(beer_1, beer_2) # relationship of interest
        cur_data <- data[data$Style %in% to_keep,] # only keep beer_1 and beer_2
        
        cols <- c("OG","FG","ABV","IBU","Color","BoilSize","BoilTime","Efficiency")
        res <- Mclust(cur_data[,cols], G=2) # cluster on all vars
        
        compare <- cbind(cur_data$Style, res$classification)
        
        # cluster 1 has x % of the totals for each beer
        perc_beer_1 <- sum(compare[compare[,2] == 1,][,1] == beer_1)/sum(cur_data$Style == beer_1)
        perc_beer_2 <- sum(compare[compare[,2] == 1,][,1] == beer_2)/sum(cur_data$Style == beer_2)
        
        # cluster 2 has x % of the totals for each beer (redundant since it will be 1-cluster 1)
        #perc_beer_1 <- sum(compare[compare[,2] == 2,][,1] == beer_1)/sum(data$Style == beer_1)
        #perc_beer_2 <- sum(compare[compare[,2] == 2,][,1] == beer_2)/sum(data$Style == beer_2)
        
        
        #(perc_beer_1 > .9)&(perc_beer_2 < .1) | (perc_beer_2 > .9)&(perc_beer_1 < .1)
        if ( abs(perc_beer_2 - perc_beer_1) > .85 ) {
          pairs <- rbind(pairs, c(beer_1, beer_2, abs(perc_beer_2 - perc_beer_1)))
        }
        i <- i + 1
        print(paste(paste("pair ", i), "of 3422"))
        print(paste(100 - round((i/3422 * 100),0.2), "% remaining"))
      }
    }
  }
  
  length(unique(pairs[,1]))
  
  # writes pairs to file
  write.table(pairs, file="pairs_4.txt", row.names=FALSE, col.names=FALSE)
  
  sort(table(pairs[,1]))
}
#########################################################################################
lda_full <- TRUE   # TRUE runs LDA cols on full data, FALSE runs LDA cols on subset data
if (lda_full == TRUE){
####################################### LDA COLUMNS on full data ##############################
LDA6distinct = lda(data$Style~., data[,c(2:9)])
LDA6distinct #Top 2 LDA variables account for 90.5% of variation
data[,10]=0
data[,11]=0
colnames(data)[10]="LDA1Combination"
colnames(data)[11]="LDA2Combination"

for (i in 1:nrow(data)){
  data[i,10]=sum( ( (data[i,c(2:9)])*as.matrix(as.numeric(LDA6distinct$scaling[,1])) ))
  data[i,11]= sum(( (data[i,c(2:9)])*as.matrix(as.numeric(LDA6distinct$scaling[,2])) ))
}

ggplot(data, aes(LDA1Combination,LDA2Combination)) + geom_point(aes(colour = Style), size = .5) 
}
########################################## END LDA COLUMNS on full data ######################
############################################## SUBSET ##################################################
# subset data from highly distinct pairs
# NOTE: HAVE THE "pairs_4.txt" FILE IN THE WORKING DIRECTORY
pairs <- read.table("pairs_4.txt", header = T)
length(unique(pairs$beer_1))  # 
high_distinct_beers <- sort(table(pairs$beer_1)[table(pairs$beer_1) >= 5]) # keep only beers with 5 or more distinct relationships

length(high_distinct_beers)  # narrowed to highly distinct beers
high_distinct_beers <- names(high_distinct_beers) # labels for beers

class_data <- data[data$Style %in% high_distinct_beers,] # keep only high-distinct beers
row.names(class_data) <- NULL # reset row indices

# count of each style in the data
sort(table(class_data$Style), decreasing = TRUE) 
###################################### END BEER STYLE SELECTION ##############################################################

if (lda_full == FALSE){
####################################### LDA COLUMNS on subset ##############################
LDA6distinct = lda(class_data$Style~., class_data[,c(2:9)])
LDA6distinct #Top 2 LDA variables account for 90.5% of variation
class_data[,10]=0
class_data[,11]=0
colnames(class_data)[10]="LDA1Combination"
colnames(class_data)[11]="LDA2Combination"

for (i in 1:nrow(class_data)){
  class_data[i,10]=sum( ( (class_data[i,c(2:9)])*as.matrix(as.numeric(LDA6distinct$scaling[,1])) ))
  class_data[i,11]= sum(( (class_data[i,c(2:9)])*as.matrix(as.numeric(LDA6distinct$scaling[,2])) ))
}

ggplot(class_data, aes(LDA1Combination,LDA2Combination)) + geom_point(aes(colour = Style), size = .5) 
}
####################################### END LDA COLUMNS #########################
###################################### CLASSIFICATION ##############################################################
set.seed(8734)

l <- levels(factor(class_data$Style)) # levels of beer (in words) for converting back

labels_factor <- as.numeric(factor(class_data$Style)) # true labels as numeric
true_labels <- labels_factor

l  #sanity check
unique(labels_factor)

# test set (and train set)
n <- dim(class_data)[1]
testSample <- sample(1:n,ceiling(.8*n))
true_test <- labels_factor[testSample]
labels_factor[testSample] <- 0    # artificially erase labels for test set

# checking labels_factor var
table(labels_factor)
sum(labels_factor == 0)/length(labels_factor) # should be 80%


# MGHD
#cols <- names(data)[-1] # full model
#cols <- c("OG", "FG", "ABV", "IBU", "Color")
#cols <- c("OG", "FG", "ABV")
#cols <- c("Color", "ABV") # best model using existing columns
cols <- c("LDA1Combination", "LDA2Combination") # LDA vars

res_MGHD <- MGHD(data= class_data[,cols], G= 6, label= labels_factor)

predicted_labels <- res_MGHD@map  # maximum a posteriori estimates of labels
predicted_test <- predicted_labels[testSample]

sum(predicted_test == true_test)/length(testSample) # accuracy

# plots
plot(class_data[,c("LDA1Combination","LDA2Combination")], col=predicted_test, main = "Predicted")
plot(class_data[,c("LDA1Combination","LDA2Combination")], col=true_test, main = "True")

# Confusion Matrix
confus <- confusionMatrix(factor(predicted_test, labels = l), factor(true_test,labels = l))
confus$table

rand_class <- FALSE
if (rand_class == TRUE){
############# RANDOM CLASSIFICATION ################
# choose (ngroup) random beers, look at predication accuracy with LDA1 LDA2
accuracies <- c()
combos <- matrix(data=NA, nrow=200, ncol=6)
for (i in 1:200){
  ngroup <- 6
  set.seed(NULL)
  high_distinct_beers <- sample(unique(data$Style), size=ngroup, replace=FALSE)
  class_data <- data[data$Style %in% high_distinct_beers,] # keep only high-distinct beers
  row.names(class_data) <- NULL # reset row indices

  # count of each style in the data
  print(sort(table(class_data$Style), decreasing = TRUE))


  l <- levels(factor(class_data$Style)) # levels of beer (in words) for converting back

  labels_factor <- as.numeric(factor(class_data$Style)) # true labels as numeric
  true_labels <- labels_factor

  l  #sanity check
  unique(labels_factor)

  # test set (and train set)
  n <- dim(class_data)[1]
  testSample <- sample(1:n,ceiling(.8*n))
  true_test <- labels_factor[testSample]
  labels_factor[testSample] <- 0    # artificially erase labels for test set

  cols <- c("LDA1Combination", "LDA2Combination") # LDA vars

  res_MGHD <- MGHD(data= class_data[,cols], G= ngroup, label= labels_factor)

  predicted_labels <- res_MGHD@map  # maximum a posteriori estimates of labels
  predicted_test <- predicted_labels[testSample]

  accuracies[i] <- sum(predicted_test == true_test)/length(testSample) # accuracy
  combos[i,] <- high_distinct_beers
  print(i)
  print(accuracies[i])
}

outmat <- cbind(combos, accuracies)

write.table(outmat, file="random_beer_prediction.txt", row.names=FALSE, col.names=FALSE)
}

rand_preds <- read.table(file="random_beer_prediction.txt")
summary(rand_preds$V7) # distribution of simulated accuracies

mean(rand_preds$V7) # sample avg pred accuracy
var(rand_preds$V7) # sample var pred accuracy

plot(kde(rand_preds$V7), lty = 1, xlab = "Prediction Accuracy", 
     main = "Prediction Acc. Dist. for 200 6-Beer Subsets")
abline(v=.75,col='red')
legend(.23,3.3, legend=c("Distinct Beers"),
       col=c("red"), lty=1, cex=0.8)

########### best beers
best <- rand_preds[which.max(rand_preds$V7),]
best_data <- data[data$Style %in% best[-7],]
row.names(best_data) <- NULL
l <- levels(factor(best_data$Style)) # levels of beer (in words) for converting back
labels_factor <- as.numeric(factor(best_data$Style)) # true labels as numeric
true_labels <- labels_factor
l  #sanity check
unique(labels_factor)
# test set (and train set)
n <- dim(best_data)[1]
testSample <- sample(1:n,ceiling(.8*n))
true_test <- labels_factor[testSample]
labels_factor[testSample] <- 0    # artificially erase labels for test set
cols <- c("LDA1Combination", "LDA2Combination") # LDA vars
res_MGHD <- MGHD(data= best_data[,cols], G= 6, label= labels_factor)
predicted_labels <- res_MGHD@map  # maximum a posteriori estimates of labels
predicted_test <- predicted_labels[testSample]
confus2 <- confusionMatrix(factor(predicted_test, labels = l), factor(true_test,labels = l))
confus2

###################################### END CLASSIFICATION ##############################################################


