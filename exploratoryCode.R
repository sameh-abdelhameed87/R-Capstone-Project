library(jsonlite)
library(dplyr)
library(ggplot2)

# Extract Data and loads it
dataFile <- "yelp_dataset_challenge_academic_dataset.zip"
unzip(dataFile)
dataFile <- "./yelp_dataset_challenge_academic_dataset"
fileList <- list.files("./yelp_dataset_challenge_academic_dataset")


# Data files' locations
fileBusiness <- "./yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_business.json"
fileCheckIn <- "./yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_checkin.json"
fileReview <- "./yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_review.json"
fileTip <- "./yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_Tip.json"
fileUser<- "./yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_user.json"

# Function to prepare and read the JSON data files
ReadData <- function(fileLocation, numberOfLines = -1L) {
  con <- file(fileLocation, open = "r")
  readJSON <- readLines(con, n = numberOfLines)
  readJSON <- paste("[", paste(readJSON, collapse = ","), "]")  # To make JSON array
  close(con)
  fromJSON(readJSON)
  }

dataBusiness <- ReadData(fileBusiness)
dataCheckIn <- ReadData(fileCheckIn)
dataTip <- ReadData(fileTip)
dataUser <- ReadData(fileUser)
dataReview <- readData(fileReview)
  
##################################################################################

# Find the total number of lines in the review file
lineNumber <- 0
con <- file(fileReview, open = "r") # opens a file connection
while( length(readLines(con, n=1)) > 0 ) lineNumber <- lineNumber+1;
close(con)
lineNumber
noOfReviews <- 1569264

# Find the total number of 5 star review.

noOfReviews <- 1569264 # number of reviews in data file
fiveStarNumber <- 0 

con <- file(fileReview, open = "r")
for( i in 1:noOfReviews ) {
    if (fromJSON(readLines(con, n= 1, warn = FALSE))$stars == 5) fiveStarNumber <- fiveStarNumber+1
}
close(con)
fiveStarNumber/noOfReviews

# Find the total number of lines in the business file

lineNumber <- 0 
con <- file(fileBusiness, open = "r")
while( length(readLines(con, n = 1)) > 0 ) lineNumber <- lineNumber+1;
close(con)
lineNumber
noOfBusinesses <- 61184

# Find the total number of lines in the tip file
lineNumber <- 0
con <- file(fileTip, open = "r") # opens a file connection
while( length(readLines(con, n=1)) > 0 ) lineNumber <- lineNumber+1;
close(con)
lineNumber
noOfTip <- 495107

# Find the number of users in the data.
lineNumber <- 0
con <- file(fileUser, open = "r") # opens a file connection
while( length(readLines(con, n=1)) > 0 ) lineNumber <- lineNumber+1;
close(con)
lineNumber
noOfUsers <- 366715


# Find user with >10,000 funny votes
noOfUsers <- 366715
userNames <- {}

con <- file(fileUser, open = "r")
for(i in 1:366715) {
  temp <- fromJSON(readLines(con, n = 1))
  
  if(!is.null(temp$compliments$funny)) {
    if(temp$compliments$funny >10000) userNames <- c(userNames, temp$name)
  }
}
close(con)



# Collate data of users with >1 fan or funny vote.
noOfUsers <- 366715
userNames <- {}
fanFunnyTable <- data.frame(fan = {}, funny = {})
con <- file(fileUser, open = "r")
for(i in 1:366715) {
  temp <- fromJSON(readLines(con, n = 1))

  if(!is.null(temp$compliments$funny)) {
    if(temp$compliments$funny >1) tempFunny <- TRUE else tempFunny <- FALSE
  } else tempFunny <- FALSE
  
  if(!is.null(temp$fans)) {
    if(temp$fans >1) tempFans <- TRUE else tempFans <- FALSE
  } else tempFans <- FALSE
  
  fanFunnyTable <- rbind(fanFunnyTable, c(tempFans, tempFunny))
}
close(con)



# Collects sample data of the first 100 observations from all the files. 
con <- file(fileBusiness, open = "r" )
sampleBusiness<- readLines(con, n = 100)
sampleBusiness <- paste("[", paste( sampleBusiness, collapse = ","), "]")
close(con)


con <- file(fileCheckIn, open = "r" )
sampleCheckIn<- readLines(con, n = 100)
sampleCheckIn <- paste("[", paste( sampleCheckIn, collapse = ","), "]")
close(con)

con <- file(fileReview, open = "r" )
sampleReview<- readLines(con, n = 100)
sampleReview <- paste("[", paste( sampleReview, collapse = ","), "]")
close(con)

con <- file(fileTip, open = "r" )
sampleTip <- readLines(con, n = 100)
sampleTip <- paste("[", paste( sampleTip, collapse = ","), "]")
close(con)

con <- file(fileUser, open = "r" )
sampleUser<- readLines(con, n = 100)
sampleUser <- paste("[", paste( sampleUser, collapse = ","), "]")
close(con)


sampleCheckIn <- fromJSON(sampleCheckIn)
sampleReview <- fromJSON(sampleReview)
sampleTip <- fromJSON(sampleTip)
sampleUser <- fromJSON(sampleUser)
sampleBusiness <- fromJSON(sampleBusiness)

View(sampleCheckIn)
View(sampleReview)
View(sampleTip)
View(sampleUser)
View(sampleBusiness)


####################################################################################
####################################################################################


# Ranks the users according to the total number of votes they received.

totalVotes <- rowSums(dataUser$votes) ## Computes the total number of votes
rankVotes <- ntile(totalVotes, n = 10) ## Ranks data according to deciles
topDecile <- rankVotes == 10
qplot(x=factor(rankVotes), y= log10(totalVotes+1), geom = "jitter", color = factor(rankVotes))+ 
  geom_boxplot()+
  xlab("Decile")+
  ylab("Total Vote (log)")+
  ggtitle("Number of votes by Decile")##Plots the data according to decile



# Finds the indices of the reviews that were made by the top decile.
topDecileReviewInd <- rep(FALSE, 1569264)
topDecileID <- dataUser$user_id[topDecile]
tStart <- proc.time()
con <- file(fileReview, open = "r")
line <- 0 
while(line < 1569264) {
  temp <- data.frame(fromJSON(readLines(con, n = 1)))
  line <- line+1
  if(temp$user_id %in% topDecileID){
    topDecileReviewInd[line] <- TRUE
  }
  if(line %in% c(10000, 50000,  100000, 200000, 400000, 600000, 800000, 1000000, 120000)) {
    print(proc.time()-tStart)}
  
}
tEnd <- proc.time()
tEnd -tStart
close(con)
rm("temp", "line")

sum(topDecileReviewInd)


# Find of business IDs, user_ids, dates and star rating of all reviews.

reviewsUserID <- rep("",1569264 )
reviewsBusinessID <- rep("",1569264 )
reviewsDate <- rep("",1569264 )
reviewsStars <- rep(0,1569264 )

tStart <- proc.time()
con <- file(fileReview, open = "r")
line <- 0 

while(line < 1569264) {
  
  temp <- data.frame(fromJSON(readLines(con, n = 1)))
  line <- line+1
  reviewsUserID[line] <- as.character(temp$user_id)
  reviewsBusinessID[line] <- as.character(temp$business_id[[1]])
  reviewsDate[line] <- as.character(temp$date[[1]])
  reviewsStars[line] <- temp$stars[[1]]
  
  if(line %in% c(10000, 50000,  100000, 200000, 
                 400000, 600000, 800000, 1000000, 120000)) {
    print(proc.time()-tStart)}
  
}

tEnd <- proc.time()
tEnd -tStart
close(con)
rm("temp", "line")





#find the number of checkins per business for top decile
topBusinessCheckInsIndex <- dataCheckIn$business_id %in% topBusinessID
topBusinessTotalCheckIns <- {}
for(i in 1:sum(topBusinessCheckInsIndex)) {
  ind <- !is.na(as.vector(topBusinessCheckIns$checkin_info[i,]))
  topBusinessTotalCheckIns <- c(topBusinessTotalCheckIns,
                                sum(as.vector(topBusinessCheckIns$checkin_info[i,])[ind]))
}

rm("ind")
  

#find the total number of checkins for every business
totalCheckin <- {}
for(i in 1:length(dataCheckIn$business_id)){
  totalCheckin <- c(totalCheckin,
                    sum(na.omit(unlist(dataCheckIn$checkin_info[i,]))))
}


#pick a random sample, find the total number of  reviews for the random sample 
#and find the total number of check ins.
randSample <- sample(1:1569264, sum(topDecileReviewInd)) ## Pick random sample of same size as top decile
randSampleBusinessID <- reviewsBusinessID[randSample] ## biz ids in rand samp
randSampleBusinessID <- unique(randSampleBusinessID) ## pick out unique ids

topDecileBusinessID <- unique(reviewsBusinessID[topDecileReviewInd]) ##businesses reviewed by top dec

tempInd <- reviewsBusinessID %in% randSampleBusinessID 
randomSampleReviewNo <- reviewsBusinessID[tempInd] 
randomSampleReviewNo <- table(as.factor(randomSampleReviewNo)) ##no of reviews per business in sample    


topBusinessReviewNo <-  reviewsBusinessID[topDecileReviewInd] #subset top decile
topBusinessReviewNo <- table(as.factor(topBusinessReviewNo)) #row names are business ids.
aveRandSampRevNo <- sum(randomSampleReviewNo)/length(randomSampleReviewNo) ##total/(review number)
aveTopRevNo <- sum(topBusinessReviewNo)/length(topBusinessReviewNo) ##total/(review number)

tempInd <- dataCheckIn$business_id %in% randSampleBusinessID
randSampleCheckIn <- totalCheckin[tempInd] ## Total Checkins of rand samp business

tempInd <- dataCheckIn$business_id %in% topDecileBusinessID
topDecileCheckIn <- totalCheckin[tempInd] ## Total Checkins of top decile business

aveRandSampCheckIn <- sum(randSampleCheckIn)/
  length(randSampleBusinessID) ## mean checkins per business in rand samp

aveTopDecCheckIn <- sum(topDecileCheckIn)/ 
  length(unique(reviewsBusinessID[topDecileReviewInd]))  ## mean checkins per business in top dec

randSampRevStars <- reviewsStars[randSample]
aveRandSampRevStars <- sum(randSampRevStars)/
  sum(topDecileReviewInd) ## mean stars per review

topDecRevStars <- reviewsStars[topDecileReviewInd]
aveTopDecStars <- sum(topDecRevStars)/
  sum(topDecileReviewInd) ##mean stars per review

rm("tempInd")

# Tabulate the number of reviews 3 months before and after a celebrity review is posted.  

topBusinessReviewNo <-  reviewsBusinessID[topDecileReviewInd] #subset top decile
topBusinessReviewNo <- table(as.factor(topBusinessReviewNo)) #row names are business ids.


topDecileNo <- sum(topDecileReviewInd)
topBusinessBefore <- rep(0,topDecileNo)
topBusinessAfter <- rep(0,topDecileNo)

reviewsDate <- as.Date(reviewsDate)
topBusinessDate <- reviewsDate[topDecileReviewInd]
topBusinessID <- reviewsBusinessID[topDecileReviewInd]
topBusinessIDUnique <- unique(topBusinessID)

tStart <- proc.time()
line <- 0
while(line < 5000 ){
  line <- line+1
  tempInd <- which(reviewsBusinessID == topBusinessIDUnique[line])
  tempInd1 <- which(topBusinessID == topBusinessIDUnique[line])
  
  for(i in tempInd){
    tempInd2 <- which(topBusinessDate[tempInd1] > (reviewsDate[i]-90) & 
                        topBusinessDate[tempInd1] < reviewsDate[i])
    
    topBusinessAfter[tempInd1[tempInd2]] <- topBusinessAfter[tempInd1[tempInd2]]+1
    
    tempInd2 <- which(topBusinessDate[tempInd1] < (reviewsDate[i]+90) & 
                        topBusinessDate[tempInd1] > reviewsDate[i])
    
    topBusinessBefore[tempInd1[tempInd2]] <- topBusinessBefore[tempInd1[tempInd2]]+1
  }
  
  if(line %in% c(100,1000, 5000,  10000, 20000, 
                 40000)) {
    print(proc.time()-tStart)}
  
}


# Find correlation between score and checkins


## Generates data frame for random sample with stars, checkins and business ids.
tempStars <- rep(NA, length(randSampleBusinessID))
tempCheckins <- rep(NA, length(randSampleBusinessID))

for(i in 1:length(dataCheckIn$business_id)){
  if(dataCheckIn$business_id[i] %in% randSampleBusinessID){
    temp <- which(randSampleBusinessID == dataCheckIn$business_id[i])
    tempCheckins[temp] <- totalCheckin[i]
  }
}

for(i in 1:length(dataBusiness$business_id)){
  if(dataBusiness$business_id[i] %in%  randSampleBusinessID){
    temp <- which(randSampleBusinessID == dataBusiness$business_id[i])
    tempStars[temp] <- dataBusiness$stars[i]
  }
}

randSampDF <- data.frame(stars= tempStars, checkins = tempCheckins, businessID = randSampleBusinessID)
randSampDF <- na.omit(randSampDF)


## Generates data frame for random sample with stars, checkins and business ids.

tempStars <- rep(NA, length(randSampleBusinessID))
tempCheckins <- rep(NA, length(randSampleBusinessID))

for(i in 1:length(dataCheckIn$business_id)){
  if(dataCheckIn$business_id[i] %in% topDecileBusinessID){
    temp <- which(topDecileBusinessID == dataCheckIn$business_id[i])
    tempCheckins[temp] <- totalCheckin[i]
  }
}

for(i in 1:length(dataBusiness$business_id)){
  if(dataBusiness$business_id[i] %in%  topDecileBusinessID){
    temp <- which(topDecileBusinessID == dataBusiness$business_id[i])
    tempStars[temp] <- dataBusiness$stars[i]
  }
}

topDecDF <- data.frame(stars=tempStars, checkins=tempCheckins, businessID = topDecileBusinessID)
topDecDF <- na.omit(topDecDF)

rm("temp", "tempStars", "tempCheckins")



# inds average checkins by star rating,  plots graph, and generates linear model for random sample
groupRandSampDF <- group_by(randSampDF, stars)
randSampSummary <- summarise(groupRandSampDF,totalcheckins = sum(checkins)/length(checkins))
qplot(x = stars, y = log(totalcheckins), data = randSampSummary, geom =  "smooth") +geom_point()
randSampFit <- lm(totalcheckins ~ stars, data = randSampSummary  )
summary(randSampFit)
plot(stars, resid(randSampFit))


#Finds average checkins by star rating,  plots graph, and generates linear model for Celebrity group

groupTopDecDF <- group_by(topDecDF, stars)
topDecSummary <- summarise(groupTopDecDF,totalcheckins = sum(checkins)/length(checkins))
qplot(x = stars, y = log(totalcheckins), data = topDecSummary, geom =  "smooth") +geom_point()
topDecFit <- lm(totalcheckins ~ stars, data = topDecSummary)
summary(topDecFit)
plot(topDecDF$stars, resid(topDecFit))


# Splits tabulated reviews 3 months before/after into good and bad reviews,
# then performs a paired t test.
temp <- which(topBusinessID %in% topBusinessIDUnique[1:5000])
beforeAfterDF <- data.frame(before = topBusinessBefore[temp], 
                            after = topBusinessAfter[temp],
                            stars = reviewsStars[topDecileReviewInd][temp])

goodDFInd <- beforeAfterDF$stars %in% c(4,5)
badDFInd <- beforeAfterDF$stars %in% c(1,2)

goodDF <- beforeAfterDF[goodDFInd,]
badDF <- beforeAfterDF[badDFInd,]

t.test(goodDF$before, goodDF$after, paired = TRUE)
t.test(badDF$before, badDF$after, paired = TRUE)