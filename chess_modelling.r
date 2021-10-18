
### get some JSONs ###
#install.packages("lme4")
library(jsonlite)
library(lme4)

json1 <- fromJSON("https://api.chess.com/pub/player/hug_mi/games/2021/08", flatten = TRUE)
json2 <- fromJSON("https://api.chess.com/pub/player/hug_mi/games/2021/09", flatten = TRUE)
json3 <- fromJSON("https://api.chess.com/pub/player/hug_mi/games/2021/10", flatten = TRUE)

mydata1 <- rbind(json1$games, json2$games, json3$games)

#################################
######## Main prep ##############
mydata2 <- subset(mydata1 ,select = -c(pgn,tcn, initial_setup, fen, rules, white.uuid,black.uuid , uuid, accuracies.white, accuracies.black))
mydata2 <- mydata2[,-c(8,12)]
colnames(mydata2)

# process dates  midnight games moved to day before
mydata2$datetime <- as.POSIXct(mydata2$end_time , origin="1970-01-01")
mydata2$date  <- as.Date( format( mydata2$datetime, "%Y-%m-%d") )
mydata2$date_num <- mydata2$date
mydata2$hour  <- as.numeric(strftime(mydata2$datetime, format="%H"))
mydata2$date <- ifelse(mydata2$hour < 3 , mydata2$date-1, mydata2$date)
mydata2$datefac <- as.factor(mydata2$date)
mydata2$ID <- c(1:length(mydata2$date))
#mydata2[125:135,]


# Exclusion - keep only up to 17th October for reproducibility 
mydata2 <- mydata2[mydata2$date_num <= as.Date('2021-10-17'),]

# read in & merge up late info
uplate <- read.csv("C:\\Users\\hugh_miller\\Dropbox\\ActuariesDigital\\NormalDeviance\\uplate.csv")
colnames(uplate)[1] <- c("date")
uplate$date <- as.Date(uplate$date)


res_list <- data.frame(
  c("checkmated"   ,      "insufficient"   ,    "repetition"   ,      "resigned"     ,      "stalemate"   ,
    "timeout"     ,       "timevsinsufficient", "win" ),
  c(0,0.5,0.5,0,0.5,0,0.5,1) 
)
colnames(res_list) <- c("result","result2")

# merge on info, create new variables etc
mydata2 <- merge(mydata2,uplate ,by="date", all.x=TRUE, sort=FALSE)
result_list <-  ifelse(mydata2$white.username == "hug_mi", mydata2$white.result, mydata2$black.result)
result_list_opp <-  ifelse(mydata2$black.username == "hug_mi", mydata2$white.result, mydata2$black.result)
rating_opp_ <-  ifelse(mydata2$black.username == "hug_mi", mydata2$white.rating, mydata2$black.rating)
rating_me_ <-  ifelse(mydata2$white.username == "hug_mi", mydata2$white.rating, mydata2$black.rating)
colour_ <-  ifelse(mydata2$white.username == "hug_mi", "W", "B")
mydata2$result <- result_list
mydata2$result_opp <- result_list_opp
mydata2$rating_opp <- rating_opp_
mydata2$rating_diff <- rating_opp_ - rating_me_
mydata2$rating_diff_m60_p60 <- pmin(60, pmax(-60, mydata2$rating_diff))

mydata2$colour <- colour_
summary(as.factor(mydata2$result))
mydata2$hour_band <- ifelse( mydata2$hour >= 22 | mydata2$hour <4, "Late", 
                         ifelse( mydata2$hour < 12, "Morning", 
                                 ifelse ( mydata2$hour <= 17, "Afternoon", "Evening")))

### Merge on result scores 
mydata3 <- merge(mydata2,res_list ,by="result", all.x=TRUE, sort=FALSE)
mydata3 <- mydata3[order(mydata3$datetime),]
mean(mydata3$result2)

#################################
######## Model data   ##############

### Keep key columns and delete draws ###
mydata4 <- subset(mydata3 ,select = c(datefac, result2, colour, rating_opp, Late_fl, 
                                      date_num, rating_diff, rating_diff_m60_p60 , hour_band))
mydata4 <- mydata4[mydata4$result2 != 0.5,]


### Just last month ###
mydata5 <- mydata4[mydata4$date_num > as.Date('2021-09-17') & mydata4$date_num <= as.Date('2021-10-17'),]
fac_new <- droplevels(mydata5$datefac)


#################################
######## Analysis  ##############


############################################
### GLM fit  ####
############################################
fit1b <- glm(result2~colour+rating_opp+Late_fl+hour_band, data=mydata4, family=binomial)
summary(fit1b)
AIC(fit1b)
## Best model - using rating difference with cap/cup ###


fit1e <- glm(result2~colour+rating_diff_m60_p60+Late_fl+hour_band, data=mydata4, family=binomial)
summary(fit1e)
AIC(fit1e)
# not much evidence that the late flag indicates poor performance the day after
# Opponent rating comes through very strongly
# Moderate evidence around time of day differences


############################################
### Mixed model ineffective  ####
############################################

fit2 <- glmer(result2~ colour+rating_diff_m60_p60 + (1 | datefac), family = binomial, data = mydata4, nAGQ =1)
summary(fit2)
# AIC no better, random effect variance fairly unconvincing

############################################
### Simple runs test ####
############################################

# install.packages('snpar')
library(snpar)

# Full time series
runs.test(mydata4$result2, alternative = "two.sided")

# Last month
runs.test(mydata5$result2, alternative = "two.sided")

# Not much evidence of anything going on


############################################
### Bootstrap it instead ####
### Assuming we use daily average as the prediction,
### see how unusual performance is against random allocations (via bootstrap)
############################################


#################### testing allocation of win / losses to days ##############
myfunc_ll <- function(a){
  mu <- mean(a)
  if (mu == 0 || mu == 1) {ll = 0} else {
    ll <-  sum(   a*log(mu) + (1-a)*log(1-mu))
  }
  return(ll)
}

myll <- sum(tapply(mydata5$result2 , fac_new, myfunc))

results_win <- rep(NULL,1000)
set.seed(14563)
for  (i in 1:1000){
  results_win[i] <-  sum(tapply(sample(mydata5$result2) , fac_new, myfunc_ll))
}

hist(results_win)
lines(c(myll,myll) , c(0,1000), col="red")
Fn_dist_win <- ecdf(results_win)
Fn_dist_win(myll)
# Actual appears in the normal range


##### alterative - check for cluster in opponent rating difference across days ###

myfunc_ss <- function(a){
  mu <- mean(a)
  ll <- sum((a-mu)^2)
  return(ll)
}

fac_new <- droplevels(mydata5$datefac)
my_opp_diff_ss <- sum(tapply(mydata5$rating_diff , fac_new, myfunc_ss))

results_diff <- rep(NULL,1000)
set.seed(14563)
for  (i in 1:1000){
  results_diff[i] <-  sum(tapply(sample(mydata5$rating_diff) , fac_new , myfunc_ss))
}

hist(results_diff)
lines(c(my_opp_diff_ss,my_opp_diff_ss) , c(0,1000), col="red")
Fn_dist <- ecdf(results_diff)
Fn_dist(my_opp_diff_ss)
my_opp_diff_ss
# Again, not much there

### As a sense check is there clustering in opponent rating
### This should see strong clustering as my rating fluctuates  ###

my_opp_ss <- sum(tapply(mydata5$rating_opp , fac_new, myfunc_ss))

results_rating <- rep(NULL,1000)
set.seed(14563)
for  (i in 1:1000){
  results_rating[i] <-  sum(tapply(sample(mydata5$rating_opp) , fac_new , myfunc_ss))
}

hist(results_rating)
lines(c(my_opp_ss,my_opp_ss) , c(0,1000), col="red")
Fn_dist <- ecdf(results_rating)
Fn_dist(my_opp_ss) # indeed - very low implied p-value
my_opp_ss
# Yes, definitely variation in rated opoonent by day
