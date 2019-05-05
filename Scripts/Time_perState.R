## functions gives total time spent doing a behavior while in a given predator state. Function was originally designed for grooming only and later generalized.

model_dataset <- function (h, g) {
  samplesize <- length(unique(dataset$file.num))
  dur.grooming <- function (x) {
  dur.groom <- sum(dataset$duration[dataset$state == h
  & dataset$file.num == x & (dataset$pred.state == "far" | dataset$pred.state == "close") 
  & dataset$predator == g])
  dur.groom}
  
  dur.groom <- sapply(1:samplesize, dur.grooming)
  
  dur.grooming.np <- function (x) {
  dur.groom <- sum(dataset$duration[dataset$state == h 
  & dataset$file.num == x & dataset$pred.state == "0" 
  & dataset$predator == g])
  dur.groom}
  
  dur.groom.np <- sapply(1:samplesize, dur.grooming.np)
  
  
  ## Total time spent in a given predator state
  total.time <- function (x) {
  total <- sum(dataset$duration[(dataset$pred.state == "far" | dataset$pred.state == "close") & dataset$file.num == x & dataset$predator == g])
  total}
  grooming.time <- sapply(1:samplesize, total.time)
  
  total.time.np <- function (x) {
  total <- sum(dataset$duration[dataset$pred.state == "0" & dataset$file.num == x & dataset$predator == g])
  total}
  grooming.time.np <- sapply(1:samplesize, total.time.np) 
  
  ## Sex of select individual
  sex <- function (x) {
  y <- dataset$sex[dataset$pred.state == "0" & dataset$file.num == x & dataset$predator == g][1]
  y}
  
  sex.np <- sapply(1:samplesize, sex)
  
  sex.pred <- function (x) {
  y <- dataset$sex[(dataset$pred.state == "far" | dataset$pred.state == "close") & dataset$file.num == x & dataset$predator == g][1]
  y}
  
  sex.p <- sapply(1:samplesize, sex)
  
  Sex <- c(as.character(sex.np), as.character(sex.p))
  
  ## date on which assay was perfomed 
  date <- function (x) { 
    file <- as.character(dataset$date[dataset$predator == g & dataset$file.num == x][1])
    file}
  date(1)
  dat <- sapply(1:samplesize, date)
  Date <- c(dat,dat)
  
  ## time of day at wich video recording was done
  rec <- function (x) { 
    rec <- as.character(dataset$rec.time[dataset$predator == g & dataset$file.num == x][1])
    rec}
  rec.t <- sapply(1:samplesize, rec)
  rec.time <- c(rec.t, rec.t)
  
  ## Temp during recording 
  temp <- function (x) {
    t <- dataset$temp[dataset$predator == g & dataset$file.num == x][1]}
  tempe <- sapply(1:samplesize, temp)
  Temp <- c(tempe, tempe)
  
  age1 <- function (x) {
    t <- dataset$age[dataset$predator == g & dataset$file.num == x][1]}
  age <- sapply(1:samplesize, age1)
  Age <- c(age, age)
  
  ## Humidity
  
  humidity <- function (x) {
    y <- dataset$humidity[dataset$pred.state == "0" & dataset$file.num == x & dataset$predator == g][1]
    y}
  humidity.np <- sapply(1:samplesize, humidity)
  
  humidity.pred <- function (x) {
    y <- dataset$humidity[(dataset$pred.state == "far" | dataset$pred.state == "close") & dataset$file.num == x & dataset$predator == g][1]
    y}
  humidity.p <- sapply(1:samplesize, humidity.pred)
  
  Humidity <- c(as.character(humidity.np), as.character(humidity.p))
  
  ## Combining them all into one dataframe
  total.dur <- c(dur.groom.np, dur.groom)
  behav <- rep(h, 2*samplesize)
  time <- c(grooming.time.np, grooming.time)
  pred.type <- rep(g, 2*samplesize)
  pred.state <- c(rep("absent", samplesize), rep("present", samplesize))
  file.num <- c(c(1:samplesize), c(1:samplesize))
  Sex
  Date
  Temp
  Age
  
  ## Making a dataframe
  spiderdata.grooming <- data.frame(file.num, Sex, Age, Date, rec.time, Humidity, Temp, behav, total.dur, time, pred.type, pred.state)
  return (spiderdata.grooming)}

############# END OF FUNCTION #################
