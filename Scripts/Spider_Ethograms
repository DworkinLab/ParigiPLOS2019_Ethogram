########### Ethogram ############

### This code can be used to make all spider ethograms using the dataset Spider_Ethograms.csv 
### Please set your path to the right file: Spider_Ethograms.csv 
spiderdata <- read.csv("~/../../Spider_Ethogram.csv")

## Remove unwated columns from the dataset
spiderdata$X <- NULL
spiderdata$humidity <- NULL

## Videos were analyzed at 50% speed and timestamps are in ms. Therefore we need to do some math to convert to seconds
spiderdata$sec.timestamp <- (spiderdata$time.stamp/2)
spiderdata$sec.timestamp <- (spiderdata$sec.timestamp/1000)

## File numbers are coded as numeric. But we need them to be coded as a factor
spiderdata$file.num <- factor(spiderdata$file.num)

## Extracting duration from timestamps
timestamp <- spiderdata$sec.timestamp
states <- spiderdata$state
duration <- rep(NA, length(states))
for (i in 1:length(timestamp)) { duration[i] <- (timestamp[i+1] - timestamp[i])}
spiderdata$duration <- duration

## There is a "behavior" called EOF that stands for End Of File. This needs to be recoded and assigned a duration of 0s 
## If not done, you will get a negative duration at EOF
for (i in 1:length(spiderdata$state.event)) { 
  if (spiderdata$state.event[i] == "end_trial") {spiderdata$duration[i] <- 0}
}

## This makes sure that the time we spent putting in the spider is taken out of the analysis
spiderdata$pred.state <- as.character(spiderdata$pred.state)
for (i in 1:length(spiderdata$code)) { 
  if (spiderdata$code[i] == "1") {spiderdata$pred.state[i] <- "entered"}
}                                         

## Walking was originally divided into inching and walking. This for-loop removes inching and changing it to walking
for (i in 1: length(spiderdata$state)) {
  if (spiderdata$state[i] == "inch") {spiderdata$state[i] <- "walk"}
}

## This step is necessary for what we will do next
spiderdata$state <- as.character(spiderdata$state) 
spiderdata$event <- as.character(spiderdata$event)

## Assigning more descriptive names to behaviors

for (i in 1: length(spiderdata$state)) {
  if (spiderdata$state[i] == "rev_walk") {spiderdata$state[i] <- "retreat"}
}

for (i in 1: length(spiderdata$state)) {
  if (spiderdata$state[i] == "timeout") {spiderdata$state[i] <- "occl"}
}

for (i in 1: length(spiderdata$event)) {
  if (spiderdata$event[i] == "ab_lift") {spiderdata$event[i] <- "ab"}
}

for (i in 1: length(spiderdata$event)) {
  if (spiderdata$event[i] == "wing_disp") {spiderdata$event[i] <- "wd"}
}

# Changing factor levels to make them appear in the order I want on the ethograms. 

spiderdata$state <- factor(spiderdata$state, levels = c("occl", "retreat", "stop", "run", "walk", "groom"))
spiderdata$event <- factor(spiderdata$event, levels = c("capture","jump", "fly", "wd", "ab", "turn", "pause", "end_trial", "0"))

## Our spider videos were recorded for a slightly longer time than the mantid videos. This code cuts off the recording at 5 mins

for (i in 1: length(spiderdata$code)) {
  if (spiderdata$code[i] == "1" & spiderdata$sec.timestamp[i] > 300) {spiderdata$sec.timestamp[i] <- 300}}

for (i in 1: length(spiderdata$code)) {
  if (spiderdata$pred.state[i] == "0" & spiderdata$sec.timestamp[i] > 300) {spiderdata$pred.state[i] <- "entered"}}


for(i in 1: length(spiderdata$pred.state)) {
  if (spiderdata$pred.state[i] == "entered" & spiderdata$sec.timestamp[i] > 300) {spiderdata$predator[i] <- NA}
}
spiderdata <- na.omit(spiderdata)

## Now I need to reconvert time stamps to durations
for (i in 1: length(spiderdata$sec.timestamp)) {
  if (i < length(spiderdata$sec.timestamp)) {
    if (spiderdata$pred.state[i] != "0" & spiderdata$pred.state[i+1] != "0") {spiderdata$sec.timestamp[i+1] <- spiderdata$sec.timestamp[i] + spiderdata$duration[i]}
  }}

## Just capitalzing sex to make it look pretty on the figures
spiderdata$sex <- as.character(spiderdata$sex)
for (i in 1: length(spiderdata$sec.timestamp)) {
  if (spiderdata$sex[i] == "male") {spiderdata$sex[i] <- "Male"}
  if (spiderdata$sex[i] == "female") {spiderdata$sex[i] <- "Female"}
}

## Ending at 920s
for (i in 1: length(spiderdata$file)){
  if (spiderdata$sec.timestamp[i] > 920) {spiderdata$sec.timestamp[i] <- NA}
}
spiderdata <- na.omit(spiderdata)



######## FUNCTION THAT MAKES ETHOGRAMs WITH SPIDER DATA ############

ethogram.plotting <- function(e) {
  par(family = "", mfcol= c(2,1), oma = c(5,1,0,0.01), mar = c(0,5,0.1,0.01))
  
  time.stamp <- spiderdata$sec.timestamp[spiderdata$file.num == e  & spiderdata$event != "0" & spiderdata$event != "end_trial"]
  time.stamp3 <- spiderdata$sec.timestamp[spiderdata$event != "0" & spiderdata$event != "end_trial"]
  
  event.names <- factor(spiderdata$event[ spiderdata$event != "0" & spiderdata$event != "end_trial" ])
  events <- factor(spiderdata$event[spiderdata$file.num == e & spiderdata$event != "0" & spiderdata$event != "end_trial" ], levels = c("capture","jump", "fly", "wd", "ab", "turn", "pause", "end_trial", "0") )
  pred.in <- spiderdata$sec.timestamp[spiderdata$code == "1" & spiderdata$file.num == e]
  
  plot(rep(1, length(events)) ~ time.stamp, ylim=c(0, 9), xlim=c(0, 920), type="n", ann=F, yaxt="n",xaxt='n', bty = "n")
  rect(pred.in,-1.5, 920,7.2, border= NA, col = "gray84") # to keep white space between predator state and others, change -1 to 0
  rect(pred.in,-1,0,7.2, border = NA, col = "gray95")
  
  for (i in 1:length(events))  {
    ytop <- as.numeric(events[i])
    ybottom <- ytop - 0.5
    rect(xleft=time.stamp[i], xright= time.stamp[i],
    ybottom=ybottom, ytop=ytop, col = ytop)
  }
  
  
  axis(side=2, las = 1, at = (1:length(levels(event.names)) -0.25), labels=levels(event.names), line = -1.1, cex.axis = 1.5, lwd = 1) 
  lines (x =c(0,0), y = c(-5, 7.2), lwd = 2.2)
  
  text (840,6.5, labels = "Spider", font=2, cex = 1.5)
  
  # text ((900-65),8.7, labels = unique(spiderdata$sex[spiderdata$file.num == e]), font=1)
  # text ((900-85),8.1, labels = unique(spiderdata$temp[spiderdata$file.num == e]), font=1)
  # text ((900-50),8.1, labels = "C", font=1)
  # text ((900-90),7.55, labels = unique(spiderdata$age[spiderdata$file.num == e]), font=1)
  # text ((900-50),7.5, labels = "days", font=1)
  
  states <- spiderdata$state[spiderdata$file.num == e]
  state.names <- factor(spiderdata$state)
  time.stamp2 <- spiderdata$sec.timestamp[spiderdata$file.num == e]
  
  plot(rep(1, length(states)) ~ time.stamp2, ylim=c(0, (1+nlevels(state.names))), xlim=c(0, 920), type="n", ann=F, yaxt="n", xaxt = "n", xlab = "time", bty= "n")
  rect(pred.in,0,920,7.05, border = NA, col = "gray84") ## if you want the white space between predator state and others, change 7 to 6.75
  rect(pred.in,0,min(time.stamp3),7.05, border = NA, col = "gray95")
  for (i in 1:length(states)){
    ytop <- as.numeric(states[i])
    ybottom <- ytop - 0.5
    rect(xleft=time.stamp2[i], xright=time.stamp2[i+1],
    ybottom=ybottom, ytop=ytop, col = "black")}
  
  axis(side=2, las = 1, at = (1:length(levels(state.names)) -0.25), labels=levels(state.names), line = -1.1, cex.axis = 1.5, lwd = 1) 

  lines (x =c(0,0), y = c(0, 50), lwd = 2.2)
  lines (x =c(0,920), y = c(0, 0), lwd = 2.2)
  
  ###### Adding predator data onto the ethogram ###### 
  spiderdata.again <- spiderdata[spiderdata$file.num == e,]
  pred.crap <- data.frame( pred.state= spiderdata.again$pred.state[ spiderdata.again$pred.state == "close"| spiderdata.again$pred.state == "far" ], sec.timestamp= spiderdata.again$sec.timestamp[ spiderdata.again$pred.state == "close" | spiderdata.again$pred.state == "far"] )
  pred.crap$colour <- sub( "close", "black", pred.crap$pred.state )
  pred.crap$colour <- sub( "far", "gray84", pred.crap$colour )
  
  ytop <- 7.5
  ybottom <- ytop - 0.4
  for (i in 1:nrow(pred.crap)){
    rect(xleft= pred.crap$sec.timestamp[i], 
    xright= pred.crap$sec.timestamp[i+1],
    ybottom=ybottom, ytop=ytop, 
    col = pred.crap$colour[i] , 
    border = pred.crap$colour[i])}
  
  axis(side=2, las = 1, at = 7.16, labels = "Spider location", line = -1.1,  cex.axis = 1, font =3, lwd = 1)}

## For Figure 1a
pdf()
ethogram.plotting(31)
dev.off()
setwd("~/Desktop")


### For S1 File
pdf()                       
all.flies <- rep( NA, 30 )
for ( i in 1:30 ) {
  all.flies[i] <- ethogram.plotting( levels(spiderdata$file.num )[i])
}
dev.off()


## Saving all ethograms as an esp
setEPS()

postscript("~/Desktop/", width = 10, height = 6, onefile = F, family = "Times", pointsize = 12)

setEPS()

postscript("~/Desktop/Figure1a.eps", pointsize = 12)

ethogram.plotting(1)

dev.off()
