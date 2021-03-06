### This code can be used to make all Mantid ethograms using the dataset Mantid_Ethograms.csv 
### Please set your path to the right file: Mantid_Ethograms.csv 


mantiddata <- read.csv(file = "../../Mantid_Ethograms.csv", header = T)

setwd("~/Desktop")

## Processing mantid dataset for ethogram function

## Videos were analyzed at 50% speed and timestamps are in ms. Therefore we need to do some math to convert to seconds
mantiddata$sec.timestamp <- (mantiddata$time.stamp/2) 
mantiddata$sec.timestamp <- (mantiddata$sec.timestamp/1000) 

## File numbers are coded as numeric. But we need them to be coded as a factor
mantiddata$file.num <- factor(mantiddata$file.num)


## Adding a column with durations as with spider data -- see Spider_Ethogram.R
timestamp <- mantiddata$sec.timestamp
states <- mantiddata$state
duration <- rep(NA, length(states))
for (i in 1:length(timestamp)) { duration[i] <- (timestamp[i+1] - timestamp[i])}
mantiddata$duration <- duration

# If not done, gives a negative duration at EOF. messes up spider present data -- also see Spider_Ethogram.R
for (i in 1:length(mantiddata$state)) { 
  if (mantiddata$state[i] == "end_trial") {mantiddata$duration[i] <- 0}
}

## Chainging all stopping states to events called pausing. Because stopping that it transition (i.e. less than 1s is called pausing in our study)
mantiddata$event <- as.character(mantiddata$event)
mantiddata$state <- as.character(mantiddata$state) 
for (i in 1:length(mantiddata$state)) {
  if (mantiddata$state[i] == "stop" & ((mantiddata$sec.timestamp[i+1] - mantiddata$sec.timestamp[i]) < 1) 
  & (mantiddata$event[i] == "0")) {mantiddata$event[i] <- "pause"}}
for (i in 1:length(mantiddata$state)){
  if (mantiddata$state[i] == "stop" & mantiddata$event[i] == "pause") {mantiddata$state[i] <- mantiddata$state [i-1]}}


## This makes sure that the  time we spent putting in the spider is taken out of the analysis
mantiddata$pred.state <- as.character(mantiddata$pred.state)
for (i in 1:length(mantiddata$code)) { 
  if (mantiddata$code[i] == "1") {mantiddata$pred.state[i] <- "entered"}}


## Assigning more descriptive names to behaviors
for (i in 1: length(mantiddata$state)) {
  if (mantiddata$state[i] == "rev_walk") {mantiddata$state[i] <- "retreat"}
}

for (i in 1: length(mantiddata$state)) {
  if (mantiddata$state[i] == "timeout") {mantiddata$state[i] <- "occl"}
}

for (i in 1: length(mantiddata$event)) {
  if (mantiddata$event[i] == "ab_lift") {mantiddata$event[i] <- "ab"}
}

for (i in 1: length(mantiddata$event)) {
  if (mantiddata$event[i] == "wing_disp") {mantiddata$event[i] <- "wd"}
}


# Changing factor levels to make them appear in the order I want on the ethograms. 
mantiddata$state <- factor(mantiddata$state, levels = c("occl", "retreat", "stop", "run", "walk", "groom"))
mantiddata$event <- factor(mantiddata$event, levels = c("capture","jump", "fly", "wd", "ab", "turn", "pause", "end_trial", "0"))

## Ending the assay at 920 s
for (i in 1: length(mantiddata$state)) {
  if (mantiddata$sec.timestamp[i] >920) {mantiddata$sec.timestamp[i] <- 920}
}




######## ETHOGRAM WITH MANTID DATA ############

ethogram.plotting <- function(e) {
  par(family = "", mfcol= c(2,1), oma = c(5,1,0,0.01), mar = c(0,5,0.1,0.01))
  
  time.stamp <- mantiddata$sec.timestamp[mantiddata$file.num == e  & mantiddata$event != "0" & mantiddata$event != "end_trial"]
  time.stamp3 <- mantiddata$sec.timestamp[mantiddata$event != "0" & mantiddata$event != "end_trial"]
  
  event.names <- factor(mantiddata$event[ mantiddata$event != "0" & mantiddata$event != "end_trial" ])
  events <- factor(mantiddata$event[mantiddata$file.num == e & mantiddata$event != "0" & mantiddata$event != "end_trial" ], levels = c("capture","jump", "fly", "wd", "ab", "turn", "pause", "end_trial", "0") )
  pred.in <- mantiddata$sec.timestamp[mantiddata$code == "1" & mantiddata$file.num == e]
  
  plot(rep(1, length(events)) ~ time.stamp, ylim=c(0, 9), xlim=c(0, 920), type="n", ann=F, yaxt="n",xaxt='n', bty = "n")
  rect(pred.in,-1,max(time.stamp3),7.2, border= NA, col = "gray84") # to keep white space between predator state and others, change -1 to 0.
  rect(pred.in,-1,0,7.2, border = NA, col = "gray94")
  
  for (i in 1:length(events))  {
    ytop <- as.numeric(events[i])
    ybottom <- ytop - 0.5
    rect(xleft=time.stamp[i], xright= time.stamp[i],
    ybottom=ybottom, ytop=ytop, col = ytop)}
  
  
  axis(side=2, las = 1, at = (1:length(levels(event.names)) -0.25), labels=levels(event.names), line = -1.1, cex.axis = 1.5, lwd = 1) 
  lines (x =c(0,0), y = c(-5, 7.2), lwd = 2.2)
  
  text (840,6.5, labels = "Mantid", font=2, cex = 1.5)
  #text (60,7.5, labels = "Mantid", font=2, cex = 1.2)
  #text ((900-65),8.7, labels = unique(mantiddata$sex[mantiddata$file.num == e]), font=1)
  #text ((900-85),8.1, labels = unique(mantiddata$temp[mantiddata$file.num == e]), font=1)
  #text ((900-50),8.1, labels = "C", font=1)
  #text ((900-90),7.55, labels = unique(mantiddata$age[mantiddata$file.num == e]), font=1)
  #text ((900-50),7.5, labels = "days", font=1)
  
  states <- mantiddata$state[mantiddata$file.num == e]
  state.names <- factor(mantiddata$state)
  time.stamp2 <- mantiddata$sec.timestamp[mantiddata$file.num == e]
  
  plot(rep(1, length(states)) ~ time.stamp2, ylim=c(0, (1+nlevels(state.names))), xlim=c(0, 920), type="n", ann=F, yaxt="n", xaxt = "n", xlab = "time", bty= "n")
  rect(pred.in,0,920,7.05, border = NA, col = "gray84") ## if you want the white space between predator state and others, change 7 to 6.75
  rect(pred.in,0,0,7.05, border = NA, col = "gray95")
  for (i in 1:length(states))  {
    ytop <- as.numeric(states[i])
    ybottom <- ytop - 0.5
    rect(xleft=time.stamp2[i], xright=time.stamp2[i+1],
    ybottom=ybottom, ytop=ytop, col = "black")}
  
  axis(side=2, las = 1, at = (1:length(levels(state.names)) -0.25), labels=levels(state.names), line = -1.1,cex.axis = 1.5, lwd = 1) 
  axis(side = 1, c(0,184,368,552,736,920), line = -0.5, cex.axis = 1.5, lwd = 2.2)
  lines (x =c(0,0), y = c(0, 50), lwd = 2.2)
  mtext (text = "Time (s)", at = c(500), side = 1, font=1, cex = 1.5, line = 2)

  ###### Adding predstor data ###### 
  mantiddata.again <- mantiddata[mantiddata$file.num == e,]
  pred.crap <- data.frame( pred.state= mantiddata.again$pred.state[ mantiddata.again$pred.state == "close"| mantiddata.again$pred.state == "far" ], sec.timestamp= mantiddata.again$sec.timestamp[ mantiddata.again$pred.state == "close" | mantiddata.again$pred.state == "far"] )
  pred.crap$colour <- sub( "close", "black", pred.crap$pred.state )
  pred.crap$colour <- sub( "far", "gray84", pred.crap$colour )
  
  ytop <- 7.5
  ybottom <- ytop - 0.4
  for (i in 1:nrow(pred.crap))  {
    rect( xleft= pred.crap$sec.timestamp[i], 
          xright= pred.crap$sec.timestamp[i+1],
          ybottom=ybottom, 
          ytop=ytop, 
          col = pred.crap$colour[i], 
          border = pred.crap$colour[i])}

  axis(side=2, las = 1, at = 7.16, labels = "Mantid location", line = -1.1,cex.axis = 1, font =3, lwd = 1)}


## For Figure 1a
pdf()                       
ethogram.plotting(1)
dev.off()

## For S1 Figures
pdf()                       
all.flies <- rep(NA, 30)
for ( i in 1:30 ) {
  all.flies[i] <- ethogram.plotting(levels(mantiddata$file.num)[i])}
dev.off()


setEPS()
postscript("~/Desktop/Figure1b.eps", pointsize = 12)
ethogram.plotting(1)
dev.off()

























