### Script that examines the effect of season in which recordings were performed. Figure in S2 File
### Find the CSV called Prop_MonthEffect.csv and Freq_monthEffect.csv

### You will need these..
require(sciplot)

ConfInt <- function(y){
  mean_x <- mean(y)
  std_dev_x <- sd(y)
  std_err_x <- sd(y) / sqrt(length(y))
  lower_CI_x <- mean_x + std_err_x * (qt(p = 0.05 / 2, df = length(y) - 1))
  upper_CI_x <- mean_x + std_err_x * (qt(p = (1 - 0.05 / 2), df = length(y) - 1))
  return(c(lower_CI_x, upper_CI_x))}

# For % time spent in behavioral states in the absence of predator
montheffect <- read.csv("/Users/abhijnaparigi/Dropbox/Abhijna/Ethogram/CSVs/Prop_MonthEffect.csv", header = T)

## Converting to %
montheffect$prop <- montheffect$prop*100
## Preprocessing
montheffect <- subset(montheffect, montheffect$behav != "occl")
montheffect$behav <- as.factor(as.character(montheffect$behav))

## For frequence with which behavioral events were performed in the absence of predator
montheffect2 <- read.csv("/Users/abhijnaparigi/Dropbox/Abhijna/Ethogram/CSVs/Freq_monthEffect.csv", header = T)



### Plots ###

par(mfrow = c(1,2), cex.axis = 8)
par(mfrow = c(1,2), mar = c(5, 4.5, 2, 2) + 0.1, omi = c(bottom = 0, left=0.6, top=0, right=0))

lineplot.CI(montheffect$behav, response = montheffect$prop, 
            group = montheffect$pred, xlab = "States", ylab = "% time", 
            ci.fun = function(y) ConfInt(y), legend = F, 
            cex.axis = 1.7, cex.leg = 1.2, cex.lab = 1.7, type = "p", cex = 1.9, lwd = 2, err.width = 0.055)
box(lwd = 2)

lineplot.CI(montheffect2$behav, response = montheffect2$freq, group = montheffect2$pred, 
            xlab = "Events", ylab = "# occurrences / min", ci.fun = function(y) ConfInt(y), 
            legend = F, x.leg = 2, y.leg = 2.25, cex.axis = 1.7, , cex.leg = 1.2, cex.lab = 1.7, 
            type = "p", cex = 1.9, lwd = 2, err.width = 0.055)
box(lwd = 2)

legend(x = 1.5, y = 2.5, pch = 1, legend = "Before spider addition", col = "grey3", bty = "n", cex = 1.7,  x.intersp = 0.23)
legend(x = 1.5, y = 2.3, pch = 16, legend = "Before mantid addition", col = "grey3", bty = "n", cex = 1.7,  x.intersp = 0.23)


