### This script can be used to build coefplots for behavioral states ## 
##  But first you must run all the models for states from the script called Statistical_Models.R


## Better check if the models were run correctly before proceeding
# summary(model1.MCMCglmm)
# summary(model2.MCMCglmm)
# summary(model3.MCMCglmm)
# summary(model4.MCMCglmm)


## Then we plot ..

par(mfrow = c(1,2), oma = c(2,1,3,1) , mar = c(4,0,1,0), cex.axis = 2, plt=c(0.1,0.9,0,0.7))

## Spider grooming

names <- c("Intercept", "Time (cent.)", "Pred state","Age",
           "Temp","Start time","Sex")
coefplot2(model1.MCMCglmm, upper1 = "n", lwd.1 = 0, lwd.2 = 4, vertical = T, 
          v.axis = F, h.axis = T, top.axis = F, cex.pts = 1.25, cex.var = 1, cex.axis = 10,
          main = NA, col.pts = "black", xlim = c(-300, 550), las = 1, intercept = T)


## Spider walking
coefplot2(model2.MCMCglmm, upper1 = "n", lwd.1 = 0, lwd.2 = 4, vertical = T, 
          v.axis = F, h.axis = T, top.axis = F, cex.pts = 1.25, cex.var = 1, cex.axis = 10,
          main = NA, col.pts = "red", xlim = c(-300, 550), intercept = T, add = T, offset =  0.2, col = "red")

axis(side=2, las = 1, at = (1:length(names)), labels=names, line = -7.5, cex.lab = 3, lwd = 3, pos = -300) 
rect(xleft = -300, ybottom =0.63, xright = 550, ytop = 7.37, lwd = 3.5)
mtext("Spider", side = 3, line = 0.15, cex = 2, adj = NA)


## Mantid grooming
coefplot2(model3.MCMCglmm, upper1 = "n", lwd.1 = 0, lwd.2 = 4, vertical = T, 
          v.axis = F, h.axis = T, top.axis = F, cex.pts = 1.25, cex.var = 1, cex.axis = 10,
          main = NA, col.pts = "black", xlim = c(-300, 550), intercept = T)

axis(side=2, las = 1, at = (1:length(names)), labels=names, line = -7.5, cex.lab = 3, lwd = 3, pos = -300) 
rect(xleft = -300, ybottom =0.63, xright = 550, ytop = 7.37, lwd = 3.5)
mtext("Mantid", side = 3, line = 0.15, cex = 2, adj = NA)
par(srt=270)


## Mantid walking
coefplot2(model4.MCMCglmm, upper1 = "n", lwd.1 = 0, lwd.2 = 4, vertical = T, 
          v.axis = F, h.axis = T, top.axis = F, cex.pts = 1.25, cex.var = 1, cex.axis = 10,
          main = NA, col.pts = "red", xlim = c(-300, 450), intercept = T, add = T, offset =  0.2, col = "red")

# axis(side=2, las = 1, at = (1:length(names)), labels=names, line = -7.5, cex.lab = 3, lwd = 3, pos = -300) 
# rect(xleft = -300, ybottom =0.63, xright = 550, ytop = 7.37, lwd = 3.5)
# #text(322, 3.5, labels = "Locomotion", cex = 2)




## Spider stopping ### Picture dimentions 20/7 ###

par(mfrow = c(1,2), oma = c(3,3,4,7) , mar = c(4,0,1,0), cex.axis = 2, plt=c(0.1,0.9,0,0.7))

coefplot2(model5.MCMCglmm, upper1 = "n", lwd.1 = 0, lwd.2 = 4, vertical = T, 
          v.axis = F, h.axis = T, top.axis = F, cex.pts = 1.25, cex.var = 1, cex.axis = 10,
          main = NA, col.pts = "black", xlim = c(-50, 50), intercept = T)
axis(side=2, las = 1, at = (1:length(names)), labels=names, line = -7.5, cex.lab = 3, lwd = 3, pos = -42) 
rect(xleft = -42, ybottom =0.63, xright = 43, ytop = 7.37, lwd = 3.5)
mtext("Spider", side = 3, line = 0.15, cex = 2, adj = NA)
par(srt=270)


## Mantid stopping

coefplot2(model6.MCMCglmm, upper1 = "n", lwd.1 = 0, lwd.2 = 4, vertical = T, 
          v.axis = F, h.axis = T, top.axis = F, cex.pts = 1.25, cex.var = 1, cex.axis = 10,
          main = NA, col.pts = "black", xlim = c(-50, 50), intercept = T)

axis(side=2, las = 1, at = (1:length(names)), labels=names, line = -7.5, cex.lab = 3, lwd = 3, pos = -42) 
rect(xleft = -42, ybottom =0.63, xright = 43, ytop = 7.37, lwd = 3.5)
mtext("Mantid", side = 3, line = 0.15, cex = 2, adj = NA)
par(srt=270)


