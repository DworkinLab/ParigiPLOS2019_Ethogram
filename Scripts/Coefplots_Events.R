### This script can be used to build coefplots for behavioral events ## 
##  But first you must run all the models for events from the script called Statistical_Models.R

require(coefplot2)

# You might want to run this chunk of code and make sure that the models were run alright
## spider 
# summary(modelE1.MCMCglmm) # ab
# summary(modelE2.MCMCglmm) # fly
# summary(modelE3.MCMCglmm) # pause
# summary(modelE4.MCMCglmm) # wd
# summary(modelE5.MCMCglmm) # turn
# summary(modelE6.MCMCglmm) # jump

# ## mantid
# summary(modelE7.MCMCglmm) # ab
# summary(modelE8.MCMCglmm) # fly
# summary(modelE9.MCMCglmm) # pause
# summary(modelE10.MCMCglmm) # wd
# summary(modelE11.MCMCglmm) # turn
# summary(modelE12.MCMCglmm) # jump


## The plots start here ##

### Spider
par(mfrow = c(2,3), cex.axis = 1.6)
names <- c("Intercept", "Pred state","Time(cent.)")

# ab
coefplot2(modelE3.lme4, upper1 = "n", lwd.1 = 0, lwd.2 = 4, vertical = T, 
          v.axis = F, h.axis = T, top.axis = F, cex.pts = 1.25, cex.var = 1, cex.axis = 10,
          main = NA, col.pts = "black", xlim = c(-5, 5), intercept = T)
axis(side=2, las = 1, at = (1:length(names)), labels=names, line = -7.5, cex.lab = 3, pos = -5) 
rect(xleft = -5, ybottom =0.43, xright = 5, ytop = 2.565, lwd = 3.5)
mtext("Ab", side = 3, line = 0.15, cex = 1.3, adj = NA)


# fly
coefplot2(modelE3.fly.lme4, upper1 = "n", lwd.1 = 0, lwd.2 = 4, vertical = T, 
          v.axis = F, h.axis = T, top.axis = F, cex.pts = 1.25, cex.var = 1, cex.axis = 10,
          main = NA, col.pts = "black", xlim = c(-5, 5), intercept = T)

axis(side=2, las = 1, at = (1:length(names)), labels=names, line = -7.5, cex.lab = 3, pos = -5) 
rect(xleft = -5, ybottom =0.43, xright = 5, ytop = 2.565, lwd = 3.5)
mtext("Fly", side = 3, line = 0.15, cex = 1.3, adj = NA)

# pause
coefplot2(modelE3.pause.lme4, upper1 = "n", lwd.1 = 0, lwd.2 = 4, vertical = T, 
          v.axis = F, h.axis = T, top.axis = F, cex.pts = 1.25, cex.var = 1, cex.axis = 10,
          main = NA, col.pts = "black", xlim = c(-5, 5), intercept = T)

axis(side=2, las = 1, at = (1:length(names)), labels=names, line = -7.5, cex.lab = 3, pos = -5) 
rect(xleft = -5, ybottom =0.43, xright = 5, ytop = 2.565, lwd = 3.5)
mtext("Pause", side = 3, line = 0.15, cex = 1.3, adj = NA)


# wd
coefplot2(modelE3.wd.lme4, upper1 = "n", lwd.1 = 0, lwd.2 = 4, vertical = T, 
          v.axis = F, h.axis = T, top.axis = F, cex.pts = 1.25, cex.var = 1, cex.axis = 10,
          main = NA, col.pts = "black", xlim = c(-5,5), intercept = T)
axis(side=2, las = 1, at = (1:length(names)), labels=names, line = -7.5, cex.lab = 3, pos = -5) 
rect(xleft = -5, ybottom =0.43, xright = 5, ytop = 2.565, lwd = 3.5)
mtext("Wd", side = 3, line = 0.15, cex = 1.3, adj = NA)


# turn
coefplot2(modelE4.turn.lme4, upper1 = "n", lwd.1 = 0, lwd.2 = 4, vertical = T, 
          v.axis = F, h.axis = T, top.axis = F, cex.pts = 1.25, cex.var = 1, cex.axis = 10,
          main = NA, col.pts = "black", xlim = c(-5, 5), intercept = T)
axis(side=2, las = 1, at = (1:length(names)), labels=names, line = -7.5, cex.lab = 3, pos = -5) 
rect(xleft = -5, ybottom =0.57, xright = 5, ytop = 3.4, lwd = 3.54)
mtext("Turn", side = 3, line = 0.15, cex = 1.3, adj = NA)

# jump
coefplot2(modelE3.jump.lme4, upper1 = "n", lwd.1 = 0, lwd.2 = 4, vertical = T, 
          v.axis = F, h.axis = T, top.axis = F, cex.pts = 1.25, cex.var = 1, cex.axis = 10,
          main = NA, col.pts = "black", xlim = c(-5, 5), intercept = T)
axis(side=2, las = 1, at = (1:length(names)), labels=names, line = -7.5, cex.lab = 0.1, pos = -5) 
rect(xleft = -5, ybottom =0.43, xright = 5, ytop = 2.565, lwd = 3.5)
mtext("Jump", side = 3, line = 0.15, cex = 1.3, adj = NA)




## Mantid

par(mfrow = c(2,3), cex.axis = 1.57)

# ab
coefplot2(modelE4.mantid.ab.lme4, upper1 = "n", lwd.1 = 0, lwd.2 = 4, vertical = T, 
          v.axis = F, h.axis = T, top.axis = F, cex.pts = 1.25, cex.var = 1, cex.axis = 10,
          main = NA, col.pts = "black", xlim = c(-5, 5), intercept = T)

axis(side=2, las = 1, at = (1:length(names)), labels=names, line = -7.5, cex.lab = 3, pos = -5) 
rect(xleft = -5, ybottom =0.57, xright = 5, ytop = 3.4, lwd = 3.5)
mtext("Ab", side = 3, line = 0.15, cex = 1.3, adj = NA)



# fly
coefplot2(modelE3.mantid.fly.lme4, upper1 = "n", lwd.1 = 0, lwd.2 = 4, vertical = T, 
          v.axis = F, h.axis = T, top.axis = F, cex.pts = 1.25, cex.var = 1, cex.axis = 10,
          main = NA, col.pts = "black", xlim = c(-5, 5), intercept = T)


axis(side=2, las = 1, at = (1:length(names)), labels=names, line = -7.5, cex.lab = 3, pos = -5) 
rect(xleft = -5, ybottom =0.43, xright = 5, ytop = 2.57, lwd = 3.5)
mtext("Fly", side = 3, line = 0.15, cex = 1.3, adj = NA)

# pause
coefplot2(modelE4.mantid.pause.lme4, upper1 = "n", lwd.1 = 0, lwd.2 = 4, vertical = T, 
          v.axis = F, h.axis = T, top.axis = F, cex.pts = 1.25, cex.var = 1, cex.axis = 10,
          main = NA, col.pts = "black", xlim = c(-5, 5), intercept = T)

axis(side=2, las = 1, at = (1:length(names)), labels=names, line = -7.5, cex.lab = 3, pos = -5) 
rect(xleft = -5, ybottom =0.57, xright = 5, ytop = 3.41, lwd = 3.5)
mtext("Pause", side = 3, line = 0.15, cex = 1.3, adj = NA)


# wd
coefplot2(modelE3.mantid.wd.lme4, upper1 = "n", lwd.1 = 0, lwd.2 = 4, vertical = T, 
          v.axis = F, h.axis = T, top.axis = F, cex.pts = 1.25, cex.var = 1, cex.axis = 10,
          main = NA, col.pts = "black", xlim = c(-5, 5), intercept = T)
axis(side=2, las = 1, at = (1:length(names)), labels=names, line = -7.5, cex.lab = 3, pos = -5) 
rect(xleft = -5, ybottom =0.431, xright = 5, ytop = 2.57, lwd = 3.5)
mtext("Wd", side = 3, line = 0.15, cex = 1.3, adj = NA)

# turn
coefplot2(modelE3.mantid.turn.lme4, upper1 = "n", lwd.1 = 0, lwd.2 = 4, vertical = T, 
          v.axis = F, h.axis = T, top.axis = F, cex.pts = 1.25, cex.var = 1, cex.axis = 10,
          main = NA, col.pts = "black", xlim = c(-5, 5), intercept = T)
axis(side=2, las = 1, at = (1:length(names)), labels=names, line = -7.5, cex.lab = 3, pos = -5) 
rect(xleft = -5, ybottom =0.431, xright = 5, ytop = 2.57, lwd = 3.5)
mtext("Turn", side = 3, line = 0.15, cex = 1.3, adj = NA)

# jump
coefplot2(modelE3.mantid.jump.lme4, upper1 = "n", lwd.1 = 0, lwd.2 = 4, vertical = T, 
          v.axis = F, h.axis = T, top.axis = F, cex.pts = 1.25, cex.var = 1, cex.axis = 10,
          main = NA, col.pts = "black", xlim = c(-5, 5), intercept = T)
axis(side=2, las = 1, at = (1:length(names)), labels=names, line = -7.5, cex.lab = 3, pos = -5) 
mtext("Jump", side = 3, line = 0.15, cex = 1.3, adj = NA)
rect(xleft = -5, ybottom =0.431, xright = 5, ytop = 2.57, lwd = 3.5)



