# -------Plot a few PCFs for the paper (Figure 4)

# import data -------------------------------------------------------------


# from group O orange - good for segregations  

load(file = "results/groupO_4models_res.RData") #the results 
load(file = "data/GroupO_coral_ppp_marked.RData") #the ppp

img0929_or <- groupO.orange.res$img0929
img0929_or_ppp <- groupO.orange.ppp$img0929

#remove other stuff and clear memory
rm(ppp.img0483.pink)
rm(groupO.pink.res)
rm(groupO.orange.res)
rm(groupO.orange.ppp)
gc()

# from group P - good for both aggregations and segregations 

load(file = "results/groupP_pink_res.Rdata")

img0457_pink_res <- groupP.pink.res$img0457 #the results
rm(groupP.pink.res)
gc()

load(file = "results/groupP_orange_res.RData")
img0457_or_res <- groupP.orange.res$img0457
rm(groupP.orange.res)
gc()

load(file = "data/GroupP_coral_ppp_marked.RData")
img0457_pink_ppp <- groupP.pink.ppp$img0457
img0457_or_ppp <- groupP.orange.ppp$img0457
rm(groupP.pink.ppp)
rm(groupP.orange.ppp)
gc()

# from group M - 1025 - good for agg/segg at diff scales 

load(file = "results/groupM_pink_res.RData")
img1025_pink_res <- groupM.pink.res[[9]]
rm(groupM.pink.res)
gc()

load(file = "results/groupM_orange_res.RData")
img1025_or_res <- groupM.orange.res$img1025
rm(groupM.orange.res)
gc()

load(file = "data/GroupM_coral_ppp_marked.RData")
img1025_pink_ppp <- groupM.pink.ppp$img1025
img1025_or_ppp <- groupM.orange.ppp$img1025
rm(groupM.orange.ppp)
rm(groupM.pink.ppp)
gc()



# now plot ----------------------------------------------------------------

#orange sim envelope color 

o.sim.col <- yarrr::transparent("orange", trans.val = .7)
p.sim.col <- yarrr::transparent("hotpink1", trans.val = .7)


#first the img0929

csr.x <- img0929_or$envpcf$csr$r
csr.lo <- img0929_or$envpcf$csr$lo
csr.hi <- img0929_or$envpcf$csr$hi
csr.obs <- img0929_or$envpcf$csr$obs
csr.non.sig.ind <- which(csr.obs < csr.hi &  csr.obs > csr.lo)

#svg(file = "figures_for_paper/three-curves-new.svg", pointsize = 15,
#    h = 18, w = 14)
windows(h = 18, w = 14)

par(mfrow = c(3,2))
plot(csr.x, csr.lo,
     xlim = c(0, max(csr.x)), ylim = c(-1, 4),
     type = "l", col = "white",
     xaxs="i",yaxs="i",
     xlab = "r", ylab = "g(r)",
     main = "img0929 CSR")
grid(col = "grey90")
polygon(c(csr.x, rev(csr.x)), c(csr.hi, rev(csr.lo)), col = o.sim.col, lty = 0)
lines(csr.x, csr.obs, col = "grey30", lwd = 1.2)
points(csr.x[-csr.non.sig.ind], csr.obs[-csr.non.sig.ind], col = "darkorange",
       pch = 16)
abline(h = 1, lty = 2)
legend("topright", legend = c("orange morph"), lwd = 1.2, col = "darkorange")

#plot the point pattern 
#cols <- beachcolours(range = c(0, 50), monochrome = TRUE)

plot(1:175, 1:175, xlim = c(0,178), ylim = c(0,178), col = "white",
     xlab = "", ylab = "", asp = 1,
     main = paste("Coral Point Pattern img0929 |", "Orange n = ", 
                  img0457_or_ppp$n))
points(img0929_or_ppp$x, img0929_or_ppp$y, cex = 1.5, 
       pch = 21, bg = "darkorange", col = "black")
#dev.off()


# img 0457 ----------------------------------------------------------------

p.x <- img0457_pink_res$envpcf$csr$r
p.lo <- img0457_pink_res$envpcf$csr$lo
p.hi <- img0457_pink_res$envpcf$csr$hi
p.obs <- img0457_pink_res$envpcf$csr$obs
p.non.sig <- which(p.obs < p.hi &  p.obs > p.lo)

o.x <- img0457_or_res$envpcf$csr$r
o.lo <- img0457_or_res$envpcf$csr$lo
o.hi <- img0457_or_res$envpcf$csr$hi
o.obs <- img0457_or_res$envpcf$csr$obs
o.non.sig <- which(o.obs < o.hi &  o.obs > o.lo)


#par(mfrow = c(1,2))
plot(p.x, p.lo,
     xlim = c(0, max(p.x)), ylim = c(-1, 4),
     type = "l", col = "white",
     xaxs="i",yaxs="i",
     xlab = "r", ylab = "g(r)",
     main = "img0457 CSR")
grid(col = "grey90")

polygon(c(o.x, rev(o.x)), c(o.hi, rev(o.lo)), 
        col = o.sim.col, lty = 0)

polygon(c(p.x, rev(p.x)), c(p.hi, rev(p.lo)), 
        col = p.sim.col, lty = 0)

abline(h = 1, lty = 2)

lines(p.x, p.obs, col = "grey30", lwd = 1.2)
points(p.x[-p.non.sig], p.obs[-p.non.sig], col = "hotpink",
       pch = 16)
lines(o.x, o.obs, col = "grey30", lwd = 1.2)
points(o.x[-o.non.sig], o.obs[-o.non.sig], col = "darkorange",
       pch = 16)
legend("topright", legend = c("orange morph", 
                              "pink morph"), lwd = 1.2, col = c("darkorange",
                                                                "hotpink"))

#plot the point pattern 

plot(1:175, 1:175, xlim = c(0,178), ylim = c(0,178), col = "white",
     xlab = "", ylab = "", asp = 1, 
     main = paste("Coral Point Pattern img0457 |", "Orange n = ", 
                  img0457_or_ppp$n, "| Pink n = ", img0457_pink_ppp$n))
points(img0457_or_ppp$x, img0457_or_ppp$y, cex = 1.5, 
       pch = 21, bg = "darkorange", col = "black")
points(img0457_pink_ppp$x, img0457_pink_ppp$y, cex = 1.5, 
       pch = 21, bg = "hotpink", col = "black")



# img 1025 ----------------------------------------------------------------

p.x <- img1025_pink_res$envpcf$csr$r
p.lo <- img1025_pink_res$envpcf$csr$lo
p.hi <- img1025_pink_res$envpcf$csr$hi
p.obs <- img1025_pink_res$envpcf$csr$obs
p.non.sig <- which(p.obs < p.hi &  p.obs > p.lo)

o.x <- img1025_or_res$envpcf$csr$r
o.lo <- img1025_or_res$envpcf$csr$lo
o.hi <- img1025_or_res$envpcf$csr$hi
o.obs <- img1025_or_res$envpcf$csr$obs
o.non.sig <- which(o.obs < o.hi &  o.obs > o.lo)


#par(mfrow = c(1,2))
plot(p.x, p.lo,
     xlim = c(0, max(p.x)), ylim = c(-1, 4),
     type = "l", col = "white",
     xaxs="i",yaxs="i",
     xlab = "r", ylab = "g(r)",
     main = "img1025 CSR")
grid(col = "grey90")

polygon(c(o.x, rev(o.x)), c(o.hi, rev(o.lo)), 
        col = o.sim.col, lty = 0)

polygon(c(p.x, rev(p.x)), c(p.hi, rev(p.lo)), 
        col = p.sim.col, lty = 0)

abline(h = 1, lty = 2)

lines(p.x, p.obs, col = "grey30", lwd = 1.2)
points(p.x[-p.non.sig], p.obs[-p.non.sig], col = "hotpink",
       pch = 16)
lines(o.x, o.obs, col = "grey30", lwd = 1.2)
points(o.x[-o.non.sig], o.obs[-o.non.sig], col = "darkorange",
       pch = 16)
legend("topright", legend = c("orange morph", 
                              "pink morph"), lwd = 1.2, col = c("darkorange",
                                                                "hotpink"))

#plot the point pattern 

plot(1:175, 1:175, xlim = c(0,178), ylim = c(0,178), col = "white",
     xlab = "", ylab = "", asp = 1,
     main = paste("Coral Point Pattern img1025 |", "Orange n = ", 
                  img1025_or_ppp$n, "| Pink n = ", img1025_pink_ppp$n))
points(img1025_or_ppp$x, img1025_or_ppp$y, cex = 1.5, 
       pch = 21, bg = "darkorange", col = "black")
points(img1025_pink_ppp$x, img1025_pink_ppp$y, cex = 1.5, 
       pch = 21, bg = "hotpink", col = "black")
dev.off()



