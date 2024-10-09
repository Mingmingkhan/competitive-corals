# Calculate nearest neighbour distances and plot TC parameters


# plot number of offspring ------------------------------------------------

load(file = "results/all.pd.tc.RData")

med.o.orange <- median(groupO.orange.out$tc.off)
med.p.orange <- median(groupP.orange.out$tc.off)
med.m.orange <- median(groupM.orange.out$tc.off)

all.orange <- rbind(groupO.orange.out, 
                    groupP.orange.out,
                    groupM.orange.out)

orange.corals.off <- ggplot(all.orange, 
              aes(x = tc.pd.10, y = tc.off, group = Group)) +
  geom_point(aes(shape=Group, colour = Group, size = 5)) +
  scale_color_manual(values=c("#E68000","#EE5921", "#FEBA4F")) +
  theme(legend.position="bottom") +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  ylim(0, 8) + 
  geom_hline(yintercept = med.o.orange, 
             color="#EE5921", linetype="solid", 
             lwd = 1) +
  annotate("text", x = 0.2, y = med.o.orange + 0.3, 
           label = paste("median O = ", round(med.o.orange, 3))) +
  geom_hline(yintercept = med.p.orange, 
             color="#FEBA4F", linetype="dashed", 
             lwd = 1) +
  annotate("text", x = 0.5, y = med.p.orange + 0.3, 
           label = paste("median P = ", round(med.p.orange, 3))) +
  geom_hline(yintercept = med.m.orange, 
             color="#E68000", linetype="longdash", 
             lwd = 1) +
  annotate("text", x = 0.75, y = med.m.orange + 0.3, 
           label = paste("median M = ", round(med.m.orange, 3))) +
  
  ggtitle("Number of Offspring: Orange Corals") +
  theme_bw()
orange.corals.off
  

med.o.pink <- median(groupO.pink.out$tc.off)
med.p.pink <- median(groupP.pink.out$tc.off)
med.m.pink <- median(groupM.pink.out$tc.off)

all.pink <- rbind(groupO.pink.out, 
                    groupP.pink.out,
                    groupM.pink.out)

pink.corals.off <- ggplot(all.pink, 
          aes(x = tc.pd.10, y = tc.off, group = Group)) +
  geom_point(aes(shape=Group, colour = Group, size = 5)) +
  scale_color_manual(values = c("palevioletred1", "plum", "magenta")) +
  theme(legend.position="bottom") +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  ylim(0, 8) + 
  geom_hline(yintercept = med.p.pink, 
             color="magenta", linetype="solid", lwd = 1) +
  annotate("text", x = 0.5, y = med.p.pink - 0.3, 
           label = paste("median P = ", round(med.p.pink, 3))) +
  geom_hline(yintercept = med.m.pink, 
             color="palevioletred1", linetype="longdash", lwd =1) +
  annotate("text", x = 0.3, y = med.m.pink + 0.3, 
           label = paste("median M = ", round(med.m.pink, 3))) +
  ggtitle("Number of Offspring: Pink Corals") +
  theme_bw()
pink.corals.off


# Stitch together Figure 6a -----------------------------------------------

orange.pink.off <- gridExtra::grid.arrange(orange.corals.off, pink.corals.off,
                                        ncol = 2)


# calculate nndists -----

library(spatstat)


# group O -----------------------------------------------------------------

load(file = "data/GroupO_coral_ppp_marked.RData")

groupO.orange.nndist <- vector(mode = "list", length = 6)
names(groupO.orange.nndist) <- groupO.names

for (i in 1:6){
  if (length(groupO.orange.ppp[[i]]) == 5){
    img <- groupO.names[i]
    ppp1 <- groupO.orange.ppp[[i]]
    nndist.res <- vector("list", length = 3)
    names(nndist.res) <- c("res", "mean_dist", "med_dist")
    nndist.res[[1]] <- nndist(ppp1)
    nndist.res[[2]] <- mean(nndist(ppp1))
    nndist.res[[3]] <- median(nndist(ppp1))
    
    groupO.orange.nndist[[i]] <- nndist.res
  }
}

nndist.mean <- lapply(groupO.orange.nndist, `[[`, 2)
nndist.mean <- do.call(rbind, nndist.mean)
colnames(nndist.mean) <- "mean.dist"


nndist.med <- lapply(groupO.orange.nndist, `[[`, 3)
nndist.med <- do.call(rbind, nndist.med)
colnames(nndist.med) <- "med.dist"

nndist.res <- cbind(nndist.mean, nndist.med)
nndist.res <- as.data.frame(nndist.res)
nndist.res$img <- groupO.names

x <- rep("GroupO", 6)
y <- rep("orange_coral", 6)
z <- cbind(x, y, groupO.names)
colnames(z) <- c("group", "coral_type", "img")

nndist.res <- merge(z, nndist.res, by.x = "img", by.y = "img")
nndist.res <- as.data.frame(nndist.res)
temp <- nndist.res[,4:5]
temp2 <- cbind(x, y, groupO.names, temp)
colnames(temp2) <- c("group", "coral_type", "img", "mean_dist", "med_dist")
groupO.orange.nndist.res <- temp2

#now pink
groupO.pink.nndist <- vector(mode = "list", length = 3)
img <- groupO.names[1]
ppp1 <- ppp.img0483.pink

nndist.res <- vector("list", length = 3)
names(nndist.res) <- c("res", "mean_dist", "med_dist")
nndist.res[[1]] <- nndist(ppp1)
nndist.res[[2]] <- mean(nndist(ppp1))
nndist.res[[3]] <- median(nndist(ppp1))


groupO.pink.nndist <- nndist.res

x <- "GroupO"
y <- "pink_coral"
groupO.pink.nndist.res <- groupO.pink.nndist[c(2,3)]
groupO.pink.nndist.res <- do.call(cbind, groupO.pink.nndist.res)
groupO.pink.nndist.res <- as.data.frame(groupO.pink.nndist.res)

groupO.pink.nndist.res <- cbind(x, y, img, groupO.pink.nndist.res)
colnames(groupO.pink.nndist.res) <- c("group", "coral_type", "img", "mean_dist",
                                      "med_dist")

save(groupO.orange.nndist.res, groupO.pink.nndist.res,
     file = "results/groupO.nndist.RData")


# Group P -----------------------------------------------------------------

load(file = "data/GroupP_coral_ppp_marked.RData")

groupP.orange.nndist <- vector(mode = "list", length = 21)
names(groupP.orange.nndist) <- groupP.names

for (i in 1:21){
  if (length(groupP.orange.ppp[[i]]) == 5){
    img <- groupP.names[i]
    ppp1 <- groupP.orange.ppp[[i]]
    nndist.res <- vector("list", length = 3)
    names(nndist.res) <- c("res", "mean_dist", "med_dist")
    nndist.res[[1]] <- nndist(ppp1)
    nndist.res[[2]] <- mean(nndist(ppp1))
    nndist.res[[3]] <- median(nndist(ppp1))
    groupP.orange.nndist[[i]] <- nndist.res
    names(groupP.orange.nndist[[i]]) <- img
  }
}

nndist.mean <- lapply(groupP.orange.nndist, `[[`, 2)
nndist.mean <- do.call(rbind, nndist.mean)
colnames(nndist.mean) <- "mean.dist"


nndist.med <- lapply(groupP.orange.nndist, `[[`, 3)
nndist.med <- do.call(rbind, nndist.med)
colnames(nndist.med) <- "med.dist"

nndist.res <- cbind(nndist.mean, nndist.med)
nndist.res <- as.data.frame(nndist.res)
nndist.res$img <- rownames(nndist.res)
rownames(nndist.res) <- NULL

x <- rep("GroupP", 8)
y <- rep("orange_coral", 8)
z <- cbind(x, y, nndist.res$img)
colnames(z) <- c("group", "coral_type", "img")

nndist.res <- merge(z, nndist.res, by.x = "img", by.y = "img")
nndist.res <- as.data.frame(nndist.res)
temp <- nndist.res[,4:5]
temp2 <- cbind(z, temp)
colnames(temp2) <- c("group", "coral_type", "img", "mean_dist", "med_dist")
groupP.orange.nndist.res <- temp2

#now pink

groupP.pink.nndist <- vector(mode = "list", length = 21)
names(groupP.pink.nndist) <- groupP.names


for (i in 1:21){
  if (length(groupP.pink.ppp[[i]]) == 5){
    img <- groupP.names[i]
    ppp1 <- groupP.pink.ppp[[i]]
    nndist.res <- vector("list", length = 3)
    names(nndist.res) <- c("res", "mean_dist", "med_dist")
    nndist.res[[1]] <- nndist(ppp1)
    nndist.res[[2]] <- mean(nndist(ppp1))
    nndist.res[[3]] <- median(nndist(ppp1))
    groupP.pink.nndist[[i]] <- nndist.res
    names(groupP.pink.nndist[[i]]) <- img
  }
}

nndist.mean <- lapply(groupP.pink.nndist, `[[`, 2)
nndist.mean <- do.call(rbind, nndist.mean)
colnames(nndist.mean) <- "mean.dist"


nndist.med <- lapply(groupP.pink.nndist, `[[`, 3)
nndist.med <- do.call(rbind, nndist.med)
colnames(nndist.med) <- "med.dist"

nndist.res <- cbind(nndist.mean, nndist.med)
nndist.res <- as.data.frame(nndist.res)
nndist.res$img <- rownames(nndist.res)
rownames(nndist.res) <- NULL

x <- rep("GroupP", 21)
y <- rep("pink_coral", 21)
z <- cbind(x, y, nndist.res$img)
colnames(z) <- c("group", "coral_type", "img")

nndist.res <- merge(z, nndist.res, by.x = "img", by.y = "img")
nndist.res <- as.data.frame(nndist.res)
temp <- nndist.res[,4:5]
temp2 <- cbind(z, temp)
colnames(temp2) <- c("group", "coral_type", "img", "mean_dist", "med_dist")
groupP.pink.nndist.res <- temp2


# Group M -----------------------------------------------------------------

load(file = "data/GroupM_coral_ppp_marked.RData")

groupM.orange.nndist <- vector(mode = "list", length = 9)
names(groupM.orange.nndist) <- groupM.names

for (i in 1:9){
  if (length(groupM.orange.ppp[[i]]) == 5){
    img <- groupM.names[i]
    ppp1 <- groupM.orange.ppp[[i]]
    nndist.res <- vector("list", length = 3)
    names(nndist.res) <- c("res", "mean_dist", "med_dist")
    nndist.res[[1]] <- nndist(ppp1)
    nndist.res[[2]] <- mean(nndist(ppp1))
    nndist.res[[3]] <- median(nndist(ppp1))
    groupM.orange.nndist[[i]] <- nndist.res
    names(groupM.orange.nndist[[i]]) <- img
  }
}

nndist.mean <- lapply(groupM.orange.nndist, `[[`, 2)
nndist.mean <- do.call(rbind, nndist.mean)
colnames(nndist.mean) <- "mean.dist"


nndist.med <- lapply(groupM.orange.nndist, `[[`, 3)
nndist.med <- do.call(rbind, nndist.med)
colnames(nndist.med) <- "med.dist"

nndist.res <- cbind(nndist.mean, nndist.med)
nndist.res <- as.data.frame(nndist.res)
nndist.res$img <- rownames(nndist.res)
rownames(nndist.res) <- NULL

x <- rep("GroupM", 9)
y <- rep("orange_coral", 9)
z <- cbind(x, y, nndist.res$img)
colnames(z) <- c("group", "coral_type", "img")

nndist.res <- merge(z, nndist.res, by.x = "img", by.y = "img")
nndist.res <- as.data.frame(nndist.res)
temp <- nndist.res[,4:5]
temp2 <- cbind(z, temp)
colnames(temp2) <- c("group", "coral_type", "img", "mean_dist", "med_dist")
groupM.orange.nndist.res <- temp2

#now pink

groupM.pink.nndist <- vector(mode = "list", length = 9)
names(groupM.pink.nndist) <- groupM.names

for (i in 1:9){
  if (length(groupM.pink.ppp[[i]]) == 5){
    img <- groupM.names[i]
    ppp1 <- groupM.pink.ppp[[i]]
    nndist.res <- vector("list", length = 3)
    names(nndist.res) <- c("res", "mean_dist", "med_dist")
    nndist.res[[1]] <- nndist(ppp1)
    nndist.res[[2]] <- mean(nndist(ppp1))
    nndist.res[[3]] <- median(nndist(ppp1))
    groupM.pink.nndist[[i]] <- nndist.res
    names(groupM.pink.nndist[[i]]) <- img
  }
}

nndist.mean <- lapply(groupM.pink.nndist, `[[`, 2)
nndist.mean <- do.call(rbind, nndist.mean)
colnames(nndist.mean) <- "mean.dist"


nndist.med <- lapply(groupM.pink.nndist, `[[`, 3)
nndist.med <- do.call(rbind, nndist.med)
colnames(nndist.med) <- "med.dist"

nndist.res <- cbind(nndist.mean, nndist.med)
nndist.res <- as.data.frame(nndist.res)
nndist.res$img <- rownames(nndist.res)
rownames(nndist.res) <- NULL

x <- rep("GroupM", 9)
y <- rep("pink_coral", 9)
z <- cbind(x, y, nndist.res$img)
colnames(z) <- c("group", "coral_type", "img")

nndist.res <- merge(z, nndist.res, by.x = "img", by.y = "img")
nndist.res <- as.data.frame(nndist.res)
temp <- nndist.res[,4:5]
temp2 <- cbind(z, temp)
colnames(temp2) <- c("group", "coral_type", "img", "mean_dist", "med_dist")
groupM.pink.nndist.res <- temp2


# compile all nndist ------------------------------------------------------

all.nndist <- rbind(groupO.orange.nndist.res, groupO.pink.nndist.res,
                    groupP.orange.nndist.res, groupP.pink.nndist.res,
                    groupM.orange.nndist.res, groupM.pink.nndist.res)

save(all.nndist, groupO.orange.nndist.res, groupO.pink.nndist.res,
     groupP.orange.nndist.res, groupP.pink.nndist.res,
     groupM.orange.nndist.res, groupM.pink.nndist.res, 
     file = "results/all.nndist.RData")


# test significant differences --------------------------------------------

#between mean oranges 

#between group O and group P
res <- wilcox.test(groupO.orange.nndist.res$mean_dist, 
                   groupP.orange.nndist.res$mean_dist)

res #shows non significance 

#between group O and group M
res <- wilcox.test(groupO.orange.nndist.res$mean_dist, 
                   groupM.orange.nndist.res$mean_dist)

res #shows non significance 

#between group P and group M
res <- wilcox.test(groupP.orange.nndist.res$mean_dist, 
                   groupM.orange.nndist.res$mean_dist)
res #shows non significance 

#between mean pinks 

res <- wilcox.test(groupP.pink.nndist.res$mean_dist, 
                   groupM.pink.nndist.res$mean_dist)
res #show significant difference between means 

# plot mean nndist -------------------------------------------------------------

all.nndist$group <- as.factor(all.nndist$group)
all.nndist$coral_type <- as.factor(all.nndist$coral_type)

orange.nndist <- rbind(groupO.orange.nndist.res,
                       groupP.orange.nndist.res,
                       groupM.orange.nndist.res)

nndist.orange.violin <- ggplot(data = orange.nndist, 
                            aes(x = group, y = mean_dist, fill = group)) +
  geom_violin(trim = TRUE, position = position_dodge(0.9)) +
  geom_boxplot(width = 0.1, position = position_dodge(0.9)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.6),
             aes(shape = group, size = 0.01)) +
  theme(legend.position = "bottom") + 
  scale_fill_manual(values=c("#E68000","#EE5921", "#FEBA4F")) +
  ylim(0, 15) +
  theme_minimal() +
  ggtitle("Mean NN-Dist")

nndist.orange.violin


pink.nndist <- rbind(groupO.pink.nndist.res,
                     groupP.pink.nndist.res,
                     groupM.pink.nndist.res)

nndist.pink.violin <- ggplot(data = pink.nndist, 
                             aes(x = group, y = mean_dist, fill = group)) +
  geom_violin(trim = TRUE, position = position_dodge(0.9)) +
  geom_boxplot(width = 0.1, position = position_dodge(0.9)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.60),
             aes(shape = group, size = 0.1)) +
  theme(legend.position = "bottom") + 
  scale_fill_manual(values=c("palevioletred1", "plum", "magenta")) +
  ylim(0, 15) +
  theme_minimal()+
  ggtitle("Mean NN-Dist")

nndist.pink.violin

#stitch together for Figure 6b ----------

mean.nndist.combined <- 
  gridExtra::grid.arrange(nndist.orange.violin, nndist.pink.violin,
                          ncol = 2)

