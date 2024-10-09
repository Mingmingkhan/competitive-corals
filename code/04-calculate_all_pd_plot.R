# Extract all the Goodness of Fit and Thomas Cluster parameters,
# plot the PD values (Figure 5)


# Group O -----------------------------------------------------------------

load(file = "results_lfs/groupO_4models_res.RData")

groupO.orange.out <- vector(mode = "list", length = 6)
nm <- names(groupO.orange.res)
names(groupO.orange.out) <- nm

for (i in 2:6){
  #extract the parameters 
  img <- nm[i]
  csr.pd <- groupO.orange.res[[i]]$gof$csr$p
  hp.pd <- groupO.orange.res[[i]]$gof$hp$p
  tc.pd.10 <- groupO.orange.res[[i]]$gof$tc10$p
  tc.pd.20 <- groupO.orange.res[[i]]$gof$tc20$p
  htc.pd <- groupO.orange.res[[i]]$gof$htc$p
  
  all.pd <- as.data.frame(cbind(hp.pd, tc.pd.10, tc.pd.20, htc.pd))
  best.pd <- max(all.pd)
  best.ind <- which(all.pd == best.pd)
  all.pd$delta_hp <- hp.pd - best.pd
  all.pd$delta_tc10 <- tc.pd.10 - best.pd
  all.pd$delta_tc20 <- tc.pd.20 - best.pd
  all.pd$delta_htc <- htc.pd - best.pd
  tc.sum <- summary(groupO.orange.res[[i]]$fit$tc)
  tc.off <- tc.sum$mu1 #number of offspring
  tc.prob <- tc.sum$panysib #prob in cluster
  res <- cbind(all.pd, tc.off, tc.prob)
  groupO.orange.out[[i]] <- res
}

#for img0483 red-----

img <- nm[1]
csr.pd <- groupO.orange.res[[1]]$gof$csr$p
tc.pd.10 <- groupO.orange.res[[1]]$gof$tc10$p
tc.pd.20 <- groupO.orange.res[[1]]$gof$tc20$p
#Delta PD 

all.pd <- as.data.frame(cbind(NA, tc.pd.10, tc.pd.20, NA))
colnames(all.pd) <- c("hp.pd", "tc.pd.10", "tc.pd.20", "htc.pd")
best.pd <- max(all.pd, na.rm = TRUE)
best.ind <- which(all.pd == best.pd)
all.pd$delta_hp <- NA
#all.pd$delta_csr <- csr.pd - best.pd
all.pd$delta_tc10 <- tc.pd.10 - best.pd
all.pd$delta_tc20 <- tc.pd.20 - best.pd
all.pd$delta_htc <- NA
tc.sum <- summary(groupO.orange.res[[1]]$fit$tc)
tc.off <- tc.sum$mu1 #number of offspring
tc.prob <- tc.sum$panysib #prob in cluster 
res <- cbind(all.pd, tc.off, tc.prob)

groupO.orange.out[[1]] <- res

# compile group O orange output results --------------------------------------------------

groupO.orange.out <- do.call(rbind, groupO.orange.out)

#now for pink ---- 
img <- nm[1]
csr.pd <- groupO.pink.res$gof$csr$p
tc.pd.10 <- groupO.pink.res$gof$tc10$p
tc.pd.20 <- groupO.pink.res$gof$tc20$p

all.pd <- as.data.frame(cbind(NA, tc.pd.10, tc.pd.20, NA))
colnames(all.pd) <- c("hp.pd", "tc.pd.10", "tc.pd.20", "htc.pd")
best.pd <- max(all.pd, na.rm = TRUE)
best.ind <- which(all.pd == best.pd)
all.pd$delta_hp <- NA
all.pd$delta_tc10 <- tc.pd.10 - best.pd
all.pd$delta_tc20 <- tc.pd.20 - best.pd
all.pd$delta_htc <- NA
tc.sum <- summary(groupO.pink.res$fit$tc)
tc.off <- tc.sum$mu1 #number of offspring
tc.prob <- tc.sum$panysib #prob in cluster 
res <- cbind(all.pd, tc.off, tc.prob)

groupO.pink.out <- res

# Group P -----------------------------------------------------------------

# orange ----
load(file = "results_lfs/groupP_orange_res.RData")

groupP.orange.out <- vector(mode = "list", length = 21)
nm <- names(groupP.orange.res)
names(groupP.orange.out) <- nm

for (i in 1:21){
  if (length(groupP.orange.res[[i]]) == 3){
  #extract the parameters 
  img <- nm[i]
  csr.pd <- groupP.orange.res[[i]]$gof$csr$p
  hp.pd <- groupP.orange.res[[i]]$gof$hp$p
  tc.pd.10 <- groupP.orange.res[[i]]$gof$tc10$p
  tc.pd.20 <- groupP.orange.res[[i]]$gof$tc20$p
  htc.pd <- groupP.orange.res[[i]]$gof$htc$p
  
  all.pd <- as.data.frame(cbind(hp.pd, tc.pd.10, tc.pd.20, htc.pd))
  best.pd <- max(all.pd)
  best.ind <- which(all.pd == best.pd)
  all.pd$delta_hp <- hp.pd - best.pd
  all.pd$delta_tc10 <- tc.pd.10 - best.pd
  all.pd$delta_tc20 <- tc.pd.20 - best.pd
  all.pd$delta_htc <- htc.pd - best.pd
  tc.sum <- summary(groupP.orange.res[[i]]$fit$tc)
  tc.off <- tc.sum$mu1 #number of offspring
  tc.prob <- tc.sum$panysib #prob in cluster
  res <- cbind(all.pd, tc.off, tc.prob)
  groupP.orange.out[[i]] <- res
  }
}

groupP.orange.out <- do.call(rbind, groupP.orange.out)

rm(groupP.orange.res)
gc()

#pink ----
load(file = "results_lfs/groupP_pink_res.RData")

groupP.pink.out <- vector(mode = "list", length = 21)
nm <- names(groupP.pink.res)
names(groupP.pink.out) <- nm

for (i in 1:21){
  if (length(groupP.pink.res[[i]]) == 3){
    #extract the parameters 
    img <- nm[i]
    csr.pd <- groupP.pink.res[[i]]$gof$csr$p
    hp.pd <- groupP.pink.res[[i]]$gof$hp$p
    tc.pd.10 <- groupP.pink.res[[i]]$gof$tc10$p
    tc.pd.20 <- groupP.pink.res[[i]]$gof$tc20$p
    htc.pd <- groupP.pink.res[[i]]$gof$htc$p
    
    all.pd <- as.data.frame(cbind(hp.pd, tc.pd.10, tc.pd.20, htc.pd))
    best.pd <- max(all.pd)
    best.ind <- which(all.pd == best.pd)
    all.pd$delta_hp <- hp.pd - best.pd
    all.pd$delta_tc10 <- tc.pd.10 - best.pd
    all.pd$delta_tc20 <- tc.pd.20 - best.pd
    all.pd$delta_htc <- htc.pd - best.pd
    tc.sum <- summary(groupP.pink.res[[i]]$fit$tc)
    tc.off <- tc.sum$mu1 #number of offspring
    tc.prob <- tc.sum$panysib #prob in cluster
    res <- cbind(all.pd, tc.off, tc.prob)
    groupP.pink.out[[i]] <- res
  }
}

groupP.pink.out <- do.call(rbind, groupP.pink.out)
rm(groupP.pink.res)
gc()


# Group M -----------------------------------------------------------------

# orange ----
load(file = "results_lfs/groupM_orange_res.RData")

groupM.orange.out <- vector(mode = "list", length = 21)
nm <- names(groupM.orange.res)
names(groupM.orange.out) <- nm

for (i in 1:9){
  if (length(groupM.orange.res[[i]]) == 3){
    #extract the parameters 
    img <- nm[i]
    csr.pd <- groupM.orange.res[[i]]$gof$csr$p
    hp.pd <- groupM.orange.res[[i]]$gof$hp$p
    tc.pd.10 <- groupM.orange.res[[i]]$gof$tc10$p
    tc.pd.20 <- groupM.orange.res[[i]]$gof$tc20$p
    htc.pd <- groupM.orange.res[[i]]$gof$htc$p
    
    all.pd <- as.data.frame(cbind(hp.pd, tc.pd.10, tc.pd.20, htc.pd))
    best.pd <- max(all.pd)
    best.ind <- which(all.pd == best.pd)
    all.pd$delta_hp <- hp.pd - best.pd
    all.pd$delta_tc10 <- tc.pd.10 - best.pd
    all.pd$delta_tc20 <- tc.pd.20 - best.pd
    all.pd$delta_htc <- htc.pd - best.pd
    tc.sum <- summary(groupM.orange.res[[i]]$fit$tc)
    tc.off <- tc.sum$mu1 #number of offspring
    tc.prob <- tc.sum$panysib #prob in cluster
    res <- cbind(all.pd, tc.off, tc.prob)
    groupM.orange.out[[i]] <- res
  }
}

groupM.orange.out <- do.call(rbind, groupM.orange.out)

rm(groupM.orange.res)
gc()

#pink ----
load(file = "results_lfs/groupM_pink_res.RData")

groupM.pink.out <- vector(mode = "list", length = 9)
nm <- names(groupM.pink.res)
names(groupM.pink.out) <- nm

for (i in 1:9){
  if (length(groupM.pink.res[[i]]) == 3){
    #extract the parameters 
    img <- nm[i]
    csr.pd <- groupM.pink.res[[i]]$gof$csr$p
    hp.pd <- groupM.pink.res[[i]]$gof$hp$p
    tc.pd.10 <- groupM.pink.res[[i]]$gof$tc10$p
    tc.pd.20 <- groupM.pink.res[[i]]$gof$tc20$p
    htc.pd <- groupM.pink.res[[i]]$gof$htc$p
    
    all.pd <- as.data.frame(cbind(hp.pd, tc.pd.10, tc.pd.20, htc.pd))
    best.pd <- max(all.pd)
    best.ind <- which(all.pd == best.pd)
    all.pd$delta_hp <- hp.pd - best.pd
    all.pd$delta_tc10 <- tc.pd.10 - best.pd
    all.pd$delta_tc20 <- tc.pd.20 - best.pd
    all.pd$delta_htc <- htc.pd - best.pd
    tc.sum <- summary(groupM.pink.res[[i]]$fit$tc)
    tc.off <- tc.sum$mu1 #number of offspring
    tc.prob <- tc.sum$panysib #prob in cluster
    res <- cbind(all.pd, tc.off, tc.prob)
    groupM.pink.out[[i]] <- res
  }
}

groupM.pink.out <- do.call(rbind, groupM.pink.out)
rm(groupM.pink.res)
gc()

# save all delta pd  ------------------------------------------------------

groupO.names <- rownames(groupO.orange.out)
groupO.orange.pd.1 <- cbind("GroupO", "orange_coral", groupO.names)
colnames(groupO.orange.pd.1) <- c("Group", "coral_type", "img")
groupO.orange.out <- cbind(groupO.orange.pd.1, groupO.orange.out)

groupO.pink.pd.1 <- cbind("GroupO", "pink_coral", groupO.names[1])
colnames(groupO.pink.pd.1) <- c("Group", "coral_type", "img")
groupO.pink.out <- cbind(groupO.pink.pd.1, groupO.pink.out)

groupP.orange.pd.1 <- cbind("GroupP", "orange_coral", rownames(groupP.orange.out))
groupP.orange.pd.1 <- as.data.frame(groupP.orange.pd.1)
colnames(groupP.orange.pd.1) <- c("Group", "coral_type", "img")
groupP.orange.out <- cbind(groupP.orange.pd.1, groupP.orange.out)

groupP.pink.pd.1 <- cbind("GroupP", "pink_coral", rownames(groupP.pink.out))
groupP.pink.pd.1 <- as.data.frame(groupP.pink.pd.1)
colnames(groupP.pink.pd.1) <- c("Group", "coral_type", "img")
groupP.pink.out <- cbind(groupP.pink.pd.1, groupP.pink.out)

groupM.orange.pd.1 <- cbind("GroupM", "orange_coral", rownames(groupM.orange.out))
groupM.orange.pd.1 <- as.data.frame(groupM.orange.pd.1)
colnames(groupM.orange.pd.1) <- c("Group", "coral_type", "img")
groupM.orange.out <- cbind(groupM.orange.pd.1, groupM.orange.out)

groupM.pink.pd.1 <- cbind("GroupM", "pink_coral", rownames(groupM.orange.out))
groupM.pink.pd.1 <- as.data.frame(groupM.pink.pd.1)
colnames(groupM.pink.pd.1) <- c("Group", "coral_type", "img")
groupM.pink.out <- cbind(groupM.pink.pd.1, groupM.pink.out)


all.pd.tc <- rbind(groupO.orange.out, groupO.pink.out, 
                groupP.orange.out, groupP.pink.out,
                groupM.orange.out, groupM.pink.out)
rownames(all.pd.tc) <- NULL

save(groupO.orange.out, groupO.pink.out, 
     groupP.orange.out, groupP.pink.out,
     groupM.orange.out, groupM.pink.out,
     all.pd.tc, file = "results/all.pd.tc.RData")

write.csv(all.pd.tc, file = "results/all.pd.tc.csv", row.names = FALSE)


# Plot PD values ----------------------------------------------------------

load(file = "results/all.pd.tc.RData")

library(ggplot2)
library(tidyr)

# geom split violin function ----------------------------------------------

library(ggplot2)
GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                         1))
                               quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           })

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}


# lengthen PD data --------------------------------------------------------

pd.dat <- all.pd.tc[,c(1, 2, 3, 4, 5, 7)]

pd.dat.long <- pivot_longer(pd.dat, 
                            cols = 4:6,
                            names_to = "model_fit",
                            values_to = "Pd_value")
pd.dat.long$Group <- as.factor(pd.dat.long$Group)
pd.dat.long$coral_type <- as.factor(pd.dat.long$coral_type)
pd.dat.long$img <- as.factor(pd.dat.long$img)
pd.dat.long$model_fit <- as.factor(pd.dat.long$model_fit)

#group O pd 

o.pd <- ggplot(pd.dat.long[1:21,], 
               aes(x = model_fit, y = Pd_value, fill = coral_type)) + 
  geom_violin() + 
  scale_fill_manual(values=c("darkorange", "hotpink")) +
  ggtitle("Goodnes of Fit: Group O - Orange dominated") + 
  geom_point(position = position_jitterdodge(jitter.width = 0.25),
             aes(shape = coral_type)) +
  geom_point(aes(y = 0.9596), size = 4, colour = "hotpink") +
  theme(legend.position = "bottom") + 
  ylim(0, 1) + 
  geom_boxplot(width = 0.05, position = position_dodge(0.01),
               notch = FALSE)

o.pd

#group p split

p.split.pd <- ggplot(pd.dat.long[22:108,], 
                     aes(x = model_fit, y = Pd_value, fill = coral_type)) + 
  geom_split_violin() + 
  scale_fill_manual(values=c("darkorange", "hotpink")) +
  ggtitle("Goodnes of Fit: Group P - Pink dominated") + 
  geom_point(position = position_jitterdodge(jitter.width = 0.25),
             aes(shape = coral_type)) +
  theme(legend.position = "bottom") + 
  ylim(0, 1) + 
  geom_boxplot(width = 0.15, position = position_dodge(0.3),
               notch = FALSE)
p.split.pd


#group M split

m.split.pd <- ggplot(pd.dat.long[109:162,], 
                     aes(x = model_fit, y = Pd_value, fill = coral_type)) + 
  geom_split_violin() + 
  scale_fill_manual(values=c("darkorange", "hotpink")) +
  ggtitle("Goodnes of Fit: Group M - Mixed") + 
  geom_point(position = position_jitterdodge(jitter.width = 0.25),
             aes(shape = coral_type)) +
  theme(legend.position = "bottom") + 
  ylim(0, 1) + 
  geom_boxplot(width = 0.15, position = position_dodge(0.3),
               notch = FALSE)
m.split.pd

#stitch together (Figure 5)

library(gridExtra)
all.split.pd <- grid.arrange(o.pd, p.split.pd, m.split.pd, 
                             ncol = 3)


