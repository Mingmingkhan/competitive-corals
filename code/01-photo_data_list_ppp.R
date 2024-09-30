#import all the data files, create list of data frames with 
#coral positions, identify groups, and generate point patterns 

setwd("data/coral_points_cm/")
library(dplyr)


# Import metadata ---------------------------------------------------------


list_csv_files <- list.files()
dat.names <- list_csv_files
dat.names <- gsub('data.','', dat.names)
dat.names <- gsub('.csv','', dat.names)
dat.list <- vector("list", length(dat.names))

dat.list <- lapply(list_csv_files, 
          function(x) read.csv(x, stringsAsFactors = FALSE))
names(dat.list) <- dat.names

setwd(system("git rev-parse --show-toplevel", intern=T))

#Get the metadata for relevant photos 
#(taken from PANGAEA, converted to CSV)

dat <- read.csv(file = "data/ps118_69-1_depth.csv")
marked <- list.files("SVGs_raw/")
marked <- gsub(' ','_', marked)
marked <- gsub('.svg','', marked)
marked <- as.data.frame(marked)
colnames(marked) <- "name"

#Get metadata for the relevant photos only 
marked <- merge(marked, dat, by.x = "name", 
                by.y = "name")

#use short form of the name 
nm <- marked$name
nm <- stringr::str_split_fixed(nm, "IMG_", 2)
nm <- nm[,2]
nm <- paste(rep("img", times = 36), nm, sep = "")
marked$nm <- nm

#export these for GIS 
write.csv(marked, file = "data/photo_coords.csv")


# Collate abundance -------------------------------------------------------

coral.counts <- list()
for (i in 1:length(dat.list)){
  temp <- dat.list[[i]]
  #extract the pink and orange 
  pink_c <- temp[temp$Desc == "pink",]
  orange_c <- temp[temp$Desc == "orange",]
  counts <- c(nrow(pink_c), nrow(orange_c)) #how many orange/pink
  tot <- sum(counts)
  props <- as.matrix(c(nrow(pink_c)/tot, nrow(orange_c)/tot))
  props <- t(props)
  res <- cbind(counts[1], counts[2], props)
  colnames(res) <- c("pink_count", "orange_count", "pink_prop", "orange_prop")
  coral.counts[[i]] <- res
}

names(coral.counts) <- dat.names
coral.counts <- do.call(rbind, coral.counts)
row.names(coral.counts) <- dat.names
coral.counts <- as.data.frame(coral.counts)

write.csv(coral.counts, file = "data/coral.counts.props.csv")

#use hclust to assign splits using coral proportions ----

cols <-c("darkorange", "deeppink", "purple")

dist <- vegan::vegdist(coral.counts[,3:4], method = "bray")
hc <- hclust(dist, method = "average")
plot(hc, main = "Coral Groups, Cluster Method: Average")
hc.gr <- rect.hclust(hc, k = 3, border = cols, cluster = NULL) 
#divide into 3 groups

#Use the outputs of hc.gr to get the grouped data frames 
get.group.dat <- function(x){
  #x is the output from hc.gr[[i]]
  x <- as.data.frame(x)
  nm <- rownames(x)
  x.dat <- as.data.frame(nm)
  x.dat <- merge(x.dat, marked, by.x = "nm", by.y = "nm")
  return(x.dat)
}

get.group.corals <- function(x){
  #x is the output from hc.gr[[i]]
  x <- as.data.frame(x)
  ind <- x$x
  group.dat.list <- vector("list", length(ind))
  names(group.dat.list) <- rownames(x)
  for (i in 1:length(group.dat.list)) {
    ind2 <- ind[i]
    temp <- dat.list[[ind2]]
    group.dat.list[[i]] <- temp
  }
  return(group.dat.list)
}

#mostly orange
groupO.dat <- get.group.dat(hc.gr[[1]])
groupO.corals <- get.group.corals(hc.gr[[1]])
groupO.names <- names(get.group.corals(hc.gr[[1]]))

#mostly pink
groupP.dat <- get.group.dat(hc.gr[[2]])
groupP.corals <- get.group.corals(hc.gr[[2]])
groupP.names <- names(get.group.corals(hc.gr[[2]]))

#mixed
groupM.dat <- get.group.dat(hc.gr[[3]])
groupM.corals <- get.group.corals(hc.gr[[3]])
groupM.names <- names(get.group.corals(hc.gr[[3]]))

# save the data list for use in future scripts 

save(dat.list, marked, dat.names, groupO.corals, groupO.dat, 
     groupP.corals, groupP.dat, groupM.corals, groupM.dat, 
     groupO.names, groupP.names, groupM.names, coral.counts, 
     file = "data/CoralsDataList.RData")


# create bar plots of abundance/props -------------------------------------

load(file = "data/CoralsDataList.RData")

corals.dat <- read.csv(file = "data/coral.counts.props.csv")

groupO.names <- as.data.frame(groupO.names)
colnames(groupO.names) <- "img"

groupP.names <- as.data.frame(groupP.names)
colnames(groupP.names) <- "img"

groupM.names <- as.data.frame(groupM.names)
colnames(groupM.names) <- "img"

groupO.counts <- merge(groupO.names, corals.dat, by.x = "img", by.y = "img")
x <- rep("GroupO", times = 6)
#y <- rep("red_coral", times = 6)
groupO.counts <- cbind(x, groupO.counts)

groupP.counts <- merge(groupP.names, corals.dat, by.x = "img", by.y = "img")
x <- rep("GroupP", times = 21)
#y <- rep("red_coral", times = 6)
groupP.counts <- cbind(x, groupP.counts)

groupM.counts <- merge(groupM.names, corals.dat, by.x = "img", by.y = "img")
x <- rep("GroupM", times = 9)
#y <- rep("red_coral", times = 6)
groupM.counts <- cbind(x, groupM.counts)

all.counts <- rbind(groupO.counts, groupP.counts, groupM.counts)
colnames(all.counts) <- c("Group", "img", "pink_coral", "orange_coral", 
                          "pink_prop", "orange_prop")

# make long 

library(tidyr)
all.counts.long <- pivot_longer(data = all.counts, 
                                cols = c("pink_coral", "orange_coral"), 
                                names_to = "coral_type",
                                values_to = "coral_count")
all.counts.long <- all.counts.long[,c(1, 2, 5, 6)]
all.counts.long$Group <- as.factor(all.counts.long$Group)
all.counts.long$coral_type <- as.factor(all.counts.long$coral_type)

#barplot 
library(ggplot2)
library(forcats)

o.bar.plot <- ggplot(data = all.counts.long[1:12,], 
                     aes (x = img, y = coral_count, fill = coral_type)) + 
  geom_bar(stat = "identity") +  
  scale_fill_manual(values = c("hotpink", "darkorange")) +
  coord_flip() + 
  ggtitle("Group O: Dominated by orange") +
  ylim(0, 450) + 
  theme(legend.position = "none")
o.bar.plot

p.bar.plot <- ggplot(data = all.counts.long[13:54,], 
                     aes (x = img, y = coral_count, fill = coral_type)) + 
  geom_bar(stat = "identity") +  
  scale_fill_manual(values = c("hotpink", "darkorange")) +
  coord_flip() + 
  ggtitle("Group P: Dominated by Pink") +
  ylim(0, 450) +
  theme(legend.position = "none")
p.bar.plot

m.bar.plot <- ggplot(data = all.counts.long[55:72,], 
                     aes (x = img, y = coral_count, fill = coral_type)) + 
  geom_bar(stat = "identity") +  
  scale_fill_manual(values = c("hotpink", "darkorange")) +
  coord_flip() + 
  ggtitle("Group C: Mixed") +
  ylim(0, 450) +
  theme(legend.position = "bottom")
m.bar.plot

all.abu.bar <- gridExtra::grid.arrange(o.bar.plot, p.bar.plot, m.bar.plot, 
                                       ncol = 1)

windows(h = 21, w = 10)
all.abu.bar

library(svglite)

ggsave(file = "figures/barplots.svg", plot = all.abu.bar, 
       h= 21, w = 10, scale = 1)
dev.off()


# create planar point patterns (ppp) ------------------------------------------------

library(spatstat)

load(file = "data/CoralsDataList.RData")

corals.dat <- read.csv(file = "data/coral.counts.props.csv")


# Group O: mostly orange -------------------------------------------------------------

groupO.orange.ppp <- vector("list", length = length(groupO.corals))
names(groupO.orange.ppp) <- groupO.names

for (i in 1:length(groupO.corals)) {
  data1 <- groupO.corals[[i]]
  orange_c <- data1[data1$Desc == "orange",]
  if (nrow(orange_c) >= 30) {
    win.cc <- as.rectangle(owin(c(0,175),c(0,175)))
    ppp.orange <- ppp(orange_c[,4], orange_c[,5], window = win.cc)
    groupO.orange.ppp[[i]] <- ppp.orange
  }
  
}

#Only img0483 has enough pink for a ppp 
data1 <- groupO.corals[[1]]
pink_c <- data1[data1$Desc == "pink",]
win.cc <- as.rectangle(owin(c(0,175),c(0,175)))
ppp.img0483.pink <- ppp(pink_c[,4], pink_c[,5], window = win.cc)

save(groupO.names,
     ppp.img0483.pink, groupO.orange.ppp, 
     file = "data/GroupO_coral_ppp_marked.RData")

# Group P: Most pink corals ----------------------------------------------------------

#get the orange ppp's first 
groupP.orange.ppp <- vector("list", length = length(groupP.corals))
names(groupP.orange.ppp) <- groupP.names

for (i in 1:length(groupP.corals)) {
  data1 <- groupP.corals[[i]]
  orange_c <- data1[data1$Desc == "orange",]
  if (nrow(orange_c) >= 30) {
    win.cc <- as.rectangle(owin(c(0,175),c(0,175)))
    ppp.orange <- ppp(orange_c[,4], orange_c[,5], window = win.cc)
    groupP.orange.ppp[[i]] <- ppp.orange
  }
  
}

#get the pink ppps 

groupP.pink.ppp <- vector("list", length = length(groupP.corals))
names(groupP.pink.ppp) <- groupP.names

for (i in 1:length(groupP.corals)) {
  data1 <- groupP.corals[[i]]
  pink_c <- data1[data1$Desc == "pink",]
  if (nrow(pink_c) >= 30) {
    win.cc <- as.rectangle(owin(c(0,175),c(0,175)))
    ppp.pink <- ppp(pink_c[,4], pink_c[,5], window = win.cc)
    groupP.pink.ppp[[i]] <- ppp.pink
  }
  
}


save(groupP.names, 
     groupP.pink.ppp,
     groupP.orange.ppp, file = "data/GroupP_coral_ppp_marked.RData"
)





# Group M: mixed coral groups ---------------------------------------------

#get the orange ppp's first 
groupM.orange.ppp <- vector("list", length = length(groupM.corals))
names(groupM.orange.ppp) <- groupM.names

for (i in 1:length(groupM.corals)) {
  data1 <- groupM.corals[[i]]
  orange_c <- data1[data1$Desc == "orange",]
  if (nrow(orange_c) >= 30) {
    win.cc <- as.rectangle(owin(c(0,175),c(0,175)))
    ppp.orange <- ppp(orange_c[,4], orange_c[,5], window = win.cc)
    groupM.orange.ppp[[i]] <- ppp.orange
  }
  
}

#get the pink ppps 

groupM.pink.ppp <- vector("list", length = length(groupM.corals))
names(groupM.pink.ppp) <- groupM.names

for (i in 1:length(groupM.corals)) {
  data1 <- groupM.corals[[i]]
  pink_c <- data1[data1$Desc == "pink",]
  if (nrow(pink_c) >= 30) {
    win.cc <- as.rectangle(owin(c(0,175),c(0,175)))
    ppp.pink <- ppp(pink_c[,4], pink_c[,5], window = win.cc)
    groupM.pink.ppp[[i]] <- ppp.pink
  }
  
}

save(groupM.names, 
     groupM.pink.ppp,
     groupM.orange.ppp, file = "data/GroupM_coral_ppp_marked.RData"
)


