library(spatstat)
library(selectspm)


# Group O: Mostly Orange -----------------------------------------------------------------

load(file = "data/GroupO_coral_ppp_marked.RData")
load(file = "data/GroupO_orange_bg_list.RData") #this is a highly 
#compressed RData file that contains the heterogenous substrate background


# first orange ------------------------------------------------------------

groupO.orange.res <- vector("list", length(groupO.bg))
names(groupO.orange.res) <- groupO.names

for (i in 2:length(groupO.names)){
  #extract the parameters 
  img <- groupO.names[i]
  ppp1 <- groupO.orange.ppp[[i]]
  im.ppp1 <- groupO.bg[[i]]
  
  #create results 
  res<- vector("list", length = 3)
  res[[1]]<- vector("list", length = 4) # fit
  res[[2]]<- vector("list", length = 4) #envpcf
  res[[3]]<- vector("list", length = 5) #gof 
  
  
  #fit all the models
  res[[1]][[1]]<-ppm(ppp1~1, statistic="pcf") #csr
  res[[1]][[2]]<-kppm(ppp1~im.ppp1,statistic="pcf") #hp 
  res[[1]][[3]]<-kppm(ppp1~1, statistic="pcf",cluster="Thomas", rmax = 20) #tc
  res[[1]][[4]]<-kppm(ppp1~im.ppp1, statistic="pcf",cluster="Thomas") #htc
  
  
  names(res)<-c("fit","envpcf","gof")
  names(res[[1]])<-c("csr","hp","tc","htc")
  names(res[[2]])<-c("csr","hp","tc","htc")
  names(res[[3]]) <- c("csr","hp","tc10","tc20", "htc")
  
  #get envelopes and goodness of fit 
  
  #csr envelope
  res[[2]][[1]] <- (envelope(res[[1]][[1]], pcf,
                             savefuns=TRUE,nsim=9999,nrank=500))
  #gof 
  res[[3]][[1]] <- LF.gof(res[[2]][[1]])
  
  
  #hp envelope
  res[[2]][[2]] <- (envelope(res[[1]][[2]], pcf,
                             savefuns=TRUE,nsim=9999,nrank=500))
  
  #gof 
  res[[3]][[2]] <- LF.gof(res[[2]][[2]])
  
  #thomas cluster
  #envelope
  res[[2]][[3]]<-(envelope(res[[1]][[3]], pcf,
                           savefuns=TRUE,nsim=9999,nrank=500))
  
  #gof rmax 10 
  res[[3]][[3]]<-LF.gof(res[[2]][[3]], rmin = 0, rmax = 10)
  #god rmax 20 
  res[[3]][[4]]<-LF.gof(res[[2]][[3]], rmin = 0, rmax = 20)
  
  #htc 
  #envelope
  res[[2]][[4]]<-(envelope(res[[1]][[4]], pcf,
                           savefuns=TRUE,nsim=9999,nrank=500))
  res[[3]][[5]]<-LF.gof(res[[2]][[4]])
  
  groupO.orange.res[[i]] <- res
}

#for img0483 separately -----------------
# first orange
img <- groupO.names[1]
ppp1 <- groupO.orange.ppp[[1]]
im.ppp1 <- groupO.bg[[1]]

#create results 
res<- vector("list", length = 3)
res[[1]]<- vector("list", length = 4) # fit
res[[2]]<- vector("list", length = 4) #envpcf
res[[3]]<- vector("list", length = 5) #gof 

#fit all the models
res[[1]][[1]]<-ppm(ppp1~1, statistic="pcf") #csr
res[[1]][[3]]<-kppm(ppp1~1, statistic="pcf",cluster="Thomas", rmax = 20) #tc

names(res)<-c("fit","envpcf","gof")
names(res[[1]])<-c("csr","hp","tc","htc")
names(res[[2]])<-c("csr","hp","tc","htc")
names(res[[3]]) <- c("csr","hp","tc10","tc20", "htc")

#get envelopes and goodness of fit 

#csr envelope
res[[2]][[1]] <- (envelope(res[[1]][[1]], pcf,
                           savefuns=TRUE,nsim=9999,nrank=500))
#gof 
res[[3]][[1]] <- LF.gof(res[[2]][[1]])

#thomas cluster
#envelope
res[[2]][[3]]<-(envelope(res[[1]][[3]], pcf,
                         savefuns=TRUE,nsim=9999,nrank=500))

#gof rmax 10 
res[[3]][[3]]<-LF.gof(res[[2]][[3]], rmin = 0, rmax = 10)
#god rmax 20 
res[[3]][[4]]<-LF.gof(res[[2]][[3]], rmin = 0, rmax = 20)

groupO.orange.res[[1]] <- res

#now pink

img <- groupO.names[1]
ppp1 <- ppp.img0483.pink
im.ppp1 <- groupO.bg[[1]]

#create results 
res<- vector("list", length = 3)
res[[1]]<- vector("list", length = 4) # fit
res[[2]]<- vector("list", length = 4) #envpcf
res[[3]]<- vector("list", length = 5) #gof 


#fit all the models
res[[1]][[1]]<-ppm(ppp1~1, statistic="pcf") #csr
res[[1]][[3]]<-kppm(ppp1~1, statistic="pcf",cluster="Thomas") #tc

names(res)<-c("fit","envpcf","gof")
names(res[[1]])<-c("csr","hp","tc","htc")
names(res[[2]])<-c("csr","hp","tc","htc")
names(res[[3]]) <- c("csr","hp","tc10","tc20", "htc")

#get envelopes and goodness of fit 

#csr envelope
res[[2]][[1]] <- (envelope(res[[1]][[1]], pcf,
                           savefuns=TRUE,nsim=9999,nrank=500))
#gof 
res[[3]][[1]] <- LF.gof(res[[2]][[1]])


#thomas cluster
#envelope
res[[2]][[3]]<-(envelope(res[[1]][[3]], pcf,
                         savefuns=TRUE,nsim=9999,nrank=500))

#gof rmax 10 
res[[3]][[3]]<-LF.gof(res[[2]][[3]], rmin = 0, rmax = 10)
#god rmax 20 
res[[3]][[4]]<-LF.gof(res[[2]][[3]], rmin = 0, rmax = 20)

groupO.pink.res <- res

save(groupO.pink.res, groupO.orange.res, 
     file = "results/groupO_4models_res.RData")
#Note, due to large file sizes, this file is not uploaded to 
#GitHub and can instead be found in the linked Dropbox. To 
#run further analyses on these results files, download it into
#the results folder 

# Group P: mostly pink ----------------------------------------------------

load(file = "data/GroupP_coral_ppp_marked.RData")
load(file = "data/GroupP_pink_bg_list.RData") #this is a highly 
#compressed RData file that contains the heterogenous substrate background


# first orange ------------------------------------------------------------

groupP.orange.res <- vector("list", length(groupP.orange.ppp))
names(groupP.orange.res) <- groupP.names

for (i in 1:21){
  if (length(groupP.orange.ppp[[i]]) == 5) {
    #extract the parameters 
    img <- groupP.names[i]
    ppp1 <- groupP.orange.ppp[[i]]
    im.ppp1 <- groupP.bg[[i]]
    
    #create results 
    res<- vector("list", length = 3)
    res[[1]]<- vector("list", length = 4) # fit
    res[[2]]<- vector("list", length = 4) #envpcf
    res[[3]]<- vector("list", length = 5) #gof 
    
    
    #fit all the models
    res[[1]][[1]]<-ppm(ppp1~1, statistic="pcf") #csr
    res[[1]][[2]]<-kppm(ppp1~im.ppp1,statistic="pcf") #hp 
    res[[1]][[3]]<-kppm(ppp1~1, statistic="pcf",cluster="Thomas", 
                        rmax = 20) #tc
    res[[1]][[4]]<-kppm(ppp1~im.ppp1, statistic="pcf",cluster="Thomas") #htc
    
    
    names(res)<-c("fit","envpcf","gof")
    names(res[[1]])<-c("csr","hp","tc","htc")
    names(res[[2]])<-c("csr","hp","tc","htc")
    names(res[[3]]) <- c("csr","hp","tc10","tc20", "htc")
    
    #get envelopes and goodness of fit 
    
    #csr envelope
    res[[2]][[1]] <- (envelope(res[[1]][[1]], pcf,
                savefuns=TRUE,nsim=9999,nrank=500))
    #gof 
    res[[3]][[1]] <- LF.gof(res[[2]][[1]])
    
    
    #hp envelope
    res[[2]][[2]] <- (envelope(res[[1]][[2]], pcf,
                               savefuns=TRUE,nsim=9999,nrank=500))
    
    #gof 
    res[[3]][[2]] <- LF.gof(res[[2]][[2]])
    
    #thomas cluster
    #envelope
    res[[2]][[3]]<-(envelope(res[[1]][[3]], pcf,
                             savefuns=TRUE,nsim=9999,nrank=500))
    
    #gof rmax 10 
    res[[3]][[3]]<-LF.gof(res[[2]][[3]], rmin = 0, rmax = 10)
    #god rmax 20 
    res[[3]][[4]]<-LF.gof(res[[2]][[3]], rmin = 0, rmax = 20)
    
    #htc 
    #envelope
    res[[2]][[4]]<-(envelope(res[[1]][[4]], pcf,
                             savefuns=TRUE,nsim=9999,nrank=500))
    res[[3]][[5]]<-LF.gof(res[[2]][[4]])
    
    
    groupP.orange.res[[i]] <- res
  }
}

save(groupP.orange.res, file = "results/groupP_orange_res.RData")


# Now pink corals ---------------------------------------------------------


groupP.pink.res <- vector("list", 21)
names(groupP.pink.res) <- groupP.names

for (i in 1:21){
  if (length(groupP.pink.ppp[[i]]) == 5) {
    img <- groupP.names[i]
    ppp1 <- groupP.pink.ppp[[i]]
    im.ppp1 <- groupP.bg[[i]]
    
    #create results 
    res<- vector("list", length = 3)
    res[[1]]<- vector("list", length = 4) # fit
    res[[2]]<- vector("list", length = 4) #envpcf
    res[[3]]<- vector("list", length = 5) #gof 
    
    
    #fit all the models
    res[[1]][[1]]<-ppm(ppp1~1, statistic="pcf") #csr
    res[[1]][[2]]<-kppm(ppp1~im.ppp1,statistic="pcf") #hp 
    res[[1]][[3]]<-kppm(ppp1~1, statistic="pcf",cluster="Thomas", rmax = 20) #tc
    res[[1]][[4]]<-kppm(ppp1~im.ppp1, statistic="pcf",cluster="Thomas") #htc
    
    
    names(res)<-c("fit","envpcf","gof")
    names(res[[1]])<-c("csr","hp","tc","htc")
    names(res[[2]])<-c("csr","hp","tc","htc")
    names(res[[3]]) <- c("csr","hp","tc10","tc20", "htc")
    
    #get envelopes and goodness of fit 
    
    #csr envelope
    res[[2]][[1]] <- (envelope(res[[1]][[1]], pcf,
                               savefuns=TRUE,nsim=9999,nrank=500))
    #gof 
    res[[3]][[1]] <- LF.gof(res[[2]][[1]])
    
    
    #hp envelope
    res[[2]][[2]] <- (envelope(res[[1]][[2]], pcf,
                               savefuns=TRUE,nsim=9999,nrank=500))
    
    #gof 
    res[[3]][[2]] <- LF.gof(res[[2]][[2]])
    
    #thomas cluster
    #envelope
    res[[2]][[3]]<-(envelope(res[[1]][[3]], pcf,
                             savefuns=TRUE,nsim=9999,nrank=500))
    
    #gof rmax 10 
    res[[3]][[3]]<-LF.gof(res[[2]][[3]], rmin = 0, rmax = 10)
    #god rmax 20 
    res[[3]][[4]]<-LF.gof(res[[2]][[3]], rmin = 0, rmax = 20)
    
    #htc 
    #envelope
    res[[2]][[4]]<-(envelope(res[[1]][[4]], pcf,
                             savefuns=TRUE,nsim=9999,nrank=500))
    res[[3]][[5]]<-LF.gof(res[[2]][[4]])
    
    
    groupP.pink.res[[i]] <- res
  }
}

save(groupP.pink.res, file = "results/groupP_pink_res.RData")

groupP.pink.res <- groupB.pink.res2


# Group M: Mixed assemblages ----------------------------------------------

load(file = "data/GroupM_coral_ppp_marked.RData.RData")
load(file = "data/GroupC-Mixed_backgrounds.RData") #this is a highly 
#compressed RData file that contains the heterogenous substrate background

#first orange ----
groupM.orange.res <- vector("list", 9)
names(groupM.orange.res) <- groupM.names

for (i in 1:9){
  if (length(groupM.orange.ppp[[i]]) == 5) {
    img <- groupM.names[i]
    ppp1 <- groupM.orange.ppp[[i]]
    im.ppp1 <- groupM.bg[[i]]
    
    #create results 
    res<- vector("list", length = 3)
    res[[1]]<- vector("list", length = 4) # fit
    res[[2]]<- vector("list", length = 4) #envpcf
    res[[3]]<- vector("list", length = 5) #gof 
    
    
    #fit all the models
    res[[1]][[1]]<-ppm(ppp1~1, statistic="pcf") #csr
    res[[1]][[2]]<-kppm(ppp1~im.ppp1,statistic="pcf") #hp 
    res[[1]][[3]]<-kppm(ppp1~1, statistic="pcf",cluster="Thomas", rmax = 20) #tc
    res[[1]][[4]]<-kppm(ppp1~im.ppp1, statistic="pcf",cluster="Thomas") #htc
    
    
    names(res)<-c("fit","envpcf","gof")
    names(res[[1]])<-c("csr","hp","tc","htc")
    names(res[[2]])<-c("csr","hp","tc","htc")
    names(res[[3]]) <- c("csr","hp","tc10","tc20", "htc")
    
    #get envelopes and goodness of fit 
    
    #csr envelope
    res[[2]][[1]] <- (envelope(res[[1]][[1]], pcf,
                               savefuns=TRUE,nsim=9999,nrank=500))
    #gof 
    res[[3]][[1]] <- LF.gof(res[[2]][[1]])
    
    
    #hp envelope
    res[[2]][[2]] <- (envelope(res[[1]][[2]], pcf,
                               savefuns=TRUE,nsim=9999,nrank=500))
    
    #gof 
    res[[3]][[2]] <- LF.gof(res[[2]][[2]])
    
    #thomas cluster
    #envelope
    res[[2]][[3]]<-(envelope(res[[1]][[3]], pcf,
                             savefuns=TRUE,nsim=9999,nrank=500))
    
    #gof rmax 10 
    res[[3]][[3]]<-LF.gof(res[[2]][[3]], rmin = 0, rmax = 10)
    #god rmax 20 
    res[[3]][[4]]<-LF.gof(res[[2]][[3]], rmin = 0, rmax = 20)
    
    #htc 
    #envelope
    res[[2]][[4]]<-(envelope(res[[1]][[4]], pcf,
                             savefuns=TRUE,nsim=9999,nrank=500))
    res[[3]][[5]]<-LF.gof(res[[2]][[4]])
    
    
    groupM.orange.res[[i]] <- res
  }
}

save(groupM.orange.res, file = "results/groupM_orange_res.RData")

#now pink -----

groupM.pink.res <- vector("list", 9)
names(groupM.pink.res) <- groupM.names

for (i in 1:9){
  if (length(groupM.pink.ppp[[i]]) == 5) {
    img <- groupM.names[i]
    ppp1 <- groupM.pink.ppp[[i]]
    im.ppp1 <- groupM.bg[[i]]
    
    #create results 
    res<- vector("list", length = 3)
    res[[1]]<- vector("list", length = 4) # fit
    res[[2]]<- vector("list", length = 4) #envpcf
    res[[3]]<- vector("list", length = 5) #gof 
    
    
    #fit all the models
    res[[1]][[1]]<-ppm(ppp1~1, statistic="pcf") #csr
    res[[1]][[2]]<-kppm(ppp1~im.ppp1,statistic="pcf") #hp 
    res[[1]][[3]]<-kppm(ppp1~1, statistic="pcf",cluster="Thomas", rmax = 20) #tc
    res[[1]][[4]]<-kppm(ppp1~im.ppp1, statistic="pcf",cluster="Thomas") #htc
    
    
    names(res)<-c("fit","envpcf","gof")
    names(res[[1]])<-c("csr","hp","tc","htc")
    names(res[[2]])<-c("csr","hp","tc","htc")
    names(res[[3]]) <- c("csr","hp","tc10","tc20", "htc")
    
    #get envelopes and goodness of fit 
    
    #csr envelope
    res[[2]][[1]] <- (envelope(res[[1]][[1]], pcf,
                               savefuns=TRUE,nsim=9999,nrank=500))
    #gof 
    res[[3]][[1]] <- LF.gof(res[[2]][[1]])
    
    
    #hp envelope
    res[[2]][[2]] <- (envelope(res[[1]][[2]], pcf,
                               savefuns=TRUE,nsim=9999,nrank=500))
    
    #gof 
    res[[3]][[2]] <- LF.gof(res[[2]][[2]])
    
    #thomas cluster
    #envelope
    res[[2]][[3]]<-(envelope(res[[1]][[3]], pcf,
                             savefuns=TRUE,nsim=9999,nrank=500))
    
    #gof rmax 10 
    res[[3]][[3]]<-LF.gof(res[[2]][[3]], rmin = 0, rmax = 10)
    #god rmax 20 
    res[[3]][[4]]<-LF.gof(res[[2]][[3]], rmin = 0, rmax = 20)
    
    #htc 
    #envelope
    res[[2]][[4]]<-(envelope(res[[1]][[4]], pcf,
                             savefuns=TRUE,nsim=9999,nrank=500))
    res[[3]][[5]]<-LF.gof(res[[2]][[4]])
    
    
    groupM.pink.res[[i]] <- res
  }
}

save(groupM.pink.res, file = "results/groupM_pink_res.RData")

