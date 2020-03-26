##########################################################################
# policyMakingPowersDoubleCluster.R
# GR: November 8, 2017
# This file reads policy-making powers for lower and upper houses and
# executives.
# It first performs principal component analysis of common powers in both houses
# keeping as many dimensions as needed.
# It then finds the optimal number of clusters for both sets of houses
# In a second run, a new cluster analysis is performed that takes as input
# the clusters from the first run, and the "additional" powers that we have
# for lower houses and upper houses (this second step is carried out separately
# in upper and lower houses)
##########################################################################

library (psych)

#################################
#### Import and prepare data ####
#################################
LowerHouse <- read.table ("lowerHousePowers.txt", sep="\t", header=TRUE)
UpperHouse <- read.table ("upperHousePowers.txt", sep="\t", header=TRUE)
Executive  <- read.table ("presidentialPowers.txt", sep="\t", header=TRUE)

LH.label <- paste(tolower (substr(LowerHouse$country, start=1, stop=3)), LowerHouse$startyear, sep="-")
UH.label <- paste(tolower (substr(UpperHouse$country, start=1, stop=3)), UpperHouse$startyear, sep="-")
PR.label <- paste(tolower (substr(Executive$country, start=1, stop=3)), Executive$startyear, sep="-")
LowerHouse$chamber <- rep("lower", nrow(LowerHouse))
UpperHouse$chamber <- rep("upper", nrow(UpperHouse))

sharedPowers <- c("amend","partialobservationsoverride","vetooverride"
                  ,"budgetspending","question")
lowHouseOnly <- c("committeeslo","stafflo","introdbillslo")
uppHouseOnly <- c("vetoupper","introdbillsup")

Leg.W <- rbind (LowerHouse[,sharedPowers], UpperHouse[,sharedPowers])
Chamber <- c(LowerHouse$chamber, UpperHouse$chamber)
Leg.add <- rowSums(Leg.W) # simple additive scores


Leg.label <- c(paste("L", tolower (substr(LowerHouse$country, start=1, stop=3))
                     , LowerHouse$startyear, sep="-")
               , paste("U", tolower (substr(UpperHouse$country, start=1, stop=3))
                       , UpperHouse$startyear, sep="-"))

# Principal components Executives
PR.W <- Executive[,!is.element(colnames(Executive), c("country","startyear","endyear"))]
PR.add <- rowSums (PR.W)

pc.PR <- princomp(PR.W, scores=T)
screeplot (pc.PR)
summary(pc.PR) # 2 principal components might be necessary
plot (pc.PR$scores[,1], pc.PR$scores[,2], ylab="PC2 scores", xlab="PC1 scores"
      , type="n", xlim=c(-2.5,2.5), main="Executive")
text (xy.coords(pc.PR$scores[,1], jitter(pc.PR$scores[,2], amount=0.115)), labels=PR.label, cex=0.8)
plot (pc.PR$scores[,1], PR.add, ylab="Additive scores", xlab="PC1 scores"
      , type="n", xlim=c(-2.5,2.5), main="Executive")
text (xy.coords(pc.PR$scores[,1], jitter(PR.add, amount=0.5)), labels=PR.label, cex=0.8)

PR.scores <- as.matrix (pc.PR$scores[,c(1:2)])
rownames(PR.scores) <- paste (Executive$country, Executive$startyear, Executive$endyear, sep="-")


################################
#### First round clustering ####
################################

# Principal components all chambers together 
pc.Leg <- princomp(Leg.W, scores=T)
screeplot (pc.Leg)
summary(pc.Leg) # 1 principal component is enough
plot (pc.Leg$scores[,1], pc.Leg$scores[,2],# Leg.add,
      ylab="PC2 scores", xlab="PC1 scores"
      , type="n", xlim=c(-2.5,2.5), main="Full Legislature")
text (xy.coords(pc.Leg$scores[,1],pc.Leg$scores[,2]),# jitter(Leg.add, amount=0.5)),
                labels=Leg.label, cex=0.8)

Leg.scores <- as.matrix (pc.Leg$scores[,1:2])
rownames(Leg.scores) <- c(paste (LowerHouse$country, LowerHouse$startyear
                                 , LowerHouse$endyear, sep="-")
                          , paste (UpperHouse$country, UpperHouse$startyear
                                 , UpperHouse$endyear, sep="-"))

# Runs with a single clustering round for lower and upper houses
Lower <- LowerHouse[,is.element(colnames(LowerHouse), c(sharedPowers, lowHouseOnly))]
pc.Lower <- princomp (Lower, scores=T)
screeplot (pc.Lower)
summary (pc.Lower) # 2 components
plot (pc.Lower$scores[,1], pc.Lower$scores[,2],# Leg.add,
      ylab="PC2 scores", xlab="PC1 scores"
      , type="n", xlim=c(-2.5,2.5), main="Lower House only")
text (xy.coords(pc.Lower$scores[,1],pc.Lower$scores[,2]),# jitter(Leg.add, amount=0.5)),
      labels=Leg.label[1:39], cex=0.8)

Lower.scores <- as.matrix (pc.Lower$scores[,1:2])
rownames(Lower.scores) <- paste (LowerHouse$country, LowerHouse$startyear
                                 , LowerHouse$endyear, sep="-")

Upper <- UpperHouse[,is.element(colnames(UpperHouse), c(sharedPowers, uppHouseOnly))]
pc.Upper <- princomp (Upper, scores=T)
screeplot (pc.Upper)
summary (pc.Upper) # 2 components
plot (pc.Upper$scores[,1], pc.Upper$scores[,2],# Leg.add,
      ylab="PC2 scores", xlab="PC1 scores"
      , type="n", xlim=c(-2.5,2.5), main="Upper House only")
text (xy.coords(pc.Upper$scores[,1],pc.Upper$scores[,2]),# jitter(Leg.add, amount=0.5)),
      labels=Leg.label[40:length(Leg.label)], cex=0.8)

Upper.scores <- as.matrix (pc.Upper$scores[,1:2])
rownames(Upper.scores) <- paste (UpperHouse$country, UpperHouse$startyear
                                 , UpperHouse$endyear, sep="-")


#################################
#### Finding kmeans clusters ####
#################################
# These two functions help find optimal number of clusters visually
# findKmeans iterates from 1 to 8 clusters, producing Between SS and Within SS to be analyzed later
findKmeans <- function (x) {
  obj2return <- list ()
  for (i in 1:8) {
    obj2return[[i]] <- kmeans(x, centers=i, nstart=20)
  }
  return (obj2return)
}

# screeLikePlot produces a scree plot of the ratio of between to total SS
# The idea is to take as many clusters as needed until reaching the "bend" in the "elbow"
screeLikePlot <- function (x) {
  tmp <- c()
  for (i in 1:length(x)) {
    tmp[i] <- 100*(x[[i]]$betweenss/x[[i]]$totss)
  }
  return (tmp)
}

# legislature scores
temp <- findKmeans (Leg.scores)
plot (screeLikePlot (temp), type="b"
      , main="optimal # of clusters for Legislature PC scores, first run") 
Leg.cluster <- kmeans(Leg.scores, centers=3, nstart=20) # I see 3 clusters

# Lower house scores
temp <- findKmeans (Lower.scores)
plot (screeLikePlot (temp), type="b"
      , main="optimal # of clusters for Lower House PC scores, first and only run") 
Lower.cluster <- kmeans(Lower.scores, centers=3, nstart=20) # I see 3 clusters

# Upper house scores
temp <- findKmeans (Upper.scores)
plot (screeLikePlot (temp), type="b"
      , main="optimal # of clusters for Upper House PC scores, first and only run") 
Upper.cluster <- kmeans(Upper.scores, centers=3, nstart=20) # I see 3 clusters


# Presidential scores
temp <- findKmeans (PR.scores)
plot (screeLikePlot (temp), type="b"
      , main="optimal # of clusters for Executive PC scores") # I see 3
PR.cluster <- kmeans(PR.scores, centers=3, nstart=20)



#######################
#### Plots to keep ####
#######################

# Executive
graphicsPath <- "Graphs/"  # Save copies here as well
pdf (paste0 (graphicsPath, "ExecutiveClustering.pdf"), h=5, w=7)
plot (pc.PR$scores[,1], pc.PR$scores[,2], ylab="PC2 scores", xlab="PC1 scores"
      , type="n", xlim=c(-3,2.5), main="Executive")
text (xy.coords(pc.PR$scores[,1], jitter(pc.PR$scores[,2], amount=0.115))
      , labels=PR.label, cex=0.8, col="gray")
points (xy.coords(PR.cluster$centers), pch=rownames(PR.cluster$centers), cex=1.3)
dev.off ()

# Full legislature
plot (pc.Leg$scores[,1],pc.Leg$scores[,2],  xlab="PC1 scores",ylab="PC2 scores",
            #, xlim=c(-2.5,2.5),
      pch="", main="Full Legislature")
text (pc.Leg$scores[,1],pc.Leg$scores[,2], #1,
       labels=Leg.label, cex=0.8, col="gray", srt=45)
points (Leg.cluster$centers[,1],Leg.cluster$centers[,2],# rep(1, length(Leg.cluster$centers)),
        pch=rownames(Leg.cluster$centers), cex=1.3)


# Lower House 1 round clustering
pdf (paste0 (graphicsPath, "LowerHouseClustering.pdf"), h=5, w=7)
plot (pc.Lower$scores[,1], pc.Lower$scores[,2], ylab="PC2 scores", xlab="PC1 scores"
      , type="n", xlim=c(-3,2.5), main="Lower House (1 round)")
text (xy.coords(pc.Lower$scores[,1], jitter(pc.Lower$scores[,2], amount=0.115))
      , labels=Leg.label[1:39], cex=0.8, col="gray")
points (xy.coords(Lower.cluster$centers), pch=rownames(Lower.cluster$centers), cex=1.3)
dev.off ()

# Upper House 1 round clustering
pdf (paste0 (graphicsPath, "UpperHouseClustering.pdf"), h=5, w=7)
plot (pc.Upper$scores[,1], pc.Upper$scores[,2], ylab="PC2 scores", xlab="PC1 scores"
      , type="n", xlim=c(-3,2.5), main="Upper House (1 round)")
text (xy.coords(pc.Upper$scores[,1], jitter(pc.Upper$scores[,2], amount=0.115))
      , labels=Leg.label[40:length(Leg.label)], cex=0.8, col="gray")
points (xy.coords(Upper.cluster$centers), pch=rownames(Upper.cluster$centers), cex=1.3)
dev.off ()

#################################
#### Second round clustering ####
#################################
   
   # Principal component step
   pc.LH <- princomp(cbind(LowerHouse[,lowHouseOnly],Leg.cluster$cluster[Chamber=="lower"]), scores=T)
   screeplot (pc.LH)
   summary(pc.LH) # 1 components is enough
   LH.scores <- as.matrix (pc.LH$scores[,1])
   
   
   pc.UH <- princomp(cbind(UpperHouse[,uppHouseOnly],Leg.cluster$cluster[Chamber=="upper"]), scores=T)
   screeplot (pc.UH)
   summary(pc.UH) # 1 component is enough, there's only two variables
   UH.scores <- as.matrix (pc.UH$scores[,1])
   
   temp <- findKmeans (LH.scores)
   plot (screeLikePlot (temp), type="b"
         , main="optimal # of clusters for Lower Houses, second run") # I see 3 clusters 
   LH.cluster <- kmeans(LH.scores, centers=3, nstart=20) 
   
   
   temp <- findKmeans (UH.scores)
   plot (screeLikePlot (temp), type="b"
         , main="optimal # of clusters for Upper Houses, second run")# I see 4 clusters 
   UH.cluster <- kmeans(UH.scores, centers=3, nstart=20) 
   
######################################################
#### Hardwire additional column with fixed labels ####
######################################################

## Presidents
#weak
label.weak <- PR.cluster$cluster[which(names (PR.cluster$cluster)=="Argentina-1957-1993")]
#proactive
label.proac <- PR.cluster$cluster[which(names (PR.cluster$cluster)=="Argentina-1994-2015")]

executive.label <- ifelse (PR.cluster$cluster==label.weak, "Weak president",
                           ifelse (PR.cluster$cluster==label.proac
                                   , "Proactive president"
                                   , "Agenda-setting president"))

## Lower House
label.only  <- LH.cluster$cluster[which(names (LH.cluster$cluster)=="Bolivia-2010-2015")]
label.limited <- LH.cluster$cluster[which(names (LH.cluster$cluster)=="Argentina-1983-2015")]

lh.label <- ifelse (LH.cluster$cluster==label.only, "Only budget",
                           ifelse (LH.cluster$cluster==label.limited
                                   , "Limited powers"
                                   , "Except budget"))

## Upper House
label.only.uh  <- UH.cluster$cluster[which(names (UH.cluster$cluster)=="Bolivia-2010-2015")]
label.limited.uh <- UH.cluster$cluster[which(names (UH.cluster$cluster)=="Argentina-1983-2015")]

uh.label <- ifelse (UH.cluster$cluster==label.only.uh, "Only budget",
                    ifelse (UH.cluster$cluster==label.limited.uh
                            , "Limited powers"
                            , "Except budget"))

############################
#### More plots to keep ####
############################
pdf (paste0 (graphicsPath, "LowerHouseClustering2ndRound.pdf"), h=5, w=7)
stripchart (pc.LH$scores[,1],  xlab="PC1 scores (second round)"
            ,type="n", xlim=c(-2.5,2.5), main="Lower House", ylab="")
text (pc.LH$scores[,1], 1,
      labels=names(LH.cluster$cluster), cex=0.8, col="gray", srt=45)
text (LH.cluster$centers, rep(1, length(LH.cluster$centers)), labels=unique(lh.label), cex=1.3, srt=45)
dev.off()

pdf (paste0 (graphicsPath, "UpperHouseClustering2ndRound.pdf"), h=5, w=7)
stripchart (pc.UH$scores[,1],  xlab="PC1 scores (second round)"
            ,type="n", xlim=c(-2.5,2.5), main="Upper House", ylab="")
text (pc.UH$scores[,1], 1,
      labels=names(UH.cluster$cluster), cex=0.8, col="gray", srt=45)
text (UH.cluster$centers, rep(1, length(UH.cluster$centers)), labels=unique(uh.label), cex=1.3, srt=45)
dev.off()

# Gather all important variables together
Executive  <- data.frame (cbind (Executive, PR.cluster$cluster, pc.PR$scores[,1], pc.PR$scores[,2], president.type=executive.label))

LH <- data.frame (cbind (LowerHouse[,!is.element(colnames(LowerHouse), c("investigate","amnesty"))]
                         , firstRoundPC1=as.matrix (pc.Lower$scores[,1])
                         , firstRoundPC2=as.matrix (pc.Lower$scores[,2])
                         , clusterOnlyRound1=Lower.cluster$cluster
                         , secondRoundPC=LH.scores
                         , clusterRound1=Leg.cluster$cluster[Chamber=="lower"]
                         , clusterRound2=LH.cluster$cluster
                         , lower.house.type=lh.label))

UH <- data.frame (cbind (UpperHouse[,!is.element(colnames(UpperHouse), c("investigate","amnesty"))]
                         , firstRoundPC1=as.matrix (pc.Upper$scores[,1])
                         , firstRoundPC2=as.matrix (pc.Upper$scores[,2])
                         , clusterOnlyRound1=Upper.cluster$cluster
                         , secondRoundPC=UH.scores
                         , clusterRound1=Leg.cluster$cluster[Chamber=="upper"]
                         , clusterRound2=UH.cluster$cluster
                         , upper.house.type=uh.label))

# Save results for Brian
setwd ("Datasets/OriginalDataFiles/PolicyMakingPowers/")
write.table (LH, file="LowerHouseDoubleClusters.txt", sep="\t", row.names=F)
write.table (UH, file="UpperHouseDoubleClusters.txt", sep="\t", row.names=F)
write.table (Executive, file="ExecutiveClusters.txt", sep="\t", row.names=F)





