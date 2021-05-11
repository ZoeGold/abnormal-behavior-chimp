# Supplemental Code for “Individual Variation in the Abnormal Behavior Repertoire of Chimpanzees"
# Zoë Goldsborough, Elisabeth H.M. Sterck, Frans B.M. de Waal & Christine E. Webb (2021)

# remember to set working directory

# packages needed
require(dplyr)
require(ggplot2)
require(pastecs)
require(Hmisc)
require(vegan)
require(reshape2)
require(lubridate)
require(plyr)
require(asnipe)

########### GENERAL CLEAN-UP + TOTAL HOURS OBSERVED ############
# open file of focals (file contains only focals more than >75% observed)
FocalsClean <- read.delim("Focals_75_S.csv", header = TRUE, stringsAsFactors = TRUE, sep = ",")

# amount of focals per individual
aggClean <- unique(FocalsClean[c("focalID", "date")])
aggfocal <- aggregate(x = aggClean, by = list(unique.values = aggClean$focalID), FUN = length)

# create data frame with basic information on chimpanzees
MJD <- data.frame(ID = c("Erika", "Fons", "Gaby", "Geisha", "Ghineau", "Giambo", "Jimmie", "Jing", 
                         "Moni", "Moniek", "Morami", "Raimee", "Roosje", "Tesua", "Tushi"), 
                  birth_year = c(1992, 1975, 1984, 1993, 2005, 1989, 1960, 1981, 1989, 1977, 1987, 1999, 1979, 1986, 1992),
                  sex = factor(c(1,2,1,1,2,2,1,2,1,1,1,1,1,1,1), levels = c(1,2), labels = c("Female", "Male")))
MJD$Age <- 2018 - MJD$birth_year # age at time of observations
MJD$Rank <- factor(c(1,2,1,1,2,2,0,2,0,1,1,2,2,0,1), labels = c("Low", "Medium", "High"))
MJD$Rank2 <- c(1,2,1,1,2,2,0,2,0,1,1,2,2,1,1) #not as factor

# amount of focals per individual (more than 75% observed), a focal is 10 minutes
MJD$FocalAm <- aggfocal$focalID
# hours observed
# time not visible in minutes (calculated manually using raw focals file)
MJD$NV <- c(2, 2, 3, 0.5, 4, 0, 4, 2.5, 2.5, 1.5, 3.5, 1, 0, 1.5, 0)
MJD$Focalhrs <- ((MJD$FocalAm * 10)-MJD$NV)/60

# rate of each abnormal behavior for group comparison
allbehaviorFreq <- as.data.frame.matrix(table(FocalsClean$focalID, FocalsClean$behavior))
# select only abnormal behaviors
allbehaviorFreq <- allbehaviorFreq[, c("self_scratch","body_manipulation","clap", "coprophagy", "head_shake", "crossedarm_walk", "manipulate_feces","pluck","rock", "urine_interaction")]
MJD <- cbind(MJD, allbehaviorFreq)

MJD_Rate <- MJD[, 10:19]/MJD$Focalhrs
colnames(MJD_Rate) <- paste(colnames(MJD_Rate), "R", sep = "")
# add R at the end (make vector of names of columns or paste) then cbind to MJD
MJD <- cbind(MJD, MJD_Rate)

# Add repertoire size
AbnormalOnly <- MJD[, c("coprophagy", "body_manipulation", "head_shake", "crossedarm_walk", "pluck", "rock", "clap", "urine_interaction", "manipulate_feces")]
MJD$AbnNr <- rowSums(AbnormalOnly!=0)
# without crossed-arm walk because it's female-specific
AbnormalOnly_NoCW <- MJD[, c("coprophagy", "body_manipulation", "head_shake", "pluck", "rock", "clap", "urine_interaction", "manipulate_feces")]
MJD$AbnNr_No_CW <- rowSums(AbnormalOnly_NoCW!=0)
MJD$AbnRateT <- rowSums(subset(MJD, select=c(coprophagy,body_manipulation, head_shake, manipulate_feces, crossedarm_walk, pluck, rock, clap, urine_interaction)))/MJD$Focalhrs

# Global observations (supplementary)
GlobalData <- read.delim("DataGlobals_AbnS.csv", header = TRUE, stringsAsFactors = TRUE, sep= ",")
AbnormalGlobal <- as.matrix(ftable(GlobalData$ID1, GlobalData$behavior))
# compare repertoire size global and focal
MJD$AbnNr_Global <- rowSums(AbnormalGlobal!=0)
cor.test(MJD$AbnNr, MJD$AbnNr_Global, use = "pairwise.complete.obs", method = "spearman")

# compare rate global and focal
GlobalFreq <- as.data.frame.matrix(table(GlobalData$ID1, GlobalData$behavior))
MJD <- cbind(MJD, allbehaviorFreq)
# hours observed globals calculated manually in excel by subtracting time not visible from total amount of global hours observed (119)
MJD$Globalhrs <- c(118.633, 118.533, 118.633, 118.633, 118.583, 118.633, 118.633, 118.533, 118.633, 118.633, 118.633, 118.533, 118.633, 118.633, 115.633)
MJD$GlobalRate <- rowSums(GlobalFreq)/MJD$Globalhrs 
cor.test(MJD$GlobalRate, MJD$AbnRateT, use = "pairwise.complete.obs", method= "spearman")

#### RESULTS ####
#### PIE CHARTS PER INDIVIDUAL (FIGURE 1) #####
PieC <- MJD[, c("ID", "body_manipulation", "clap", "coprophagy", "head_shake", "crossedarm_walk",
                "manipulate_feces", "pluck", "rock", "urine_interaction")]
PieMelt <- melt(PieC, "ID")
PieMelt$Behavior <- PieMelt$variable
PieMelt$Behavior <- factor(PieMelt$Behavior, levels = c("body_manipulation", "clap", "coprophagy", 
                                                        "head_shake", "crossedarm_walk", "manipulate_feces", 
                                                        "pluck", "rock", "urine_interaction"), labels = c("Body manipulation", "Clap", 
                                                                                                          "Coprophagy", "Head shake","Crossed-arm walk", 
                                                                                                          "Manipulate feces", "Pluck", "Rock", 
                                                                                                          "Urine interaction"))
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.text  = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

# to export, uncomment the png or setEPS command and the dev.off command
#png("Piecharts.eps", width = 14, height = 10, units = 'in', res = 300)
#setEPS(postscript(file = "Piecharts.eps", width = 9, height = 6.9))
ggplot(PieMelt, aes(x="", y = value, fill = Behavior)) + 
  geom_bar(stat = "identity", width = 1, position = position_fill()) +
  coord_polar(theta = "y") + scale_fill_brewer(palette="Pastel1") +
  facet_wrap( ~ ID) + blank_theme
# dev.off()

# total abnormal rate manually added to pie chart using Adobe Illustrator
round(MJD$AbnRateT, 2)

# general information on rate and size
round(stat.desc(MJD$AbnNr), 2)
round(stat.desc(MJD$AbnNr_Global), 2)
round(stat.desc(MJD$AbnRateT),2)

#### DIVERSITY ####
MJD$divAbn = MJD$Age
for (j in 1:nrow(MJD)) {
  ratesAbn  = MJD[j,c("head_shakeR", "coprophagyR", "body_manipulationR", "manipulate_fecesR", "pluckR", "urine_interactionR",
                          "clapR", "rockR", "crossedarm_walkR")]
  MJD[j,"divAbn"] = 
    diversity(ratesAbn, index = "shannon", MARGIN = 1, base = exp(1))
}
MJD[,c("divAbn")]

MJD$Even <- MJD$divAbn/log(MJD$AbnNr)
round(stat.desc(MJD$Even), 2)

# evenness score was manually added to pie charts using Adobe Illustrator
round(MJD$Even, 2)

# Link to other factors
# sex
wilcox.test(Even ~ sex, data = MJD)
wilcox.test(AbnNr ~ sex, data = MJD)
aggregate(MJD$AbnNr, list(MJD$sex),median)
wilcox.test(AbnNr_No_CW ~ sex, data = MJD)
wilcox.test(AbnRateT ~ sex, data = MJD)

# age
cor.test(MJD$Age, MJD$Even, use = "pairwise.complete.obs", method = "spearman")
cor.test(MJD$Age, MJD$AbnNr, use = "pairwise.complete.obs", method = "spearman")
cor.test(MJD$AbnRateT, MJD$Age, use = "pairwise.complete.obs", method = "spearman")

# rank
kruskal.test(Even ~ Rank, data = MJD) 
kruskal.test(AbnNr ~ Rank, data = MJD) 
kruskal.test(AbnRateT ~ Rank, data = MJD)

# evenness and rate/size
cor.test(MJD$AbnRateT, MJD$Even, use = "pairwise.complete.obs", method = "spearman")
cor.test(MJD$AbnNr, MJD$Even, use = "pairwise.complete.obs", method = "spearman")

#### EVENNESS VS ABNORMAL BEHAVIOR RATE (FIGURE 2) ####

# png("EvenRate.png", width = 14, height = 10, units = 'in', res = 300)
# setEPS(postscript(file = "EvenRate.eps", width = 14, height = 10))
ggplot() +  geom_point(aes(x = MJD$AbnRateT, y = MJD$Even, shape = MJD$ID), size = 5) +  
   scale_shape_manual(values = 0:15) + geom_smooth(aes(x = MJD$AbnRateT, y = MJD$Even), method = "lm", colour = "black")+  
  theme_bw() +  labs(x = "Hourly Rate of Abnormal Behavior",y = "Evenness of Abnormal Repertoire", shape = "ID") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(colour="black", size = 12),
        axis.text.y = element_text(colour="black", size = 12),
        axis.title.x = element_text(colour= "black", size = 16),
        axis.title.y = element_text(colour= "black", size = 16),
        legend.text = element_text(colour = "black", size = 12),
        legend.title = element_text(colour = "black", size= 16),
        axis.line = element_line(size=0.5, colour = "black"))
# dev.off()

#### DISSIMILARITY #####
# make matrices
AbnMatrix =MJD[,c("head_shakeR", "coprophagyR", "body_manipulationR", "manipulate_fecesR", "pluckR", "urine_interactionR",
                 "clapR", "rockR", "crossedarm_walkR")]
AbnMatrix = scale(AbnMatrix) # standardize variables
row.names(AbnMatrix) = MJD$ID
EuclDistMatrixAbn = dist(AbnMatrix, method = "euclidean")
signif(EuclDistMatrixAbn, digits=3)
EDM = as.matrix(EuclDistMatrixAbn)
# score to indicate overall dissimilarity with group members
MJD$SI <- rowSums(EDM)/14

# what factors affect this score
cor.test(MJD$Age, MJD$SI, method = "spearman")
t.test(SI ~ sex, data = MJD)
wilcox.test(SI ~ sex, data = MJD)
kruskal.test(SI ~ Rank, data = MJD)

#### MATRIX REGRESSION: MRQAP-DSP ####
# regression of the EDM dissimilarity matrix, using predictors age, rank, sex, kinship
# functions needed
getRwCenteredMatrix <- function(X) {
  n1=nrow(X); n2=ncol(X)
  Xrw = X
  for (i in 1:n1) for (j in 1:n2) {
    if (i == j) {} else {
      Xrw[i,j] <- X[i,j] - rowSums(X,na.rm=T)[i]/(n1-1) # row means centering
    }
  }
  return(Xrw)
}
### y = getRwCenteredMatrix(y) ###
### x = getRwCenteredMatrix(x) ###

# Preparation
nInd = nrow(EDM)
#First preparatory work on the outcome variable (dissimilarity) and the predictors 
SexDif = AgeDif = RankDif  = EDM # to make predictors a matrix (for now just fill them as EDM, then after we'll change it)
# to fill the matrices:
for (i in 1:nInd) for (j in 1:nInd) { #so for every i individual and j individual
  AgeDif[i,j]=MJD$Age[i]-MJD$Age[j] 
  SexDif[i,j]=SexDif[j,i]=1 
  if (MJD$sex[i]==MJD$sex[j]) { SexDif[i,j]=SexDif[j,i]=0 }
  RankDif[i,j]=RankDif[j,i]=0
  if (MJD$Rank2[i]>MJD$Rank2[j]) {RankDif[i,j]=1}
  if (MJD$Rank2[i]<MJD$Rank2[j]) {RankDif[i,j]=-1}
  if (MJD$Rank2[j]>MJD$Rank2[i]) {RankDif[j,i]=1}
  if (MJD$Rank2[j]<MJD$Rank2[i]) {RankDif[j,i]=-1}
}
AgeDif # effect younger older
SexDif # only looks at male/female effect, so 0 is same-sex dyad, 1 is mixed-sex dyad
RankDif # looks at difference higher and lower rank 

# Matrilineal kinship matrix
KinM <- read.csv("Matrilineal kinship matrix.csv", sep = ",", row.names = 1)
KinM <- as.matrix(KinM)

# centering of matrices
AgeDif.rwcentered = getRwCenteredMatrix(AgeDif)
EDM.rwcentered <- getRwCenteredMatrix(EDM)

#### MQRAP-DSP #####
matrixr2 <- asnipe::mrqap.dsp(EDM.rwcentered ~ AgeDif.rwcentered + SexDif + RankDif + KinM, intercept = FALSE, diagonal = FALSE, directed = "directed", randomisations = 10000)
matrixr2 

# look at residuals for assumptions model
plot(residuals(matrixr2)) 
hist(residuals(matrixr2))
qplot(sample = residuals(matrixr2), stat = "qq")

model_output <- data.frame(fitted = matrixr2$fitted.values, residuals = matrixr2$residuals)
ggplot(model_output, aes(fitted, residuals)) + geom_point() + geom_smooth(method = "lm", color = "blue")

##### COPROPHAGY AND STRESS #####
# group correlation
cor.test(MJD$coprophagyR, MJD$self_scratchR, method = "spearman")
# per individual co-occurrence
# fix the dates
FocalsClean$date <- as.Date(FocalsClean$date, format = "%Y-%m-%d")

FocalsClean$ID_index <- as.integer(as.factor(FocalsClean$focalID))

# transform data frame to ftable of behavior per individual per date
for (i in 1:max(FocalsClean$ID_index)) {
  temp <- subset(FocalsClean, FocalsClean$ID_index == i) 
  temp <- as.data.frame(as.matrix(ftable(temp$date, temp$behavior)), rownames(1, prefix = "Date"))
  temp <- tibble::rownames_to_column(temp, "Date")
# temp$length <- aggfocal$date[aggfocal$ID_index == i] # to call from other objects
  temp$ID_index <- i
  ifelse(i == 1, DataTP <- temp, DataTP <- rbind(DataTP, temp))
}

corrByGroupCO <- function(DataTP){
  return(data.frame(cbind(correl = round(rcorr(DataTP$coprophagy, DataTP$self_scratch, type = "spearman")$r[1,2], digits=10),
                          n = rcorr(DataTP$coprophagy, DataTP$self_scratch, type = "spearman")$n[1,2],
                          pvalue = round(rcorr(DataTP$coprophagy, DataTP$self_scratch, type = "spearman")$P[1,2], digits=10))))
}
CoCor <- ddply(DataTP, .(ID_index), corrByGroupCO)
round(CoCor$pvalue, digits = 3)
round(p.adjust(CoCor$pvalue, method = "BH"),digits = 3)