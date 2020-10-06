#Roading necessary packages-------------------------------------------------------------------------------------------------
library(readxl)
library(broom)
library(purrr)
library(ggplot2)

#Road in data from working directory----------------------------------------------------------------------------------------
SettlementDimorphismRawData <- read_excel("SettlementDimorphismRawData.xlsx", 
                                          col_types = c("text", "text", "text", "text", "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                        "skip"))

# Clean data and headings for analysis--------------------------------------------------------------------------------------
SettlementDimorphismRawData$Population <- as.factor(SettlementDimorphismRawData$Population)
SettlementDimorphismRawData$Sex <- as.factor(SettlementDimorphismRawData$Sex)
SettlementDimorphismRawData$Age <- as.factor(SettlementDimorphismRawData$Age)

colnames(SettlementDimorphismRawData) <- gsub(" ", ".", colnames(SettlementDimorphismRawData))
names(SettlementDimorphismRawData)[names(SettlementDimorphismRawData) == "L.Supra-Orbital.Margin"] <- "L.Supraorbital.Margin"
names(SettlementDimorphismRawData)[names(SettlementDimorphismRawData) == "L.Supra-Orbital.Margin"] <- "L.Supraorbital.Margin"
names(SettlementDimorphismRawData)[names(SettlementDimorphismRawData) == "R.Supra-Orbital.Margin"] <- "R.Supraorbital.Margin"
names(SettlementDimorphismRawData)[names(SettlementDimorphismRawData) == "R.Supra-Orbital.Margin"] <- "R.Supraorbital.Margin"

# Determine asymetry--------------------------------------------------------------------------------------------------------- 
MASttest <- t.test(SettlementDimorphismRawData$R.Mastoid, SettlementDimorphismRawData$R.Mastoid, 
                   var.equal = TRUE)
SOMttest <- t.test(SettlementDimorphismRawData$R.Supraorbital.Margin, SettlementDimorphismRawData$R.Supraorbital.Margin, 
                   var.equal = TRUE)
VAttest <- t.test(SettlementDimorphismRawData$R.Ventral.Arc, SettlementDimorphismRawData$R.Ventral.Arc, 
                  var.equal = TRUE)
SPCttest <- t.test(SettlementDimorphismRawData$R.Subpubic.Concavity, SettlementDimorphismRawData$R.Subpubic.Concavity, 
                   var.equal = TRUE)
IPRttest <- t.test(SettlementDimorphismRawData$R.Ischiopubic.Ramus, SettlementDimorphismRawData$R.Ischiopubic.Ramus, 
                   var.equal = TRUE)
GSNttest <- t.test(SettlementDimorphismRawData$R.Sciatic.Notch, SettlementDimorphismRawData$R.Sciatic.Notch, 
                   var.equal = TRUE)

ttests <- c("MASttest", "SOMttest", "VAttest", "SPCttest", "IPRttest", "GSNttest")
asymttests <- map_df(list(MASttest, SOMttest, VAttest, SPCttest, IPRttest, GSNttest), tidy)
asymttests <- cbind(ttests, asymttests)
asymttests <- as.data.frame(asymttests[,c("ttests", "statistic", "p.value", "conf.low", "conf.high")])
print(asymttests)

#Calculating pelvic scores for each individual------------------------------------------------------------------------------
#Left Side 
L.Pubic.Score <- apply(X = SettlementDimorphismRawData[,c("L.Ventral.Arc", "L.Ischiopubic.Ramus", "L.Subpubic.Concavity")],
                       MARGIN = 1, 
                       FUN = function(x) {2.726*x[1] + 1.214*x[2] + 1.073*x[3] - 16.312}
                       )
SettlementDimorphismRawData <- cbind(SettlementDimorphismRawData, L.Pubic.Score)

L.Pubic.pFemale <- apply(X = SettlementDimorphismRawData[,c("L.Pubic.Score"), drop = F], 
                         MARGIN = 1, 
                         FUN = function(x) {1/(1 + exp(x[1]))} 
                         )
SettlementDimorphismRawData <- cbind(SettlementDimorphismRawData, L.Pubic.pFemale)

SettlementDimorphismRawData$L.Pubic.Sex.Est <- ifelse(SettlementDimorphismRawData$L.Pubic.pFemale > 0.5, 1, 2)

#Right Side
R.Pubic.Score <- apply(X = SettlementDimorphismRawData[,c("R.Ventral.Arc", "R.Ischiopubic.Ramus", "R.Subpubic.Concavity")],
                       MARGIN = 1, 
                       FUN = function(x) {2.726*x[1] + 1.214*x[2] + 1.073*x[3] - 16.312}
)
SettlementDimorphismRawData <- cbind(SettlementDimorphismRawData, R.Pubic.Score)

R.Pubic.pFemale <- apply(X = SettlementDimorphismRawData[,c("R.Pubic.Score"), drop = F], 
                         MARGIN = 1, 
                         FUN = function(x) {1/(1 + exp(x[1]))} 
)
SettlementDimorphismRawData <- cbind(SettlementDimorphismRawData, R.Pubic.pFemale)

SettlementDimorphismRawData$R.Pubic.Sex.Est <- ifelse(SettlementDimorphismRawData$R.Pubic.pFemale > 0.5, 1, 2)

#Calculating cranial scores for each individual-----------------------------------------------------------------------------
#Left Side
L.Cranial.Score <- apply(X = SettlementDimorphismRawData[,c("Nuchal.Crest", "L.Mastoid", "L.Supraorbital.Margin", "Glabella", "Mental.Eminence")], 
                         MARGIN = 1, 
                         FUN = function(x = NA) {GlaMasMen <- x[4]*(-1.375) + x[2]*(-1.185) + x[5]*(-1.15) + 9.128
                                                 GlaMas <- x[4]*(-1.558) + x[2]*(-1.459) + 7.434
                                                 GlaMen <- x[4]*(-1.525) + x[5]*(-1.485) + 7.372
                                                 MenMas <- x[5]*(-1.415) + x[2]*(-1.629) + 7.382
                                                 SomMen <- x[3]*(-1.007) + x[5]*(-1.85) + 6.018
                                                 NucMen <- x[1]*(-0.7) + x[5]*(-1.559) + 5.329
                                                 if (!is.na(GlaMasMen)) {return(GlaMasMen)}
                                                 else if (!is.na(GlaMas)) {return(GlaMas)}
                                                 else if (!is.na(GlaMen)) {return(GlaMen)}
                                                 else if (!is.na(MenMas)) {return(MenMas)}
                                                 else if (!is.na(SomMen)) {return(SomMen)}
                                                 else if (!is.na(NucMen)) {return(NucMen)}
                                                 else return("NA")
                                                 }
                         )
SettlementDimorphismRawData <- cbind(SettlementDimorphismRawData, L.Cranial.Score)
SettlementDimorphismRawData$L.Cranial.Score <- as.numeric(SettlementDimorphismRawData$L.Cranial.Score)

L.Cranial.pFemale <- apply(X = SettlementDimorphismRawData[,c("L.Cranial.Score"), drop = F], 
                           MARGIN = 1, 
                           FUN = function(x) {1/(1 + exp(-(x[1])))} 
                           )
SettlementDimorphismRawData <- cbind(SettlementDimorphismRawData, L.Cranial.pFemale)

SettlementDimorphismRawData$L.Cranial.Sex.Est <- ifelse(SettlementDimorphismRawData$L.Cranial.pFemale > 0.5, 1, 2)

#Right Side
R.Cranial.Score <- apply(X = SettlementDimorphismRawData[,c("Nuchal.Crest", "R.Mastoid", "R.Supraorbital.Margin", "Glabella", "Mental.Eminence")], 
                         MARGIN = 1, 
                         FUN = function(x = NA) {GlaMasMen <- x[4]*(-1.375) + x[2]*(-1.185) + x[5]*(-1.15) + 9.128
                         GlaMas <- x[4]*(-1.558) + x[2]*(-1.459) + 7.434
                         GlaMen <- x[4]*(-1.525) + x[5]*(-1.485) + 7.372
                         MenMas <- x[5]*(-1.415) + x[2]*(-1.629) + 7.382
                         SomMen <- x[3]*(-1.007) + x[5]*(-1.85) + 6.018
                         NucMen <- x[1]*(-0.7) + x[5]*(-1.559) + 5.329
                         if (!is.na(GlaMasMen)) {return(GlaMasMen)}
                         else if (!is.na(GlaMas)) {return(GlaMas)}
                         else if (!is.na(GlaMen)) {return(GlaMen)}
                         else if (!is.na(MenMas)) {return(MenMas)}
                         else if (!is.na(SomMen)) {return(SomMen)}
                         else if (!is.na(NucMen)) {return(NucMen)}
                         else return("NA")
                         }
)
SettlementDimorphismRawData <- cbind(SettlementDimorphismRawData, R.Cranial.Score)
SettlementDimorphismRawData$R.Cranial.Score <- as.numeric(SettlementDimorphismRawData$R.Cranial.Score)

R.Cranial.pFemale <- apply(X = SettlementDimorphismRawData[,c("R.Cranial.Score"), drop = F], 
                           MARGIN = 1, 
                           FUN = function(x) {1/(1 + exp(-(x[1])))} 
)
SettlementDimorphismRawData <- cbind(SettlementDimorphismRawData, R.Cranial.pFemale)

SettlementDimorphismRawData$R.Cranial.Sex.Est <- ifelse(SettlementDimorphismRawData$R.Cranial.pFemale > 0.5, 1, 2)

#Determining greater sciatic notch sex estimation---------------------------------------------------------------------------
#Left Side
L.GSN.Sex.Est <- apply(X = SettlementDimorphismRawData[,c("L.Sciatic.Notch"), drop = F], 
                       MARGIN = 1, 
                       FUN = function(x) {if (is.na(x[1])) {return("NA")} 
                               else if (x[1] >= 4) {return("2")} 
                               else if (x[1] <= 2) {return("1")} 
                               else {return("1.5")}})
SettlementDimorphismRawData <- cbind(SettlementDimorphismRawData, L.GSN.Sex.Est)
SettlementDimorphismRawData$L.GSN.Sex.Est <- as.numeric(SettlementDimorphismRawData$L.GSN.Sex.Est)

#Right Side
R.GSN.Sex.Est <- apply(X = SettlementDimorphismRawData[,c("R.Sciatic.Notch"), drop = F], 
                       MARGIN = 1, 
                       FUN = function(x) {if (is.na(x[1])) {return("NA")} 
                               else if (x[1] >= 4) {return("2")} 
                               else if (x[1] <= 2) {return("1")} 
                               else {return("1.5")}})
SettlementDimorphismRawData <- cbind(SettlementDimorphismRawData, R.GSN.Sex.Est)
SettlementDimorphismRawData$R.GSN.Sex.Est <- as.numeric(SettlementDimorphismRawData$R.GSN.Sex.Est)

#Final sex estimation for each individual-----------------------------------------------------------------------------------
sexest <- SettlementDimorphismRawData[,c("L.Pubic.Sex.Est", "R.Pubic.Sex.Est", "L.Cranial.Sex.Est", "R.Cranial.Sex.Est", "L.GSN.Sex.Est", "R.GSN.Sex.Est")]
sexest <- transform(sexest, Sex.Est = rowMeans(sexest[,-1], na.rm = TRUE))
sexest <- as.data.frame(sexest)

SettlementDimorphismRawData <- cbind(SettlementDimorphismRawData, sexest$Sex.Est)
names(SettlementDimorphismRawData)[names(SettlementDimorphismRawData) == "sexest$Sex.Est"] <- "Sex.Est"

SettlementDimorphismRawData$Sex.Est <- apply(X = SettlementDimorphismRawData[,c("Sex.Est"), drop = F], 
                                             MARGIN = 1, 
                                             FUN = function(x) {if (x[1] < 1.5) {return("Female")} 
                                                     else if (x[1] > 1.5) {return("Male")}
                                                     else if (x[1] == 1.5) {return("Indeterminate")}
                                                     else {return("NA")}})

