# Loading necessary packages-------------------------------------------------------------------------------------------------
library(readxl)
library(broom)
library(purrr)
library(ggplot2)

# Load in data from working directory----------------------------------------------------------------------------------------
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

# Creating subsample dataframes----------------------------------------------------------------------------------------------
MB11 <- subset(SettlementDimorphismRawData, SettlementDimorphismRawData$Population == "MB11")
ARJB <- subset(SettlementDimorphismRawData, SettlementDimorphismRawData$Population == "ARJB")

# Creating trait property vectors for future calculation ease---------------------------------------------------------------
Bilateral.Trait <- c("Mastoid Process", "Supraorbital Margin", "Ventral Arc", "Subpubic Concavity", "Ischiopubic Ramus", 
                     "Greater Sciatic Notch")
Trait <- c("Nuchal Crest", "Mastoid Process", "Supraorbital Margin", "Glabella", "Mental Eminence", "Ventral Arc", 
           "Subpubic Concavity", "Ischiopubic Ramus", "Greater Sciatic Notch")
Cranial.Trait <-  c("Nuchal Crest", "Mastoid Process", "Supraorbital Margin", "Glabella", "Mental Eminence")
Pelvic.Trait <-  c("Ventral Arc", "Subpubic Concavity", "Ischiopubic Ramus", "Greater Sciatic Notch")

# Calculating pelvic sex estimation for each individual---------------------------------------------------------------------
# Left Side 
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

# Right Side
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

# Calculating cranial sex estimation for each individual--------------------------------------------------------------------
# Left Side
L.Cranial.Score <- apply(X = SettlementDimorphismRawData[,c("Nuchal.Crest", "L.Mastoid", "L.Supraorbital.Margin", "Glabella", 
                                                            "Mental.Eminence")], 
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

# Right Side
R.Cranial.Score <- apply(X = SettlementDimorphismRawData[,c("Nuchal.Crest", "R.Mastoid", "R.Supraorbital.Margin", "Glabella", 
                                                            "Mental.Eminence")], 
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

# Determining greater sciatic notch sex estimation---------------------------------------------------------------------------
# Left Side
L.GSN.Sex.Est <- apply(X = SettlementDimorphismRawData[,c("L.Sciatic.Notch"), drop = F], 
                       MARGIN = 1, 
                       FUN = function(x) {if (is.na(x[1])) {return("NA")} 
                               else if (x[1] >= 4) {return("2")} 
                               else if (x[1] <= 2) {return("1")} 
                               else {return("1.5")}})
SettlementDimorphismRawData <- cbind(SettlementDimorphismRawData, L.GSN.Sex.Est)
SettlementDimorphismRawData$L.GSN.Sex.Est <- as.numeric(SettlementDimorphismRawData$L.GSN.Sex.Est)

# Right Side
R.GSN.Sex.Est <- apply(X = SettlementDimorphismRawData[,c("R.Sciatic.Notch"), drop = F], 
                       MARGIN = 1, 
                       FUN = function(x) {if (is.na(x[1])) {return("NA")} 
                               else if (x[1] >= 4) {return("2")} 
                               else if (x[1] <= 2) {return("1")} 
                               else {return("1.5")}})
SettlementDimorphismRawData <- cbind(SettlementDimorphismRawData, R.GSN.Sex.Est)
SettlementDimorphismRawData$R.GSN.Sex.Est <- as.numeric(SettlementDimorphismRawData$R.GSN.Sex.Est)

# FINAL SEX ESTIMATION for each individual----------------------------------------------------------------------------------
sexest <- SettlementDimorphismRawData[,c("L.Pubic.Sex.Est", "R.Pubic.Sex.Est", "L.Cranial.Sex.Est", "R.Cranial.Sex.Est", 
                                         "L.GSN.Sex.Est", "R.GSN.Sex.Est")]
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

# Calculating auricular surface age estimation for each individual----------------------------------------------------------
# Left Side
SettlementDimorphismRawData$L.Aur.Sur.Sum <- apply(X = SettlementDimorphismRawData[,c("L.Transverse", "L.Texture", "L.Micro", 
                                                                                      "L.Macro", "L.Apical")],
                                                   MARGIN = 1, 
                                                   FUN = sum)

SettlementDimorphismRawData$L.Aur.Sur.AvgAge <- apply(X = SettlementDimorphismRawData[,c("L.Aur.Sur.Sum")],
                                                      MARGIN = 1,
                                                      FUN = function(x) {if (x[1] == 5 | 6) {return(17.33)}
                                                              else if (x[1] == 7 | 8) {return(29.33)}
                                                              else if (x[1] == 9 | 10) {return(37.86)}
                                                              else if (x[1] == 11 | 12) {return(51.41)}
                                                              else if (x[1] == 13 | 14) {return(59.94)}
                                                              else if (x[1] == 15 | 16) {return(66.71)}
                                                              else if (x[1] >= 17) {return(72.25)}
                                                              else {return(NA)}
                                                              }
                                                      )

SettlementDimorphismRawData$L.Aur.Sur.SDAge <- apply(X = SettlementDimorphismRawData[,c("L.Aur.Sur.Sum")],
                                                      MARGIN = 1,
                                                      FUN = function(x) {if (x[1] == 5 | 6) {return(1.53)}
                                                              else if (x[1] == 7 | 8) {return(6.71)}
                                                              else if (x[1] == 9 | 10) {return(13.06)}
                                                              else if (x[1] == 11 | 12) {return(14.47)}
                                                              else if (x[1] == 13 | 14) {return(12.95)}
                                                              else if (x[1] == 15 | 16) {return(11.88)}
                                                              else if (x[1] >= 17) {return(12.73)}
                                                              else {return(NA)}                                                      
                                                              }
                                                     )

# Right Side
SettlementDimorphismRawData$R.Aur.Sur.Sum <- apply(X = SettlementDimorphismRawData[,c("R.Transverse", "R.Texture", "R.Micro", 
                                                                                      "R.Macro", "R.Apical")],
                                                   MARGIN = 1, 
                                                   FUN = sum)

SettlementDimorphismRawData$R.Aur.Sur.AvgAge <- apply(X = SettlementDimorphismRawData[,c("R.Aur.Sur.Sum")],
                                                      MARGIN = 1,
                                                      FUN = function(x) {if (x[1] == 5 | 6) {return(17.33)}
                                                              else if (x[1] == 7 | 8) {return(29.33)}
                                                              else if (x[1] == 9 | 10) {return(37.86)}
                                                              else if (x[1] == 11 | 12) {return(51.41)}
                                                              else if (x[1] == 13 | 14) {return(59.94)}
                                                              else if (x[1] == 15 | 16) {return(66.71)}
                                                              else if (x[1] >= 17) {return(72.25)}
                                                              else {return(NA)}                                                      
                                                              }
                                                      )

SettlementDimorphismRawData$R.Aur.Sur.SDAge <- apply(X = SettlementDimorphismRawData[,c("R.Aur.Sur.Sum")],
                                                     MARGIN = 1,
                                                     FUN = function(x) {if (x[1] == 5 | 6) {return(1.53)}
                                                             else if (x[1] == 7 | 8) {return(6.71)}
                                                             else if (x[1] == 9 | 10) {return(13.06)}
                                                             else if (x[1] == 11 | 12) {return(14.47)}
                                                             else if (x[1] == 13 | 14) {return(12.95)}
                                                             else if (x[1] == 15 | 16) {return(11.88)}
                                                             else if (x[1] >= 17) {return(12.73)}
                                                             else {return(NA)}       
                                                             }
                                                     )

# Calculating pubic age estimation for each individual----------------------------------------------------------------------
# Left Side
SettlementDimorphismRawData$L.Pub.Sym.AvgAge <- apply(X = SettlementDimorphismRawData[,c("Sex.Est", "L.Pubic.Symphysis")],
                                                      MARGIN = 1,
                                                      FUN = function(x) {if (x[1] == "Male" & x[2] == 1) {return(19.4)}
                                                              else if (x[1] == "Male" & x[2] == 2) {return(25.0)}
                                                              else if (x[1] == "Male" & x[2] == 3) {return(30.7)}
                                                              else if (x[1] == "Male" & x[2] == 4) {return(38.2)}
                                                              else if (x[1] == "Male" & x[2] == 5) {return(48.1)}
                                                              else if (x[1] == "Male" & x[2] == 6) {return(60)}
                                                              else if (x[1] == "Female" & x[2] == 1) {return(18.5)}
                                                              else if (x[1] == "Female" & x[2] == 2) {return(23.4)}
                                                              else if (x[1] == "Female" & x[2] == 3) {return(28.7)}
                                                              else if (x[1] == "Female" & x[2] == 4) {return(35.2)}
                                                              else if (x[1] == "Female" & x[2] == 5) {return(45.6)}
                                                              else if (x[1] == "Female" & x[2] == 6) {return(61.2)}
                                                              else {return(NA)}
                                                              }
                                                      )

SettlementDimorphismRawData$L.Pub.Sym.SDAge <- apply(X = SettlementDimorphismRawData[,c("Sex.Est", "L.Pubic.Symphysis")],
                                                      MARGIN = 1,
                                                      FUN = function(x) {if (x[1] == "Male" & x[2] == 1) {return(2.6)}
                                                              else if (x[1] == "Male" & x[2] == 2) {return(4.9)}
                                                              else if (x[1] == "Male" & x[2] == 3) {return(8.1)}
                                                              else if (x[1] == "Male" & x[2] == 4) {return(10.9)}
                                                              else if (x[1] == "Male" & x[2] == 5) {return(14.6)}
                                                              else if (x[1] == "Male" & x[2] == 6) {return(12.6)}
                                                              else if (x[1] == "Female" & x[2] == 1) {return(2.1)}
                                                              else if (x[1] == "Female" & x[2] == 2) {return(3.6)}
                                                              else if (x[1] == "Female" & x[2] == 3) {return(6.5)}
                                                              else if (x[1] == "Female" & x[2] == 4) {return(9.4)}
                                                              else if (x[1] == "Female" & x[2] == 5) {return(10.4)}
                                                              else if (x[1] == "Female" & x[2] == 6) {return(12.2)}
                                                              else {return(NA)}
                                                              }
                                                      )

# Right Side
SettlementDimorphismRawData$R.Pub.Sym.AvgAge <- apply(X = SettlementDimorphismRawData[,c("Sex.Est", "R.Pubic.Symphysis")],
                                                      MARGIN = 1,
                                                      FUN = function(x) {if (x[1] == "Male" & x[2] == 1) {return(19.4)}
                                                              else if (x[1] == "Male" & x[2] == 2) {return(25.0)}
                                                              else if (x[1] == "Male" & x[2] == 3) {return(30.7)}
                                                              else if (x[1] == "Male" & x[2] == 4) {return(38.2)}
                                                              else if (x[1] == "Male" & x[2] == 5) {return(48.1)}
                                                              else if (x[1] == "Male" & x[2] == 6) {return(60)}
                                                              else if (x[1] == "Female" & x[2] == 1) {return(18.5)}
                                                              else if (x[1] == "Female" & x[2] == 2) {return(23.4)}
                                                              else if (x[1] == "Female" & x[2] == 3) {return(28.7)}
                                                              else if (x[1] == "Female" & x[2] == 4) {return(35.2)}
                                                              else if (x[1] == "Female" & x[2] == 5) {return(45.6)}
                                                              else if (x[1] == "Female" & x[2] == 6) {return(61.2)}
                                                              else {return(NA)}
                                                              }
                                                      )

SettlementDimorphismRawData$R.Pub.Sym.SDAge <- apply(X = SettlementDimorphismRawData[,c("Sex.Est", "R.Pubic.Symphysis")],
                                                     MARGIN = 1,
                                                     FUN = function(x) {if (x[1] == "Male" & x[2] == 1) {return(2.6)}
                                                             else if (x[1] == "Male" & x[2] == 2) {return(4.9)}
                                                             else if (x[1] == "Male" & x[2] == 3) {return(8.1)}
                                                             else if (x[1] == "Male" & x[2] == 4) {return(10.9)}
                                                             else if (x[1] == "Male" & x[2] == 5) {return(14.6)}
                                                             else if (x[1] == "Male" & x[2] == 6) {return(12.6)}
                                                             else if (x[1] == "Female" & x[2] == 1) {return(2.1)}
                                                             else if (x[1] == "Female" & x[2] == 2) {return(3.6)}
                                                             else if (x[1] == "Female" & x[2] == 3) {return(6.5)}
                                                             else if (x[1] == "Female" & x[2] == 4) {return(9.4)}
                                                             else if (x[1] == "Female" & x[2] == 5) {return(10.4)}
                                                             else if (x[1] == "Female" & x[2] == 6) {return(12.2)}
                                                             else {return(NA)}
                                                             }
                                                     )

# Calculating sternal rib age estimation for each individual----------------------------------------------------------------

# FINAL AGE ESTIMATIONS for each individual--------------------------------------------------------------------------------0

# ASSYMETRY TESTS----------------------------------------------------------------------------------------------------------- 
# Entire sample
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

asymttests <- map_df(list(MASttest, SOMttest, VAttest, SPCttest, IPRttest, GSNttest), tidy)
asymttests <- cbind(Bilateral.Trait, asymttests)
asymttests <- as.data.frame(asymttests[,c("Bilateral.Trait", "statistic", "parameter", "p.value")])
names(asymttests) <- c("Bilateral Trait", "t-value", "df", "p-value")

# MB11
MB11MASttest <- t.test(MB11$L.Mastoid, MB11$R.Mastoid, var.equal = TRUE)
MB11SOMttest <- t.test(MB11$L.Supraorbital.Margin, MB11$R.Supraorbital.Margin, var.equal = TRUE)
MB11VAttest <- t.test(MB11$L.Ventral.Arc, MB11$R.Ventral.Arc, var.equal = TRUE)
MB11SPCttest <- t.test(MB11$L.Subpubic.Concavity, MB11$R.Subpubic.Concavity, var.equal = TRUE)
MB11IPRttest <- t.test(MB11$L.Ischiopubic.Ramus, MB11$R.Ischiopubic.Ramus, var.equal = TRUE)
MB11GSNttest <- t.test(MB11$L.Sciatic.Notch, MB11$R.Sciatic.Notch, var.equal = TRUE)

MB11asymttests <- map_df(list(MB11MASttest, MB11SOMttest, MB11VAttest, MB11SPCttest, MB11IPRttest, MB11GSNttest), tidy)
MB11asymttests <- cbind(Bilateral.Trait, MB11asymttests)
MB11asymttests <- as.data.frame(MB11asymttests[,c("Bilateral.Trait", "statistic", "parameter", "p.value")])
names(MB11asymttests) <- c("Bilateral Trait", "t-value", "df", "p-value")

# ARJB
ARJBMASttest <- t.test(ARJB$L.Mastoid, ARJB$R.Mastoid, var.equal = TRUE)
ARJBSOMttest <- t.test(ARJB$L.Supraorbital.Margin, ARJB$R.Supraorbital.Margin, var.equal = TRUE)
ARJBVAttest <- t.test(ARJB$L.Ventral.Arc, ARJB$R.Ventral.Arc, var.equal = TRUE)
ARJBSPCttest <- t.test(ARJB$L.Subpubic.Concavity, ARJB$R.Subpubic.Concavity, var.equal = TRUE)
ARJBIPRttest <- t.test(ARJB$L.Ischiopubic.Ramus, ARJB$R.Ischiopubic.Ramus, var.equal = TRUE)
ARJBGSNttest <- t.test(ARJB$L.Sciatic.Notch, ARJB$R.Sciatic.Notch, var.equal = TRUE)

ARJBasymttests <- map_df(list(ARJBMASttest, ARJBSOMttest, ARJBVAttest, ARJBSPCttest, ARJBIPRttest, ARJBGSNttest), tidy)
ARJBasymttests <- cbind(Bilateral.Trait, ARJBasymttests)
ARJBasymttests <- as.data.frame(ARJBasymttests[,c("Bilateral.Trait", "statistic", "parameter", "p.value")])
names(ARJBasymttests) <- c("Bilateral Trait", "t-value", "df", "p-value")