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
                                                        "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

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

# Calculating pubic sex estimation for each individual---------------------------------------------------------------------
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
# Cleaning
sexest$L.Pubic.Sex.Est <- as.numeric(as.character(sexest$L.Pubic.Sex.Est))
sexest$R.Pubic.Sex.Est <- as.numeric(as.character(sexest$R.Pubic.Sex.Est))
sexest$L.Cranial.Sex.Est <- as.numeric(as.character(sexest$L.Cranial.Sex.Est))
sexest$R.Cranial.Sex.Est <- as.numeric(as.character(sexest$R.Cranial.Sex.Est))
sexest$L.GSN.Sex.Est <- as.numeric(as.character(sexest$L.GSN.Sex.Est))
sexest$R.GSN.Sex.Est <- as.numeric(as.character(sexest$R.GSN.Sex.Est))

sexest <- transform(sexest, Sex.Est = rowMeans(sexest, na.rm = TRUE))
sexest <- as.data.frame(sexest)

SettlementDimorphismRawData <- cbind(SettlementDimorphismRawData, sexest$Sex.Est)
names(SettlementDimorphismRawData)[names(SettlementDimorphismRawData) == "sexest$Sex.Est"] <- "Sex.Est"

# Averaging sex estimations from previous to get final sex estimation
SettlementDimorphismRawData$Sex.Est <- apply(X = SettlementDimorphismRawData[,c("Sex.Est"), drop = F], 
                                             MARGIN = 1, 
                                             FUN = function(x) {if (x[1] < 1.5) {return("Female")} 
                                                     else if (x[1] > 1.5) {return("Male")}
                                                     else if (x[1] == 1.5) {return("Indeterminate")}
                                                     else {return("NA")}})

# Making sex subsample dataframes
Females <- subset(SettlementDimorphismRawData, SettlementDimorphismRawData$Sex.Est == "Female") 
Males <- subset(SettlementDimorphismRawData, SettlementDimorphismRawData$Sex.Est == "Male") 

# Calculating auricular surface age estimation for each individual----------------------------------------------------------
# Buckland and Chaimberland Method for Auricular Surface
# Left Side
SettlementDimorphismRawData$L.Aur.Sur.Sum <- apply(X = SettlementDimorphismRawData[,c("L.Transverse", "L.Texture", "L.Micro", 
                                                                                      "L.Macro", "L.Apical")],
                                                   MARGIN = 1, 
                                                   FUN = sum)

SettlementDimorphismRawData$L.Aur.Sur.AvgAge <- apply(X = SettlementDimorphismRawData[,c("L.Aur.Sur.Sum"), drop = F],
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


SettlementDimorphismRawData$L.Aur.Sur.SDAge <- apply(X = SettlementDimorphismRawData[,c("L.Aur.Sur.Sum"), drop = F],
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


SettlementDimorphismRawData$R.Aur.Sur.AvgAge <- apply(X = SettlementDimorphismRawData[,c("R.Aur.Sur.Sum"), drop = F],
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

SettlementDimorphismRawData$R.Aur.Sur.SDAge <- apply(X = SettlementDimorphismRawData[,c("R.Aur.Sur.Sum"), drop = F],
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
Males$L.Pub.Sym.AvgAge <- apply(X = Males[,c("L.Pubic.Symphysis"), drop = F],
                                MARGIN = 1,
                                FUN = function(x) {if (x[1] == 1) {return(19.4)}
                                                   else if (x[1] == 2) {return(25.0)}
                                                   else if (x[1] == 3) {return(30.7)}
                                                   else if (x[1] == 4) {return(38.2)}
                                                   else if (x[1] == 5) {return(48.1)}
                                                   else if (x[1] == 6) {return(60)}
                                                   else {return("NA")}
                                                   }
                                )

Males$L.Pub.Sym.SDAge <- apply(X = Males[,c("L.Pubic.Symphysis"), drop = F],
                               MARGIN = 1,
                               FUN = function(x) {if (x[1] == 1) {return(2.6)}
                                                  else if (x[1] == 2) {return(4.9)}
                                                  else if (x[1] == 3) {return(8.1)}
                                                  else if (x[1] == 4) {return(10.9)}
                                                  else if (x[1] == 5) {return(14.6)}
                                                  else if (x[1] == 6) {return(12.6)}
                                                  else {return("NA")}
                                                  }
                               )

Females$L.Pub.Sym.AvgAge <- apply(X = Females[,c("L.Pubic.Symphysis"), drop = F],
                                  MARGIN = 1,
                                  FUN = function(x) {if (x[1] == 1) {return(18.5)}
                                                     else if (x[1] == 2) {return(23.4)}
                                                     else if (x[1] == 3) {return(28.7)}
                                                     else if (x[1] == 4) {return(35.2)}
                                                     else if (x[1] == 5) {return(45.6)}
                                                     else if (x[1] == 6) {return(61.2)}
                                                     else {return("NA")}
                                                     }
                                  )

Females$L.Pub.Sym.SDAge <- apply(X = Females[,c("L.Pubic.Symphysis"), drop = F],
                                 MARGIN = 1,
                                 FUN = function(x) {if (x[1] == 1) {return(2.1)}
                                                    else if (x[1] == 2) {return(3.6)}
                                                    else if (x[1] == 3) {return(6.5)}
                                                    else if (x[1] == 4) {return(9.4)}
                                                    else if (x[1] == 5) {return(10.4)}
                                                    else if (x[1] == 6) {return(12.2)}
                                                    else {return("NA")}
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

# FINAL AGE ESTIMATIONS-----------------------------------------------------------------------------------------------------

# ASSYMETRY TESTS----------------------------------------------------------------------------------------------------------- 
## MB11 Asymmetry and Age Differences
## Conducting t-tests for asymmetry.
MB11MASttest <- t.test(MB11$L.Mastoid, MB11$R.Mastoid, var.equal = TRUE)
MB11SOMttest <- t.test(MB11$L.Supra.Orbital.Margin, MB11$R.Supra.Orbital.Margin, var.equal = TRUE)
MB11VAttest <- t.test(MB11$L.Ventral.Arc, MB11$R.Ventral.Arc, var.equal = TRUE)
MB11SPCttest <- t.test(MB11$L.Subpubic.Concavity, MB11$R.Subpubic.Concavity, var.equal = TRUE)
MB11IPRttest <- t.test(MB11$L.Ischiopubic.Ramus, MB11$R.Ischiopubic.Ramus, var.equal = TRUE)
MB11GSNttest <- t.test(MB11$L.Sciatic.Notch, MB11$R.Sciatic.Notch, var.equal = TRUE)

MB11asymttests <- map_df(list(MB11MASttest, MB11SOMttest, MB11VAttest, MB11SPCttest, MB11IPRttest, MB11GSNttest), tidy)
MB11asymttests <- cbind(Bilateral.Trait, MB11asymttests)
MB11asymttests <- as.data.frame(MB11asymttests[,c("Bilateral.Trait", "statistic", "parameter", "p.value")])
names(MB11asymttests) <- c("Bilateral Trait", "t-value", "df", "p-value")

## Conducting Kruskal Wallis tests for differences between age groups. 
MB11NUCkwtest <- kruskal.test(Nuchal.Crest ~ Age, data = MB11)
MB11LMASkwtest <- kruskal.test(L.Mastoid ~ Age, data = MB11)
MB11SOMkwtest <- kruskal.test(L.Supra.Orbital.Margin ~ Age, data = MB11)
MB11GLAkwtest <- kruskal.test(Glabella ~ Age, data = MB11)
MB11MENkwtest <- kruskal.test(Mental.Eminence ~ Age, data = MB11)
MB11VAkwtest <- kruskal.test(L.Ventral.Arc ~ Age, data = MB11)
MB11SPCkwtest <- kruskal.test(L.Subpubic.Concavity ~ Age, data = MB11)
MB11IPRkwtest <- kruskal.test(L.Ischiopubic.Ramus ~ Age, data = MB11)
MB11GSNkwtest <- kruskal.test(L.Sciatic.Notch ~ Age, data = MB11)

MB11agekwtests <- map_df(list(MB11NUCkwtest, MB11LMASkwtest, MB11SOMkwtest, MB11GLAkwtest, MB11MENkwtest, 
                              MB11VAkwtest, MB11SPCkwtest, MB11IPRkwtest, MB11GSNkwtest), 
                         tidy)
MB11agekwtests <- cbind(Trait, MB11agekwtests)
MB11agekwtests <- as.data.frame(MB11agekwtests[,c("Trait", "statistic", "parameter", "p.value")])
names(MB11agekwtests) <- c("Trait", "Kruskal-Wallis H", "df", "p-value")

## ARJB Asymmetry and Age Differences
## Conducting t-tests for asymmetry.
ARJBMASttest <- t.test(ARJB$L.Mastoid, ARJB$R.Mastoid, var.equal = TRUE)
ARJBSOMttest <- t.test(ARJB$L.Supra.Orbital.Margin, ARJB$R.Supra.Orbital.Margin, var.equal = TRUE)
ARJBVAttest <- t.test(ARJB$L.Ventral.Arc, ARJB$R.Ventral.Arc, var.equal = TRUE)
ARJBSPCttest <- t.test(ARJB$L.Subpubic.Concavity, ARJB$R.Subpubic.Concavity, var.equal = TRUE)
ARJBIPRttest <- t.test(ARJB$L.Ischiopubic.Ramus, ARJB$R.Ischiopubic.Ramus, var.equal = TRUE)
ARJBGSNttest <- t.test(ARJB$L.Sciatic.Notch, ARJB$R.Sciatic.Notch, var.equal = TRUE)

ARJBasymttests <- map_df(list(ARJBMASttest, ARJBSOMttest, ARJBVAttest, ARJBSPCttest, ARJBIPRttest, ARJBGSNttest), tidy)
ARJBasymttests <- cbind(Bilateral.Trait, ARJBasymttests)
ARJBasymttests <- as.data.frame(ARJBasymttests[,c("Bilateral.Trait", "statistic", "parameter", "p.value")])
names(ARJBasymttests) <- c("Bilateral Trait", "t-value", "df", "p-value")

## Conducting Kruskal Wallis tests for differences between age groups. 
ARJBNUCkwtest <- kruskal.test(Nuchal.Crest ~ Age, data = ARJB)
ARJBLMASkwtest <- kruskal.test(L.Mastoid ~ Age, data = ARJB)
ARJBSOMkwtest <- kruskal.test(L.Supra.Orbital.Margin ~ Age, data = ARJB)
ARJBGLAkwtest <- kruskal.test(Glabella ~ Age, data = ARJB)
ARJBMENkwtest <- kruskal.test(Mental.Eminence ~ Age, data = ARJB)
ARJBVAkwtest <- kruskal.test(L.Ventral.Arc ~ Age, data = ARJB)
ARJBSPCkwtest <- kruskal.test(L.Subpubic.Concavity ~ Age, data = ARJB)
ARJBIPRkwtest <- kruskal.test(L.Ischiopubic.Ramus ~ Age, data = ARJB)
ARJBGSNkwtest <- kruskal.test(L.Sciatic.Notch ~ Age, data = ARJB)

ARJBagekwtests <- map_df(list(ARJBNUCkwtest, ARJBLMASkwtest, ARJBSOMkwtest, ARJBGLAkwtest, ARJBMENkwtest, 
                              ARJBVAkwtest, ARJBSPCkwtest, ARJBIPRkwtest, ARJBGSNkwtest), 
                         tidy)
ARJBagekwtests <- cbind(Trait, ARJBagekwtests)
ARJBagekwtests <- as.data.frame(ARJBagekwtests[,c("Trait", "statistic", "parameter", "p.value")])
names(ARJBagekwtests) <- c("Trait", "Kruskal-Wallis H", "df", "p-value")
