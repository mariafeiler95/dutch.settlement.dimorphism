library(dplyr)
library(readxl)

## Get data from working directory and cleaning.
SettlementDimorphismRawData <- read_excel("SettlementDimorphismRawData.xlsx")
names(SettlementDimorphismRawData) <- gsub(" ", ".", names(SettlementDimorphismRawData))
names(SettlementDimorphismRawData) <- gsub("-", ".", names(SettlementDimorphismRawData))
SettlementDimorphismRawData$Sex <- as.factor(SettlementDimorphismRawData$Sex)
SettlementDimorphismRawData$Age <- as.factor(SettlementDimorphismRawData$Age)

## Create MB11 (Middenbeemster) and ARJB (Arnhem) data frames.
MB11 <- subset(SettlementDimorphismRawData, SettlementDimorphismRawData$Population == "MB11")
ARJB <- subset(SettlementDimorphismRawData, SettlementDimorphismRawData$Population == "ARJB")

## Making trait list vectors for tables later on.
Bilateral.Trait <- c("Mastoid Process", "Supraorbital Margin", "Ventral Arc", "Subpubic Concavity", "Ischiopubic Ramus", "Greater Sciatic Notch")
Trait <- c("Nuchal Crest", "Mastoid Process", "Supraorbital Margin", "Glabella", "Mental Eminence", "Ventral Arc", "Subpubic Concavity", "Ischiopubic Ramus", "Greater Sciatic Notch")
Cranial.Trait <-  c("Nuchal Crest", "Mastoid Process", "Supraorbital Margin", "Glabella", "Mental Eminence")
Pelvic.Trait <-  c("Ventral Arc", "Subpubic Concavity", "Ischiopubic Ramus", "Greater Sciatic Notch")

## Summarizing the data.


## MB11 Asymmetry and Age Differences
##Conducting t-tests for asymmetry.
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
##Conducting t-tests for asymmetry.
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
