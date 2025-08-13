setwd("U:\\CIC_ACQ\\_BIOSTAT\\0.Biométrie\\Etudes\\2021\\Protocolées\\Plantier_AEROPERC\\DM\\Revue de données\\Bases\\R")

source("packages.R")

#______________________________________________________#
#Import de toutes les tables----
AE <- read.xlsx("..\\Bases\\AE_20250625.xlsx")
CEXCLUSION <- read.xlsx("..\\Bases\\CEXCLUSION_20250625.xlsx")
ID <- read.xlsx("..\\Bases\\ID_20250625.xlsx")
IE <- read.xlsx("..\\Bases\\IE_20250625.xlsx")
RD <- read.xlsx("..\\Bases\\RD_20250625.xlsx")
V1 <- read.xlsx("..\\Bases\\V1_20250625.xlsx")
V2 <- read.xlsx("..\\Bases\\V2_20250625.xlsx")
V3 <- read.xlsx("..\\Bases\\V3_20250625.xlsx")
V4 <- read.xlsx("..\\Bases\\V4_20250625.xlsx")
V5 <- read.xlsx("..\\Bases\\V5_20250625.xlsx")
V6 <- read.xlsx("..\\Bases\\V6_20250625.xlsx")
VPIP <- read.xlsx("..\\Bases\\VPIP_20250625.xlsx")
activationDynamique <- read.xlsx("..\\Bases\\AEROPERC_Activation_Dynamique.xlsx")
listeQueries <- read.xlsx("..\\Bases\\AEROPERC_listeQueries.xlsx")
listeCommentaires <- read.xlsx("..\\Bases\\AEROPERC_listeCommentaires.xlsx")
#______________________________________________________#----


#______________________________________________________#
#Creation d'un data frame avec toutes mes tables----
dataframes <-
  c("AE",
    "CEXCLUSION",
    "ID",
    "IE",
    "RD",
    "V1",
    "V2",
    "V3",
    "V4",
    "V5",
    "V6",
    "VPIP")
#______________________________________________________#----

source("RDD.R")
