#______________________________________________________#
#Ajout du 0 manquant au debut des codes patients INUTILE----

#Boucle pour ajouter le 0
# for (df_name in dataframes) {
#   df <- get(df_name)
#   df$SUBJID <- paste0("0", df$SUBJID)
#   assign(df_name, df)
# }
#______________________________________________________#----


#!!!!!GENARAL A TOUTES LES ETUDES!!!!!----

#______________________________________________________#
#Generalites----
#Nombre d'eCRF crees
NbeCRF <- 0
for (i in 2:length(IE$SUBJID)) {
  NbeCRF <- NbeCRF + 1
}
#Nombre de patients inclus
NbInclus <- 0
for (i in 2:length(RD$SUBJID)) {
  if (RD$CCNBCIAOUI[i] == "4" &&
      RD$CCNBCNIANON[i] == "17" &&
      RD$CCNBCRITEREEXCLUSIONANONP1V1[i] == "2" &&
      RD$CCNBCRITEREEXCLUSIONANONP1V2[i] == "7" &&
      RD$CCNBCRITEREEXCLUSIONANONP2V3[i] == "3") {
    NbInclus <- NbInclus + 1
  }
}
#Nombre de patients non inclus
NbNonInclus <- NbeCRF - NbInclus
#Tableau general
General <-
  data.frame(NbeCRF = NbeCRF,
             NbInclus = NbInclus,
             NbNonInclus = NbNonInclus)
#______________________________________________________#----


#______________________________________________________#
#Doublon de patients ATTENTION PLUS LONG AVEC UN GRAND NOMBRE DE PATIENTS----
IESEXDTBIRTH <-
  merge(IE, V1[, c("SUBJID", "DMSEX", "DMDTBIRTH")], by = "SUBJID", all.x = TRUE)
Doublon <- NULL
for (i in 2:(length(IESEXDTBIRTH$SUBJID) - 1)) {
  for (j in (i + 1):length(IESEXDTBIRTH$SUBJID)) {
    if (!is.na(IESEXDTBIRTH$IENAME[i]) &&
        !is.na(IESEXDTBIRTH$IENAME[j]) &&
        IESEXDTBIRTH$IENAME[i] == IESEXDTBIRTH$IENAME[j]) {
      if (!is.na(IESEXDTBIRTH$IEFIRSTNAME[i]) &&
          !is.na(IESEXDTBIRTH$IEFIRSTNAME[j]) &&
          IESEXDTBIRTH$IEFIRSTNAME[i] == IESEXDTBIRTH$IEFIRSTNAME[j]) {
        if (!is.na(IESEXDTBIRTH$DMDTBIRTH[i]) &&
            !is.na(IESEXDTBIRTH$DMDTBIRTH[j]) &&
            IESEXDTBIRTH$DMDTBIRTH[i] == IESEXDTBIRTH$DMDTBIRTH[j]) {
          if (!is.na(IESEXDTBIRTH$IESEX[i]) &&
              !is.na(IESEXDTBIRTH$IESEX[j]) &&
              IESEXDTBIRTH$IESEX[i] == IESEXDTBIRTH$IESEX[j]) {
            Doublon <-
              rbind(
                Doublon,
                data.frame(
                  SUBJID1 = IESEXDTBIRTH$SUBJID[i],
                  SUBJID2 = IESEXDTBIRTH$SUBJID[j]
                )
              )
          }
        }
      }
    }
  }
}
#______________________________________________________#----


#______________________________________________________#
#Verification de la randomisation----
#Patients non randomises
NonRandomises <- RD[1,]
#Verification si bras de rando ou date de rando vide
for (i in 2:length(RD$SUBJID)) {
  if (is.na(RD$RANDOGROUP[i]) ||
      is.na(RD$VSDTCRANDO[i])) {
    NonRandomises <- rbind(NonRandomises, RD[i,])
  }
}
NonRandomisesNonJustification <- data.frame()
#Pour permettre de remettre les lignes dans le bon ordre
NonRandomises$index <- 1:nrow(NonRandomises)
if (length(NonRandomises) > 0) {
  #Patients non randomises avec une justification sur la page d'IDRD
  NonRandomisesJustification <- merge(NonRandomises, ID, by = "SUBJID")
  #Remettre les lignes dans le bon ordre
  NonRandomisesJustification <- NonRandomisesJustification[order(NonRandomisesJustification$index),]
  NonRandomisesJustification$index <- NULL
  #Patients non randomises sans aucune justification dans la base
  for (i in 2:length(NonRandomises$SUBJID)) {
    nb <- 0
    for (j in 1:length(NonRandomisesJustification$SUBJID)) {
      if (NonRandomises$SUBJID[i] == NonRandomisesJustification$SUBJID[j]) {
        nb <- nb + 1
      }
    }
    if (nb == 0) {
      NonRandomisesNonJustification <- NonRandomises$SUBJID[i]
    } else {NonRandomisesNonJustification <- NULL}
  }
  if (length(NonRandomisesNonJustification) > 0) {
    NonRandomisesNonJustification <- as.data.frame(NonRandomisesNonJustification)
    colnames(NonRandomisesNonJustification) <- c("SUBJID")
  }
} else{NonRandomisesJustification <- NULL}

if(!is.null(NonRandomisesJustification)){
  NonRandomisesJustification <- t(NonRandomisesJustification)
}

if(!is.null(NonRandomisesNonJustification)){
  NonRandomisesNonJustification <- t(NonRandomisesNonJustification)
}
#______________________________________________________#----


#______________________________________________________#
#Patient avec une IDRD a "OUI"----
IDRDaOUI <- data.frame(SUBJID = ID$SUBJID[1])
#Pour permettre de remettre les lignes dans le bon ordre
ID$index <- 1:nrow(ID)
for (i in 2:length(ID$SUBJID)) {
  if ((!is.na(ID$IDCONSENT[i]) &&
       ID$IDCONSENT[i] == "OUI") ||
      (!is.na(ID$IDLOST[i]) &&
       ID$IDLOST[i] == "OUI") ||
      (!is.na(ID$IDDEATH[i]) &&
       ID$IDDEATH[i] == "OUI") ||
      (!is.na(ID$IDDMED[i]) && ID$IDDMED[i] == "OUI")) {
    IDRDaOUI <- rbind(IDRDaOUI, data.frame(SUBJID = ID$SUBJID[i]))
  }
}
if (nrow(IDRDaOUI) > 0) {
  IDRDaOUI <- merge(IDRDaOUI, ID, by = "SUBJID")
  #Remettre les lignes dans le bon ordre
  IDRDaOUI <- IDRDaOUI[order(IDRDaOUI$index),]
  IDRDaOUI$index <- NULL
}
IDRDaOUI <- t(IDRDaOUI)
#______________________________________________________#----


#______________________________________________________#
#Verification des criteres d'inclusion et de non inclusion----
#CRITERES D'INCLUSION
CIaNON <- data.frame()
CIaNON <- rbind(CIaNON, IE[IE$SUBJID == "Subject Identifier for the Study", ])
for (i in 2:length(IE$SUBJID)) {
  if (is.na(IE$IESTRESC1[i]) ||
      IE$IESTRESC1[i] == "NON" ||
      is.na(IE$IESTRESC2[i]) ||
      IE$IESTRESC2[i] == "NON" ||
      is.na(IE$IESTRESC3[i]) |
      IE$IESTRESC3[i] == "NON" ||
      is.na(IE$IESTRESC4[i]) ||
      IE$IESTRESC4[i] == "NON") {
    CIaNON <- rbind(CIaNON, IE[i, ])
  }
}
if (length(CIaNON) > 0) {
  CIaNON <-
    CIaNON[,!(
      names(CIaNON) %in% c(
        "IENAME",
        "IEFIRSTNAME",
        "IEDTCONSENT",
        "IESTRESC5",
        "IESTRESC6",
        "IESTRESC7",
        "IESTRESC8",
        "IESTRESC9",
        "IESTRESC10",
        "IESTRESC11",
        "IESTRESC12",
        "IESTRESC13",
        "IESTRESC14",
        "IESTRESC15",
        "IESTRESC16",
        "IESTRESC17",
        "IESTRESC18",
        "IESTRESC19",
        "IESTRESC20",
        "IESTRESC21",
        "IEEXCL",
        "IEINT",
        "IEINT2",
        "IEEXCL1",
        "IEEXCL2",
        "IEEXCL3",
        "IEEXCL4",
        "IEEXCL5",
        "IEEXCL6",
        "IE1",
        "IE2",
        "IE3"
      )
    )]
}

if(length(CIaNON$SUBJID) == 1){
  CIaNON <- NULL
} else {CIaNON <- t(CIaNON)}

#CRITERES DE NON INCLUSION
CNIaOUI <- data.frame()
CNIaOUI <- rbind(CNIaOUI, IE[IE$SUBJID == "Subject Identifier for the Study", ])
for (i in 2:length(IE$SUBJID)) {
  if (is.na(IE$IESTRESC5[i]) ||
      IE$IESTRESC5[i] == "OUI" ||
      is.na(IE$IESTRESC6[i]) ||
      IE$IESTRESC6[i] == "OUI" ||
      is.na(IE$IESTRESC7[i]) ||
      IE$IESTRESC7[i] == "OUI" ||
      is.na(IE$IESTRESC8[i]) ||
      IE$IESTRESC8[i] == "OUI" ||
      is.na(IE$IESTRESC9[i]) ||
      IE$IESTRESC9[i] == "OUI" ||
      is.na(IE$IESTRESC10[i]) ||
      IE$IESTRESC10[i] == "OUI" ||
      is.na(IE$IESTRESC11[i]) ||
      IE$IESTRESC11[i] == "OUI" ||
      is.na(IE$IESTRESC12[i]) ||
      IE$IESTRESC12[i] == "OUI" ||
      is.na(IE$IESTRESC13[i]) ||
      IE$IESTRESC13[i] == "OUI" ||
      is.na(IE$IESTRESC14[i]) ||
      IE$IESTRESC14[i] == "OUI" ||
      is.na(IE$IESTRESC15[i]) ||
      IE$IESTRESC15[i] == "OUI" ||
      is.na(IE$IESTRESC16[i]) ||
      IE$IESTRESC16[i] == "OUI" ||
      is.na(IE$IESTRESC17[i]) ||
      IE$IESTRESC17[i] == "OUI" ||
      is.na(IE$IESTRESC18[i]) ||
      IE$IESTRESC18[i] == "OUI" ||
      is.na(IE$IESTRESC19[i]) ||
      IE$IESTRESC19[i] == "OUI" ||
      is.na(IE$IESTRESC20[i]) ||
      IE$IESTRESC20[i] == "OUI" ||
      is.na(IE$IESTRESC21[i]) ||
      IE$IESTRESC21[i] == "OUI") {
    CNIaOUI <- rbind(CNIaOUI, IE[i, ])
  }
}
if (length(CNIaOUI) > 0) {
  CNIaOUI <-
    CNIaOUI[,!(
      names(CNIaOUI) %in% c(
        "IENAME",
        "IEFIRSTNAME",
        "IEDTCONSENT",
        "IESTRESC1",
        "IESTRESC2",
        "IESTRESC3",
        "IESTRESC4",
        "IEEXCL",
        "IEINT",
        "IEINT2",
        "IEEXCL1",
        "IEEXCL2",
        "IEEXCL3",
        "IEEXCL4",
        "IEEXCL5",
        "IEEXCL6",
        "IE1",
        "IE2",
        "IE3"
      )
    )]
}

if(length(CNIaOUI$SUBJID) == 1){
  CNIaOUI <- NULL
} else{CNIaOUI <- t(CNIaOUI)}
#______________________________________________________#----


#______________________________________________________#
#Valeur minimum et maximum tables----
for (df_name in dataframes) {
  df <- get(df_name)
  #Recupere les intitules des variables
  df_temp <- df[1,]
  #Transformations des colonnes sans "SUBJID"
  TransformFact <- lapply(names(df), function(colname) {
    x <- df[[colname]]
    if (colname != "SUBJID") {
      if (is.character(x)) {
        levels_x <- levels(x)
        levels_x <- levels_x[!levels_x %in% c("K", "D", "", ".")]
        if (all(grepl("^\\d+(\\.\\d*)?$", levels_x))) {
          as.numeric(as.character(x))
        } else {
          as.character(x)
        }
      } else {
        x
      }
    } else {
      x
    }
  })
  
  TransformFact <- as.data.frame(TransformFact)
  names(TransformFact) <- names(df)
  
  # 2ieme etape : transformation des dates en type Date
  # TransformFact <- lapply(TransformFact, function(x) {
  #   if (is.character(x)) {
  #     date <- as.Date(x, format = "%d/%m/%Y")
  #     if (any(is.na(date))) {
  #       date <- as.Date(x, format = "%Y-%m-%d")}
  #     if (any(!is.na(date))) {
  #       date
  #     } else {
  #       x
  #     }} else {
  #     x
  #   }})
  
  #Calcule valeurs Min et Max pour chaque colonne
  MinMax <- lapply(TransformFact, function(x) {
    # if (inherits(x, "Date")) {
    #   x <- na.omit(x)
    #   if (length(x) > 0) {
    #     TriDates <- sort(unique(x))
    #     Min <- head(TriDates, 2)
    #     Max <- tail(TriDates, 2)
    #     c(Min = paste(Min, collapse = " / "),
    #       Max = paste(Max, collapse = " / "))
    #   } else {
    #     c(Min = NA, Max = NA)
    #   }
    # } else
      if (is.numeric(x) && !all(is.na(x))) {
      TriNum <- sort(unique(x))
      Min <- head(TriNum, 1)
      Max <- tail(TriNum, 1)
      c(Min = paste(Min, collapse = " / "),
        Max = paste(Max, collapse = " / "))
    } else {
      c(Min = NA, Max = NA)
    }
  })
  
  MinMaxTemp <- as.data.frame(MinMax)
  #Verifie colonne n'est pas NA
  MinMaxTemp <- MinMaxTemp[, colSums(is.na(MinMaxTemp)) < nrow(MinMaxTemp)]
  
  #Permet d'avoir le meme nombre de colonnes entre df_temp et MinMaxTemp
  df_temp <- df_temp[names(MinMaxTemp)]
  
  #Ajout de df_temp a MinMaxTemp (les intitules des variables)
  MinMaxTemp <- rbind(df_temp, MinMaxTemp)

  #Transposition
  transposeTemp <- as.data.frame(t(MinMaxTemp))
  
  if(nrow(transposeTemp) == 0){
    transposeTemp <- NULL
  }
  
  #Data frame final
  assign(paste0(df_name, "MinMax"), transposeTemp)
}
#______________________________________________________#----


#______________________________________________________#
#Donnees manquantes----
for (df_name in dataframes) {
  df <- get(df_name)
  #Recupere les intitules des variables
  df_temp <- df[1,]
  
  #Retrait des patients 0101 et 0109 pour les visites V4 & V5
  if(df_name == "V4" || df_name == "V5"){
    df <- df[df$SUBJID != "0101", ]
    df <- df[df$SUBJID != "0109", ]
  }
  
  #Creation dataframe nombre de donnees manquantes par variable
  Manquant <- sapply(df, function(x) sum(is.na(x)))
  #PERMET DE NE PAS AFFICHER LES VARIABLES SANS DONNEES MANQUANTES
  #ATTENTION FILTRE CHANGER POUR AFFICHER TOUTES LES VARIABLES
  Filtre <- Manquant[Manquant >= 0]
  if (length(Filtre) > 0) {
    #Permet d'avoir le meme nombre de colonnes entre df_temp et Manquant
    df_temp <- df_temp[names(Manquant)]
    #Ajout de df_temp a ManquantTable (les intitules des variables)
    ManquantTable <- rbind(df_temp, Manquant)
  } else{
    ManquantTable
  }

  #Transposition
  transposeTemp <- as.data.frame(t(ManquantTable))
  #Ajout des noms des colonnes
  colnames(transposeTemp) <- c("Variables", "Manquants")
  #Recuperation des champs sans roles dans des activations dynamiques
  noActDy <- setdiff(rownames(transposeTemp), activationDynamique$CDISC_NAME)
  #Ajout des donnees manquantes dans df_Final
  df_Final <- transposeTemp[noActDy,]
  #Transformation du resultat en data frame et creation d'une table par table a la base
  assign(paste0(df_name, "Manquant"), as.data.frame(df_Final))
}
#______________________________________________________#----


#______________________________________________________#
#Donnees manquantes sur les activations dynamiques----
for (df_name in dataframes) {
  df <- get(df_name)

  #Recupere les intitules des variables
  # df_temp <- df[1,]

  #Retrait des patients 0101 et 0109 pour les visites V4 & V5
  if(df_name == "V4" || df_name == "V5"){
    df <- df[df$SUBJID != "0101", ]
    df <- df[df$SUBJID != "0109", ]
  }

  testt <- intersect(colnames(df), activationDynamique$CDISC_NAME)

  testt1 <- df[,testt]


  if(ncol(testt1) > 0){
    assign(paste0(df_name, "testt"), as.data.frame(testt1))
  } else{rm(testt1)}

}
#______________________________________________________#----


#______________________________________________________#
#Donnees NK----
for (df_name in dataframes) {
  df <- get(df_name)
  df_temp <- df[1,]
  
  #Retrait des patients 0101 et 0109 pour les visites V4 & V5
  if(df_name == "V4" || df_name == "V5"){
    df <- df[df$SUBJID != "0101", ]
    df <- df[df$SUBJID != "0109", ]
  }
  
  NKs <- sapply(df, function(x) sum(x == "K", na.rm = TRUE))
  
  #Prendre les colonnes nom et prenom avec NK car peut contenir K sans que ce soit une donnee manquante
  if(any(colnames(df) %in% c("IENAME", "IEFIRSTNAME"))){
    NKs <- sapply(df, function(x) sum(x == "NK", na.rm = TRUE))
  }
  
  #PERMET DE NE PAS AFFICHER LES VARIABLES SANS NK
  #ATTENTION FILTRE CHANGER POUR AFFICHER TOUTES LES VARIABLES
  Filtre <- NKs[NKs >= 0]
  if (length(Filtre) > 0) {
    #Permet d'avoir le meme nombre de colonnes entre df_temp et NKs
    df_temp <- df_temp[names(NKs)]
    #Ajout de df_temp a NKTable (les intitules des variables)
    NKTable <- rbind(df_temp, NKs)
  } else{
      NKTable <- data.frame()
    }
  
  #Transposition
  transposeTemp <- as.data.frame(t(NKTable))
  #Renommage
  colnames(transposeTemp) <- c("Variables", "NKs")
  #Recuperation des champs sans roles dans des activations dynamiques
  noActDy <- setdiff(rownames(transposeTemp), activationDynamique$CDISC_NAME)
  #Ajout des donnees manquantes dans df_Final
  df_Final <- transposeTemp[noActDy,]
  #Transformation du resultat en data frame et creation d'une table par table a la base
  assign(paste0(df_name, "_NK"), as.data.frame(df_Final))
}
#______________________________________________________#----


#______________________________________________________#
#Donnees ND----
for (df_name in dataframes) {
  df <- get(df_name)
  df_temp <- df[1,]
  
  #Retrait des patients 0101 et 0109 pour les visites V4 & V5
  if(df_name == "V4" || df_name == "V5"){
    df <- df[df$SUBJID != "0101", ]
    df <- df[df$SUBJID != "0109", ]
  }
  
  NDs <- sapply(df, function(x) sum(x == "D", na.rm = TRUE))
  
  #Prendre les colonnes nom et prenom avec ND car peut contenir D sans que ce soit une donnee manquante
  if(any(colnames(df) %in% c("IENAME", "IEFIRSTNAME"))){
    NDs <- sapply(df, function(x) sum(x == "ND", na.rm = TRUE))
  }
  
  #PERMET DE NE PAS AFFICHER LES VARIABLES SANS ND
  #ATTENTION FILTRE CHANGER POUR AFFICHER TOUTES LES VARIABLES
  Filtre <- NDs[NDs >= 0]
  if (length(Filtre) > 0) {
    #Permet d'avoir le meme nombre de colonnes entre df_temp et NDs
    df_temp <- df_temp[names(NDs)]
    #Ajout de df_temp a NDTable (les intitules des variables)
    NDTable <- rbind(df_temp, NDs)
  } else{
      NDTable <- data.frame()
    }
    
  #Transposition
  transposeTemp <- as.data.frame(t(NDTable))
  #Renommage
  colnames(transposeTemp) <- c("Variables", "NDs")
  #Recuperation des champs sans roles dans des activations dynamiques
  noActDy <- setdiff(rownames(transposeTemp), activationDynamique$CDISC_NAME)
  #Ajout des donnees manquantes dans df_Final
  df_Final <- transposeTemp[noActDy,]
  #Transformation du resultat en data frame et creation d'une table par table a la base
  assign(paste0(df_name, "_ND"), as.data.frame(df_Final))
}
#______________________________________________________#----


#______________________________________________________#
#Donnees NA----
for (df_name in dataframes) {
  df <- get(df_name)
  df_temp <- df[1,]
  
  #Retrait des patients 0101 et 0109 pour les visites V4 & V5
  if(df_name == "V4" || df_name == "V5"){
    df <- df[df$SUBJID != "0101", ]
    df <- df[df$SUBJID != "0109", ]
  }
  
  NAs <- sapply(df, function(x)
  sum(x == "A", na.rm = TRUE))
  
  #Prendre les colonnes nom et prenom avec NA car peut contenir A sans que ce soit une donnee manquante
  if(any(colnames(df) %in% c("IENAME", "IEFIRSTNAME"))){
    NAs <- sapply(df, function(x) sum(x == "NA", na.rm = TRUE))
  }
  
  #PERMET DE NE PAS AFFICHER LES VARIABLES SANS NA
  #ATTENTION FILTRE CHANGER POUR AFFICHER TOUTES LES VARIABLES
  Filtre <- NAs[NAs >= 0]
  if (length(Filtre) > 0) {
    #Permet d'avoir le meme nombre de colonnes entre df_temp et NAs
    df_temp <- df_temp[names(NAs)]
    #Ajout de df_temp a NATable (les intitules des variables)
    NATable <- rbind(df_temp, NAs)
  } else{
      NATable <- data.frame()
    }
  
  #Transposition
  transposeTemp <- as.data.frame(t(NATable))
  #Renommage
  colnames(transposeTemp) <- c("Variables", "NAs")
  #Recuperation des champs sans roles dans des activations dynamiques
  noActDy <- setdiff(rownames(transposeTemp), activationDynamique$CDISC_NAME)
  #Ajout des donnees manquantes dans df_Final
  df_Final <- transposeTemp[noActDy,]
  #Transformation du resultat en data frame et creation d'une table par table a la base
  assign(paste0(df_name, "_NA"), as.data.frame(df_Final))
}
#______________________________________________________#----


#______________________________________________________#
#Fusion des tables manquantes, NK, ND et NA----
#ATTENTION LES COLONNES SONT TRIEES PAR ORDRE ALPHABETIQUE DES NOMS DE VARIABLES
for (df_name in dataframes) {
  df <- get(df_name)

  #Merge des tables manquant et NK
  nomMerge1 <- get(paste0(df_name, "Manquant"))
  nomMerge2 <- get(paste0(df_name, "_NK"))
  
  #Recuperation des noms des lignes pour le data frame final
  nomMerge1$rowNames <- rownames(nomMerge1)
  nomMerge2$rowNames <- rownames(nomMerge2)
  
  resultat <- merge(nomMerge1, nomMerge2, by = c("Variables", "rowNames"), all = TRUE, sort = FALSE)
  
  rownames(resultat) <- resultat$rowNames
  resultat$rowNames <- NULL
  
  assign(paste0(df_name, "manquant_NK"), resultat)
  rm(list = c(paste0(df_name, "Manquant"), paste0(df_name, "_NK")))
  
  #Merge des tables manquant_NK et ND
  nomMerge1 <- get(paste0(df_name, "manquant_NK"))
  nomMerge2 <- get(paste0(df_name, "_ND"))
  
  #Recuperation des noms des lignes pour le data frame final
  nomMerge1$rowNames <- rownames(nomMerge1)
  nomMerge2$rowNames <- rownames(nomMerge2)
  
  resultat <- merge(nomMerge1, nomMerge2, by = c("Variables", "rowNames"), all = TRUE, sort = FALSE)
  
  rownames(resultat) <- resultat$rowNames
  resultat$rowNames <- NULL
  
  assign(paste0(df_name, "manquant_NK_ND"), resultat)
  rm(list = c(paste0(df_name, "manquant_NK"), paste0(df_name, "_ND")))
  
  #Merge des tables manquant_NK_ND et NA
  nomMerge1 <- get(paste0(df_name, "manquant_NK_ND"))
  nomMerge2 <- get(paste0(df_name, "_NA"))
  
  #Recuperation des noms des lignes pour le data frame final
  nomMerge1$rowNames <- rownames(nomMerge1)
  nomMerge2$rowNames <- rownames(nomMerge2)
  
  resultat <- merge(nomMerge1, nomMerge2, by = c("Variables", "rowNames"), all = TRUE, sort = FALSE)
  
  rownames(resultat) <- resultat$rowNames
  resultat$rowNames <- NULL
  
  rm(list = c(paste0(df_name, "manquant_NK_ND"), paste0(df_name, "_NA")))
  
  resultatFinal <- data.frame()
  for(i in 1:nrow(resultat)){
    if((!is.na(resultat$Manquants[i]) && resultat$Manquants[i] > 0)
       || (!is.na(resultat$NKs[i]) && resultat$NKs[i] > 0) 
       || (!is.na(resultat$NDs[i]) && resultat$NDs[i] > 0)
       || (!is.na(resultat$NAs[i]) && resultat$NAs[i] > 0)){
      resultatFinal <- rbind(resultatFinal, resultat[i, ])
    }
  }
  if(nrow(resultatFinal) == 0){
    resultatFinal <- NULL
  }
  assign(paste0(df_name, "manquant_NK_ND_NA"), resultatFinal)
}
#______________________________________________________#----


#______________________________________________________#
#Dataframe 1 ligne par donnee manquante par patient----
for (df_name in dataframes) {
  df <- get(df_name)
  
  #Retrait des patients 0101 et 0109 pour les visites V4 & V5
  if(df_name == "V4" || df_name == "V5"){
    df <- df[df$SUBJID != "0101", ]
    df <- df[df$SUBJID != "0109", ]
  }
  
  #Creation du dataframe pour avoir les patients dont une donnee est manquante
  dfTemp <- data.frame(Champ = character(), SUBJID = integer(), stringsAsFactors = FALSE)
  
  #Parcours de chaque colonne
  for (col in colnames(df)){
    #Parcours des lignes de chaque colonne
    for (i in 1:nrow(df)){
      #Verifier si la valeur est manquante
      if (is.na(df[i, col]) || df[i, col] == "" || df[i,col] == "K" || df[i, col] == "A" || df[i, col] == "D"){
        #Ajouter une ligne avec le nom de la colonne et le numero patient
        dfTemp <- rbind(dfTemp, data.frame(Champ = col, SUBJID = df$SUBJID[i]))
      }
    }
  }
  
  #Transformation de la colonne SUBJID en caracteres
  dfTemp$SUBJID = as.character(dfTemp$SUBJID)
  #Recuperation des champs sans roles dans des activations dynamiques
  noActDy <- setdiff(dfTemp$Champ, activationDynamique$CDISC_NAME)
  #Transformation de noActDy en data frame
  noActDy <- as.data.frame(noActDy)
  #Ajout des donnees manquantes dans df_Final
  df_Final <- left_join(noActDy, dfTemp, by = c("noActDy" = "Champ"))
  #Ajout des noms des colonnes
  colnames(df_Final) <- c("Champ", "SUBJID")
  
  if(df_name == "V4"){
    # view(df_Final)
    #Filtre pour garder que les pages a V4
    listeQueries_filter <- listeQueries %>%
      filter(PAGE_NUM == '11' | PAGE_NUM == '12' | PAGE_NUM == '13' | PAGE_NUM == '14' | is.na(PAGE_NUM))
    #Jointure entre les donnees manquantes et la liste des queries (%>% = pipe)
    subjidManquantFinal <- df_Final %>%
      left_join(listeQueries_filter, by = c("Champ" = "CDISC_NAME", "SUBJID" = "PATIENT"))
    
    #Jointure entre les donnees manquantes et la liste des commentaires
    # subjidManquantFinal <- subjidManquant %>%
    #   left_join(listeCommentaires, by = c("Champ" = "CDISC_NAME", "SUBJID" = "PATIENT"))
    
  } else if(df_name == "V5"){
    #Filtre pour garder que les pages a V5
    listeQueries_filter <- listeQueries %>%
      filter(PAGE_NUM == '15' | PAGE_NUM == '16' | PAGE_NUM == '17' | PAGE_NUM == '18' | is.na(PAGE_NUM))
    #Jointure entre les donnees manquantes et la liste des queries (%>% = pipe)
    subjidManquant <- df_Final %>%
      left_join(listeQueries_filter, by = c("Champ" = "CDISC_NAME", "SUBJID" = "PATIENT"))
    
    #Jointure entre les donnees manquantes et la liste des commentaires
    subjidManquantFinal <- subjidManquant %>%
      left_join(listeCommentaires, by = c("Champ" = "CDISC_NAME", "SUBJID" = "PATIENT"))

  } else {
    #Jointure entre les donnees manquantes et la liste des queries
    subjidManquant <- left_join(df_Final, listeQueries, by = c("Champ" = "CDISC_NAME", "SUBJID" = "PATIENT"))
    #Jointure entre les donnees manquantes et la liste des commentaires
    subjidManquantFinal <- left_join(subjidManquant, listeCommentaires, by = c("Champ" = "CDISC_NAME", "SUBJID" = "PATIENT"))
  }
  
  if(nrow(subjidManquantFinal) == 0){
    subjidManquantFinal <- NULL
  }
  
  #Transformation du resultat en data frame et creation d'une table par table a la base
  assign(paste0(df_name, "subjidManquant"), subjidManquantFinal)
}
#______________________________________________________#----


#______________________________________________________#
#!!!!!ETUDE SPECIFIQUE!!!!!----

#VERIFICATION BRAS/VISITE D'ADMINISTRATION
randRespect <- data.frame()
nbV4 <- 0
nbV5 <- 0
for (i in 2:length(RD$SUBJID)){
  if(!is.na(RD$RANDOGROUP[i]) && !is.na(RD$VSVPIPA4[i])){
    if(RD$RANDOGROUP[i] == "Administration de VPIP à la visite V4" && RD$VSVPIPA4[i] == "OUI"){
      nbV4 <- nbV4 + 1
    } else if(!is.na(RD$VSVPIPA5[i])){
      if(RD$RANDOGROUP[i] == "Administration de VPIP à la visite V5" && RD$VSVPIPA5[i] == "OUI"){
        nbV5 <- nbV5 + 1
      }
    }
  }
}
randRespect <- rbind(randRespect, nbV4)
randRespect <- cbind(randRespect, nbV5)
colnames(randRespect) <- c("Nb respect V4", "Nb respect V5")

#VERIFICATION DOSE VPIP
colonne <- c("VSINC5", "VSINC10", "VSINC15", "VSINC20", "VSINC25", "VSINC30", "VSINC35", "VSINC40")

#Fonction pour trouver la dernière colonne < 4 et non NA
last_colonne <- function(ligne) {
  for (col in rev(colonne)) {
    val <- ligne[[col]]
    if (!is.na(val) && val < 4) {
      return(col)
    }
  }
  return(NA)
}

RD$last_colonne_patient <- gsub("VSINC", "", apply(RD[, colonne], 1, last_colonne))
RD$admin_VPIP <- NA
for(i in 2:length(RD$SUBJID)){
  if(!is.na(RD$RANDOGROUP[i]) && RD$RANDOGROUP[i] == "Administration de VPIP à la visite V4"){
    RD$admin_VPIP[i] <- RD$last_colonne_patient[i] == RD$VSVPIPAPRES4[i]
  } else if(!is.na(RD$RANDOGROUP[i]) && RD$RANDOGROUP[i] == "Administration de VPIP à la visite V5"){
    RD$admin_VPIP[i] <- RD$last_colonne_patient[i] == RD$VSVPIPAPRES5[i]
  }
}

admin_VPIP_NonRespect <- data.frame()
#Recupere les intitules des variables
RD_temp <- RD[1,]
for(i in 2:length(RD$SUBJID)){
  if(!is.na(RD$admin_VPIP[i]) && RD$admin_VPIP[i] != "TRUE"){
    admin_VPIP_NonRespect <- rbind(admin_VPIP_NonRespect, RD[i, ])
    admin_VPIP_NonRespect <- admin_VPIP_NonRespect[c("SUBJID", "RANDOGROUP", "VSVPIPA4", "VSVPIPAPRES4", "VSVPIPA5", "VSVPIPAPRES5", "last_colonne_patient", "admin_VPIP")]
    #Permet d'avoir le meme nombre de colonnes entre df_temp et MinMaxTemp
    RD_temp <- RD_temp[names(admin_VPIP_NonRespect)]
    #Ajout de RD_temp a admin_VPIP_NonRespect (les intitules des variables)
    admin_VPIP_NonRespect <- rbind(RD_temp, admin_VPIP_NonRespect)
    admin_VPIP_NonRespect <- t(admin_VPIP_NonRespect)
  }
}

#CRITERES D'EXCLUSION ET D'INTERRUPTION
#CRITERES D'EXCLUSION
CEaOUI <- data.frame()
CEaOUI <- rbind(CEaOUI, CEXCLUSION[CEXCLUSION$SUBJID == "Subject Identifier for the Study", ])
for (i in 2:length(CEXCLUSION$SUBJID)) {
  if (is.na(CEXCLUSION$IEEXCL[i]) ||
      CEXCLUSION$IEEXCL[i] == "OUI" ||
      is.na(CEXCLUSION$IEEXCL1[i]) ||
      CEXCLUSION$IEEXCL1[i] == "OUI" ||
      is.na(CEXCLUSION$IEEXCL2[i]) ||
      CEXCLUSION$IEEXCL2[i] == "OUI" ||
      is.na(CEXCLUSION$IEEXCL3[i]) ||
      CEXCLUSION$IEEXCL3[i] == "OUI" ||
      is.na(CEXCLUSION$IEEXCL4[i]) ||
      CEXCLUSION$IEEXCL4[i] == "OUI" ||
      is.na(CEXCLUSION$IEEXCL5[i]) ||
      CEXCLUSION$IEEXCL5[i] == "OUI" ||
      is.na(CEXCLUSION$IEEXCL6[i]) ||
      CEXCLUSION$IEEXCL6[i] == "OUI" ||
      is.na(CEXCLUSION$IE1[i]) ||
      CEXCLUSION$IE1[i] == "OUI" ||
      is.na(CEXCLUSION$IE2[i]) ||
      CEXCLUSION$IE2[i] == "OUI" ||
      is.na(CEXCLUSION$IE3[i]) ||
      CEXCLUSION$IE3[i] == "OUI" ||
      is.na(CEXCLUSION$VSIEEXCLV4[i]) ||
      CEXCLUSION$VSIEEXCLV4[i] == "OUI" ||
      is.na(CEXCLUSION$VSIEEXCLV5[i]) ||
      CEXCLUSION$VSIEEXCLV5[i] == "OUI") {
    CEaOUI <- rbind(CEaOUI, CEXCLUSION[i, ])
  }
}
if (length(CEaOUI) > 0) {
  CEaOUI <-
    CEaOUI[,!(
      names(CEaOUI) %in% c(
        "IEINT",
        "IEINT1",
        "VSIEINTV4",
        "VSIEINTV5"
      )
    )]
}

if(length(CEaOUI$SUBJID) == 1){
  CEaOUI <- NULL
} else{CEaOUI <- t(CEaOUI)}

#CRITERES D'INTERRUPTION
CINTaOUI <- data.frame()
CINTaOUI <- rbind(CINTaOUI, CEXCLUSION[CEXCLUSION$SUBJID == "Subject Identifier for the Study", ])
for (i in 2:length(CEXCLUSION$SUBJID)) {
  if (is.na(CEXCLUSION$IEINT[i]) ||
      CEXCLUSION$IEINT[i] == "OUI" ||
      is.na(CEXCLUSION$IEINT1[i]) ||
      CEXCLUSION$IEINT1[i] == "OUI" ||
      is.na(CEXCLUSION$VSIEINTV4[i]) ||
      CEXCLUSION$VSIEINTV4[i] == "OUI" ||
      is.na(CEXCLUSION$VSIEINTV5[i]) ||
      CEXCLUSION$VSIEINTV5[i] == "OUI") {
    CINTaOUI <- rbind(CINTaOUI, CEXCLUSION[i, ])
  }
}
if (length(CINTaOUI) > 0) {
  CINTaOUI <-
    CINTaOUI[,!(
      names(CINTaOUI) %in% c(
        "IEEXCL",
        "IEEXCL1",
        "IEEXCL2",
        "IEEXCL3",
        "IEEXCL4",
        "IEEXCL5",
        "IEEXCL6",
        "IE1",
        "IE2",
        "IE3",
        "VSIEEXCLV4",
        "VSIEEXCLV5"
      )
    )]
}

if(length(CINTaOUI$SUBJID) == 1){
  CINTaOUI <- NULL
} else{
  CINTaOUI <- t(CINTaOUI)
}

#CRITERE DE JUGEMENT PRINCIPAL
critPrinc <- data.frame("P1_numeroPatient" = RD$SUBJID, "P1_pressionVPIPMaximale" = RD$last_colonne_patient)

#CRITERES DE JUGEMENT SECONDAIRES
critSec1 <- data.frame("V1_numeroPatient" = V1$SUBJID, "V1_respDif" = V1$VSRESP, "V1_inconf" = V1$VSINC, "V1_ang" = V1$VSANG)

critSec2 <- data.frame("V2_numeroPatient" = V2$SUBJID, "V2_dysp" = V2$VSDYSP, "V2_mMRC" = V2$VSMMRC, "V2_leic" = V2$VSLEIC)
#______________________________________________________#----

#Suppression des tables inutiles----
rm(
  df,
  df_Final,
  ManquantTable,
  MinMax,
  MinMaxTemp,
  NATable,
  NDTable,
  NKTable,
  nomMerge1,
  nomMerge2,
  resultat,
  TransformFact,
  dataframes,
  df_name,
  Filtre,
  Manquant,
  NAs,
  NDs,
  NKs,
  transposeTemp,
  df_temp,
  RD_temp
)
