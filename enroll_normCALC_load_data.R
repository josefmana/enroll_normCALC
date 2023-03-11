
###########
# Norm-HD
###########

library(psych)
library(dplyr)

##########################
## Set Working Directory:

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
date <- Sys.Date()

##################################
## Thresholds for data filtering:
mmse_thresh <- 27 # Threshold for MMSE (only mmse_thresh or larger kept for datCog.2)
dep_thresh <- 10  # Threshold for depression scale (values larger than 10 are ... 
                  # excluded, i.e. exclude HADS > 10 )

###############
## Load data:

data <- read.csv("data/enroll_2020-07-14.csv", sep = ",")

sx <- read.csv("data/profile.csv", sep = "\t")

var.sx <- c("subjid" ,"region", "sex", "race", "handed", "caghigh", "caglow")
sx <- sx[colnames(sx) %in% var.sx] # keep only variables in profile data set that ... 
                                   # are relevant (included in var.sx vector)
sx <- sx[sx$subjid %in% data$subjid, ] # keep only profile data from subjects ...
                                   # that are also in the enroll_language data set

## Check if data and sx have participants in the same order
check <- lapply(1:nrow(data), function(i) isTRUE(data$subjid[i] == sx$subjid[i]))
all(unlist(check))

## If the last is TRUE, add variables from Profile
for (i in c(2:length(var.sx))) {
  data[var.sx[i]] <- sx[var.sx[i]]
}

# Variables of interest (@Josef: we included some other variables, for example ... 
# the PBA which assesses psychiatric symptoms and CAG value for sample description)
var <- c("subjid",
            "hdcat",
            "age",
            "sex",
            "region",
            "race",
            "handed",
            "caghigh", 
            "caglow",
            "language",
            "edu",
            "isced", ## ISCED education level
            "anxscore", ## HADS-SIS anxiety subscore
            "hads_depscore", ## HADS-SIS depression subscore
          "hxalcab", ## alcohol problems in past
            "hxtobab", ## ever smoked
            "hxdrugab", ## ever abused drugs
            "hxmar", ## marijuana abuse
            "hxher", ## heroin abuse
            "hxcoc", ## cocaine abuse
            "hxclb", ## club drugs abuse
            "hxamp", ## amphetamins abuse
            "hxrit", ## ritalin abuse
            "hxhal", ## halucinogens abuse
            "hxinh", ## inhalants abuse
            "hxopi", ## opium abuse
            "hxpak", ## painkillers abuse
            "hxbar", ## barbiturates abuse
            "hxtrq", ## tranquilizers abuse
         "gen1", ## 1 == ass in native language and with good vision
         "sdmt", ## SDMT, 0 == missing
            "sdmt1", ## total correct
            "sdmtnd", ## reason
         "verfct", ## Category Fluency, 0 == missing
            "verfctd", ## category, 1 == animals, 2 == other
            "verfct5", ## total correct (1 min)
            "verfctnd",
         "scnt", ## Stroop Colour Naming, 0 == missing
            "scnt1", ## total correct
            "scntnd",
         "swrt", ## Stroop Word Reading, 0 == missing
            "swrt1", ## total correct
            "swrtnd",
         "sit", ## Stroop Interference Test, 0 == missing
            "sit1", ## total correct
         "trl", ## Trailmaking Test, 0 == missing
            "trla1", ## tmt A time to complete
            "trlb1", ## tmt B time to complete
         "verflt", ## Verbal Fluency Test Letters, 0 == missing
            "verflt05", ## total correct (3 min)
            "verflt15", ## total correct first letter (1 min)
            "verflt25", ## total correct second letter (1 min)
            "verflt35", ## total correct third letter (1 min)
         "mmsetotal", ## MMSE score
         "indepscl", 
         "depscore",
         "irascore",
         "psyscore",
         "aptscore",
         "exfscore",
         "pbas1sv",
         "pbas1fr",
         "pbas1wo",
         "pbas2sv",
         "pbas2fr",
         "pbas2wo",
         "pbas3sv",
         "pbas3fr",
         "pbas3wo",
         "pbas4sv",
         "pbas4fr",
         "pbas4wo",
         "pbas5sv",
         "pbas5fr",
         "pbas5wo",
         "pbas6sv",
         "pbas6fr",
         "pbas6wo",
         "pbas7sv",
         "pbas7fr",
         "pbas7wo",
         "pbas8sv",
         "pbas8fr",
         "pbas8wo",
         "pbas9sv",
         "pbas9fr",
         "pbas9wo",
         "pbas10sv",
         "pbas10sm__1",
         "pbas10sm__2",
         "pbas10sm__4",
         "pbas10sm__3",
         "pbas10sm__5",
         "pbas10fr",
         "pbas10wo",
         "pbas11sv",
         "pbas11fr",
         "pbas11wo",
         "pbainfo",
         "pbahshd"
)

# Keeping only variables of interest and only controls
datCog <- data[, colnames(data) %in% var]
datCog <- datCog[which(datCog$hdcat == 4|datCog$hdcat == 5),]

# Change all 9996, 9997, and 9998 to NA
for (i in 1:nrow(datCog)) {
  for (j in 1:ncol(datCog)) {
    if (isTRUE(datCog[i, j] == 9996 | datCog[i, j] == 9997 | datCog[i, j] == 9998)){
      datCog[i,j] = NA
    }
  }
}

# Changing format (isced to factor)
datCog$isced <- as.factor(datCog$isced)

# Recode sex, race, handed, and hdcat
datCog$race <- recode_factor(datCog$race, `1` = "Caucasian", `2` = "American Black", 
                             `3` = "Hispanic or Latino Origin",
                             `8` = "Native American", `16` = "Asian",
                             `15` = "Mixed", `6` = "Other")
datCog$handed <- recode_factor(datCog$handed, `1` = "Right-Handed",
                               `2` = "Left-handed", `3` = "Mixed-Handed")
datCog$sex <- recode(datCog$sex, "m" = "male", "f" = "female")

# Save as csv
# write.table(datCog, paste("data/", date, "-datCog-Healthy-Controls-3719.csv", sep = ""),
#             sep = ",", row.names = F)

#########################
# Data Cleaning
#########################

## 1. Exclusion Step: 
## Count and exclude participants <18 years of age, those who were not tested in their
## native language or with corrected vision or hearing, missing info about education,

# Count "Sociodemographical Exclusion": 
nrow(datCog[which(datCog$hdcat == 4),]) # gene negative: n = 1926
nrow(datCog[which(datCog$hdcat == 5),]) # family controls: n = 1793
nrow(datCog[which(datCog$age == "<18"),]) # under 18: n = 4
nrow(datCog[which(datCog$gen1 == 0|is.na(datCog$gen1)),]) # non-corrected ... 
                                          # vision/hearing or missing value: n = 130
nrow(datCog[which(is.na(datCog$isced)),]) # isced missing: n = 12
nrow(datCog[which(is.na(datCog$edu)),]) # NEW years of education missing: n = 17
                                        # 12 missing isced & edu + 5 missing only edu
nrow(datCog[which(is.na(datCog$language)),]) # NEW language missing: n = 10

# Exclude: n = 157
datCog <- datCog[-which(datCog$age == "<18"|
                          datCog$gen1 == 0|is.na(datCog$gen1)|
                          is.na(datCog$isced)|is.na(datCog$edu)|
                          is.na(datCog$language)), ]

# NEW adding changes agreed on 15.7.2020
# tmt-a drop
nrow(datCog[which(datCog$trla1 < 5), ]) # n =2
datCog[which(datCog$trla1 < 5), ]$trla1 <- NA
# tmt-b change
datCog[which(datCog$trlb1 == 0 | datCog$trlb1 == 250 ), ]$trlb1 <- 240
# edu change
datCog[which(datCog$subjid == "R91063445X"), ]$edu <- 21
datCog[which(datCog$subjid == "R449474958"), ]$isced <- 3
datCog[which(datCog$subjid == "R449474958"), ]$edu <- 13
datCog[which(datCog$subjid == "R610662549"), ]$edu <- 19
datCog[which(datCog$subjid == "R633444100"), ]$edu <- 17
datCog[which(datCog$subjid == "R862035076"), ]$edu <- 15
datCog[which(datCog$edu > 24), ]$edu <- 24

# Save data file :
# write.table(datCog,
#             paste(c("../datCog_Healthy-Controls_technicalExclusion_3562.csv"),
#                   collapse = ''),
#             append=FALSE,sep =",", col.names=TRUE, row.names=FALSE)


## 2. Exclusion Step: 
## Count Depression, Cognition and Comorbidities Exclusion:

# Step 2.1: Count how many participants scored > 10 on hads depression scale ...
# (i.e. we only exclude patients with existing hads value of > 10 ...
# (do not exclude patients with missing hads)
nrow(datCog[which(datCog$hads_depscore > dep_thresh),]) ## depressive: n = 98
nrow(datCog[which(is.na(datCog$hads_depscore)),]) ## missing hads depression: n = 867

# PBA-s Depression Score Criterion (only for us as additional information not used ...
# for cleaning data):
nrow(datCog[which(datCog$pbas1sv > 2),]) # Depression Severity: n = 185 
nrow(datCog[which(datCog$pbas2sv > 1),]) # Suicidality Severity: n = 64

## Exclude Acute, ongoing Depression (hads depression > 10): 
datCog.1 <- datCog[-which(datCog$hads_depscore > dep_thresh), ]

## Step 2.2: Check how many participants score MMSE < 27:
nrow(datCog.1[which(datCog.1$mmsetotal < mmse_thresh),]) # n = 180

## Check how many patients were not assessed with MMSE (i.e. MMSE missing)
nrow(datCog.1[which(is.na(datCog.1$mmsetotal)),]) # n = 919

## Exclude MMSE: 
datCog.1 <- datCog.1[-which(datCog.1$mmsetotal < mmse_thresh), ]


## Additional Information for us about indepent functionality --> Check independence...
# score of patients: 
nrow(datCog[which(datCog$indepscl < 75),]) # n = 6, is not used to clean data


## Step 2.3: Exclude data frpm participants who have relevant, severe comorbid ...
# condition (psychiatric, neurological or other medical disorder) that may ...
# potentially affect cognition: 

# load data with comorbid conditions:
comorbid <- read.delim("data/comorbid.csv")

## only keep variables of interest: 
var_comorbid <- c("subjid",
         "mhterm__decod",
         "mhenrf",
         "mhstdy", 
         "mhendy")

## Keeping only variables of interest and only ongoing comorbid conditions ...
## (mhenrf == 1)
comorbid <- comorbid[, colnames(comorbid) %in% var_comorbid]
comorbid.1 <- comorbid[which(comorbid$mhenrf == 1),]

## Keeping only comorbidities that have been diagnosed before or at baseline visit ...
# (mhstdy < 1)
comorbid.2 <- comorbid.1[which(comorbid.1$mhstdy < 1),] 

# Save data file :
# write.table(comorbid.2 , paste(c("../comorbid.2.csv"),collapse = ''),
#            append = F,sep = ",", col.names = T, row.names = F)

## Excluding all comorbitidies that are relevant, thus all ongoing psychiatric,  ...
## neurological and other severe medical disorders and ongoing treatment potentially ...
## affecting cognition:
var_relComorbid <- c("F00", # AD Demenz
                     "F00.0", # AD Frühbeginn
                     "F00.1", # Spätbeginn AD
                     "F00.2", # AD atypisch
                     "F01", # Vasculäre Demenz
                     "F01.1", # Multiinfarktdemenz
                     "F01.9", # n.n.b. vaskuläre Demenz
                     "F02.0", # Pick (FTD)
                     "F02.1", # Creutzfeld-Jakob
                     "F02.2", # HD Demenz
                     "F02.23", # HD Demenz mit depression
                     "F02.3", # PD Demenz
                     "F02.8", # n.n.b. Demenz
                     "F04", # organisches amnestisches Syndrom
                     "F06.2", # organische wahnhafte Störung
                     "F07.0", # Organische Persönlichkeitsstörung
                     "F07.1", # Postenzephalitisches Syndrom
                     "F07.2", # Organisches Psychosyndrom nach SHT
                     "F07.8", # Sonstiges organ. Persönlichkeitsstörung
                     "F07.9", # n.n.b. organische Persönlichkeits
                     "F10.0", # akute Alkoholintoxikation
                     "F10.2", # Alkoholkrankheit
                     "F11.2", # Opiat-Abhänigkeit
                     "F19.2", # Polytoximanie
                     "F12.2", # Cannabis-Abhängigkeit
                     "F13.2", # Benzo-Abhängigkeit
                     "F14.2", # Kokain-Abhängigkeit
                     "F15.2", # Stimulanzien- Abhängigkeit
                     "F20", # Schizophrenie
                     "F20.0", # paranoide Schizophrenie
                     "F20.8", # n.n.b. Schizophrenie
                     "F22", # anhaltende wahnhafte Störung
                     "F22.0", # akute wahnhafte Störung
                     "F23", # akute vorübergehende psychotische Störung
                     "F25.1", # schizoaffektive Störung mit akuter Manie
                     "F31.2", # Bipolare Störung mit Manie
                     "F31.4", # Bipolare Störung mit schwerer Depression
                     "F31.5", # Bipolare Störung mit Depression und Psychotischen
                     "F32.2", # Schwere Depression ohne Psychose
                     "F32.3", # schwere Depression mit Psychose
                     "F33.3", # schwere rezidivierende Depression
                     "F44", # Dissoziative Störungen
                     "F79", # Intelligenzminderung
                     "F70.9", # Leichte Intelligenzminderung
                     "G10", # HD
                     "G12.2", # ALS
                     "G30", # AD
                     "G45", # Stroke
                     "I21", # akuter Herzinfarkt
                     "R40.2", # Koma
                     "R41", # Bewusstseinsstörungen
                     "S06", # intrakranielle Verletzung
                     "S06.0", # Gehirnerschütterung
                     "S06.1", # Gehirnödem
                     "S06.2", # Diffuse Hirnverletzung
                     "S06.3", # nicht definierte Hirnverletzung
                     "S06.7", # Bewusstlosigkeit bei SHT
                     "S06.70", # Bewusstlosigkeit bis 30 min
                     "S06.71", # Bewusstlosigkeit 30 min - 24 h
                     "S06.72", # Bewusstlosigkeit > 24 h
                     "S06.79", # Bewusstlosigkeit dauer nicht bezeichnet
                     "S07", # Zerquetschungen des Kopfes
                     "Z51.0", # akute Strahlentherapie
                     "Z51.2" # akute Chemotherapie
)


### Keeping only relevant diagnoses: 
comorbid.3 <- comorbid.2[which(comorbid.2$mhterm__decod %in% var_relComorbid), ]
# Save data file : 
# write.table(comorbid.3 , paste(c("../comorbid.3.csv"),collapse = ''),
#            append = F, sep = ",", col.names = T, row.names = F)


## Keeping only data from relevant subjids: 
# var_relSubj <- datCog$subjid
# comorbid.4 <- comorbid.3[which(comorbid.3$subjid %in% var_relSubj), ]
comorbid.4 <- comorbid.3[comorbid.3$subjid %in% datCog.1$subjid, ] # keep only ...
                                # comorbid data from subjects that are also in ...
                                # the healthy control enroll data set (datCog.1)
length(unique(comorbid.4$subjid)) # NEW, n = 17 patients with at least one comorbid.4
# 19 Diagnoses remain - 2 subjects have 2 diagnoses -> we exclude n = 17 subjects ...
# because of severe comorbid conditions


# Save data file : 
# write.table(comorbid.4 , paste(c("../comorbid.4.csv"), collapse = ''),
#             append = F, sep = ",", col.names = T, row.names = F)


## Merge Comorbid with datCog.1:
datCog_comorbid <- merge(datCog.1, comorbid.4, by = "subjid", all.x = T)
                        # 3285 observations as 2 subjects have 2 comorbid diagnoses


## Exclude: n = 17 subjects with severe comorbid conditions
dataCog_comorbid_clean <- datCog_comorbid[which(is.na(datCog_comorbid$mhterm__decod)),]
                        # n = 3267, i.e. n = 17 subjects (2 subjects with 2 ... 
                        # diagnoses each) are removed 

# Save data file : 
# write.table(dataCog_comorbid_clean , paste(c("../Soft_Healthy-Controls-3267.csv"),
#                                            collapse = ''), 
#             append = F, sep = ",", col.names = T, row.names = F)


# Important remark: This is the data set  which we will report in the main text
# (Prof. Landwehrmeyer decided this)


##########################
# 3. Exclusion Step:
# Create a dataset with only data from subjects that have completed the full ...
# cognitive battery

## Check how many NAs are there in the dataset
nas <- lapply(1:length(var), function(x)
  sum(is.na(datCog[,var[x]])))
names(nas) <- var

## Exclude participants with missing tests
## First calculate how many participants will be excluded: n = 1094
nrow(dataCog_comorbid_clean[which(dataCog_comorbid_clean$sdmt == 0 |
                                    is.na(dataCog_comorbid_clean$sdmt) |
                                    is.na(dataCog_comorbid_clean$sdmt1) |
                                    dataCog_comorbid_clean$verfct == 0 |
                                    is.na(dataCog_comorbid_clean$verfct) |
                                    is.na(dataCog_comorbid_clean$verfctd) |
                                    is.na(dataCog_comorbid_clean$verfct5) |
                                    dataCog_comorbid_clean$verflt == 0 |
                                    is.na(dataCog_comorbid_clean$verflt) |
                                    is.na(dataCog_comorbid_clean$verflt05) |
                                    dataCog_comorbid_clean$scnt == 0 |
                                    is.na(dataCog_comorbid_clean$scnt) |
                                    is.na(dataCog_comorbid_clean$scnt1) |
                                    dataCog_comorbid_clean$swrt == 0 |
                                    is.na(dataCog_comorbid_clean$swrt) |
                                    is.na(dataCog_comorbid_clean$swrt1) |
                                    dataCog_comorbid_clean$sit == 0 |
                                    is.na(dataCog_comorbid_clean$sit) |
                                    is.na(dataCog_comorbid_clean$sit1) |
                                    dataCog_comorbid_clean$trl == 0 |
                                    is.na(dataCog_comorbid_clean$trl) |
                                    is.na(dataCog_comorbid_clean$trla1) |
                                    is.na(dataCog_comorbid_clean$trlb1) |
                                    is.na(dataCog_comorbid_clean$mmsetotal)),])



## Now calculate exclusions one by one
## SDMT: n = 19
nrow(dataCog_comorbid_clean[which(dataCog_comorbid_clean$sdmt == 0 |
                                    is.na(dataCog_comorbid_clean$sdmt) |
                                    is.na(dataCog_comorbid_clean$sdmt1)),]) 

## Categorical fluency: n = 25
nrow(dataCog_comorbid_clean[which(dataCog_comorbid_clean$verfct == 0 |
                                    is.na(dataCog_comorbid_clean$verfct) |
                                    is.na(dataCog_comorbid_clean$verfctd) |
                    is.na(dataCog_comorbid_clean$verfct5)),])

# Letter fluency: n = 541
nrow(dataCog_comorbid_clean[which(dataCog_comorbid_clean$verflt == 0 |
                                    is.na(dataCog_comorbid_clean$verflt) |
                                    is.na(dataCog_comorbid_clean$verflt05)),])

## Stroop naming: n = 30
nrow(dataCog_comorbid_clean[which(dataCog_comorbid_clean$scnt == 0 |
                                    is.na(dataCog_comorbid_clean$scnt) |
                                    is.na(dataCog_comorbid_clean$scnt1)),])

## Stroop wording: n = 25
nrow(dataCog_comorbid_clean[which(dataCog_comorbid_clean$swrt == 0 |
                                    is.na(dataCog_comorbid_clean$swrt) |
                                    is.na(dataCog_comorbid_clean$swrt1)),])

## Stroop interference: n = 208
nrow(dataCog_comorbid_clean[which(dataCog_comorbid_clean$sit == 0 |
                                    is.na(dataCog_comorbid_clean$sit) |
                                    is.na(dataCog_comorbid_clean$sit1)),])

## TMT: n = 517
nrow(dataCog_comorbid_clean[which(dataCog_comorbid_clean$trl == 0 |
                                    is.na(dataCog_comorbid_clean$trl) |
                                    is.na(dataCog_comorbid_clean$trla1) |
                                    is.na(dataCog_comorbid_clean$trlb1)),])

## MMSE missing: n = 912
sum(is.na(dataCog_comorbid_clean$mmsetotal))



## Exclude missing (keeping all MMSE)

# Excluding one by one: 

# SDMT:
datCog.fullCogBat <- dataCog_comorbid_clean[-which(dataCog_comorbid_clean$sdmt == 0 |
                                                  is.na(dataCog_comorbid_clean$sdmt) |
                                                  is.na(dataCog_comorbid_clean$sdmt1)),]
nrow(datCog.fullCogBat) # n = 3248 (n = 19 excluded)

## Categorical fluency:
datCog.fullCogBat <- datCog.fullCogBat[-which(datCog.fullCogBat$verfct == 0 |
                                                is.na(datCog.fullCogBat$verfct)),]
nrow(datCog.fullCogBat) # n = 3231 (n = 17 excluded)

# Letter fluency: 
datCog.fullCogBat <- datCog.fullCogBat[-which(datCog.fullCogBat$verflt == 0 |
                                                is.na(datCog.fullCogBat$verflt) |
                                                is.na(datCog.fullCogBat$verflt05)),]
nrow(datCog.fullCogBat) # n = 2708 (n = 523 excluded)

## Stroop naming:
datCog.fullCogBat <- datCog.fullCogBat[-which(datCog.fullCogBat$scnt == 0 |
                                                is.na(datCog.fullCogBat$scnt) |
                                                is.na(datCog.fullCogBat$scnt1)),]
nrow(datCog.fullCogBat) # n = 2697 (n = 11 excluded)

## Stroop wording: 
datCog.fullCogBat <- datCog.fullCogBat[-which(datCog.fullCogBat$swrt == 0 |
                                                is.na(datCog.fullCogBat$swrt) |
                                                is.na(datCog.fullCogBat$swrt1)),]
nrow(datCog.fullCogBat) # n = 2696 (n = 1 excluded)

## Stroop interference:  
datCog.fullCogBat <- datCog.fullCogBat[-which(datCog.fullCogBat$sit == 0 |
                                                is.na(datCog.fullCogBat$sit) |
                                                is.na(datCog.fullCogBat$sit1)),]
nrow(datCog.fullCogBat) # n = 2692 (n = 4 excluded)

## TMT: 
datCog.fullCogBat <- datCog.fullCogBat[-which(datCog.fullCogBat$trl == 0 |
                                                is.na(datCog.fullCogBat$trl) |
                                                is.na(datCog.fullCogBat$trla1) | 
                                                is.na(datCog.fullCogBat$trlb1)),]
nrow(datCog.fullCogBat) # n = 2650 (n = 42 excluded)

## MMSE missing: 
datCog.fullCogBat <- datCog.fullCogBat[-which( is.na(datCog.fullCogBat$mmsetotal)),]
nrow(datCog.fullCogBat) # n = 2173 (n = 477 excluded)


                                                   
# Excluding all at once:
datCog.fullCogBat <- dataCog_comorbid_clean[-which(dataCog_comorbid_clean$sdmt == 0 |
                                            is.na(dataCog_comorbid_clean$sdmt) |
                                            is.na(dataCog_comorbid_clean$sdmt1) |
                                          dataCog_comorbid_clean$verfct == 0 |
                                            is.na(dataCog_comorbid_clean$verfct) |
                                            is.na(dataCog_comorbid_clean$verfctd) |
                                            is.na(dataCog_comorbid_clean$verfct5) |
                                          dataCog_comorbid_clean$verflt == 0 |
                                            is.na(dataCog_comorbid_clean$verflt) |
                                            is.na(dataCog_comorbid_clean$verflt05) |
                                          dataCog_comorbid_clean$scnt == 0 |
                                            is.na(dataCog_comorbid_clean$scnt) |
                                            is.na(dataCog_comorbid_clean$scnt1)|
                                          dataCog_comorbid_clean$swrt == 0 |
                                            is.na(dataCog_comorbid_clean$swrt) |
                                            is.na(dataCog_comorbid_clean$swrt1)|
                                          dataCog_comorbid_clean$sit == 0 |
                                            is.na(dataCog_comorbid_clean$sit) |
                                            is.na(dataCog_comorbid_clean$sit1) |
                                          dataCog_comorbid_clean$trl == 0 |
                                            is.na(dataCog_comorbid_clean$trl) |
                                            is.na(dataCog_comorbid_clean$trla1) |
                                            is.na(dataCog_comorbid_clean$trlb1) |
                                            is.na(dataCog_comorbid_clean$mmsetotal)),]

# Save Dataset with full cognitive battery performance:
# write.table(datCog.fullCogBat ,
#             paste(c("../Hard_FullCogBat_Healthy-Controls-2174.csv"),
#                   collapse = ''),
#             append = F, sep = ",", col.names = T, row.names = F)


## New NA check, for datCog.1
nas1 <- lapply(1:length(var), function(x)
  sum(is.na(dataCog_comorbid_clean[,var[x]])))
names(nas1) <- var
## and for datCog.2
nas2 <- lapply(1:length(var), function(x)
  sum(is.na(datCog.fullCogBat[,var[x]])))
names(nas2) <- var

## NEW create and save one big happy data file for analysis
dat <- list(datCog, dataCog_comorbid_clean, datCog.fullCogBat)
names(dat) <- c("full-house", "soft-exclusion", "hard-exclusion")
save(dat, file = "hd-norm-data.RData")

