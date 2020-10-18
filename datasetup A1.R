
# datasetup article1: School adjustment among adolescence.. 
# koronakevät -survey data from April 2020

library(here)
library(dplyr)
library(data.table)
library(misty)

d <- read.csv(here("data/raw/korona2020.csv"), header = T, encoding = 'UTF-8')
# convert into datatable
DT = as.data.table(d)

##### MERGE VARIABLES FROM DIFFERENT RESPONDENT GROUPS (class)
DT[, pos1 := ifelse(!is.na(etaopetuksessa_hyvaa_49_1), etaopetuksessa_hyvaa_49_1, etaopetuksessa_hyvaa_13_1)]
DT[, pos2 := ifelse(!is.na(etaopetuksessa_hyvaa_49_2), etaopetuksessa_hyvaa_49_2, etaopetuksessa_hyvaa_13_2)]
DT[, pos3 := etaopetuksessa_hyvaa_49_3]
DT[, pos4 := ifelse(!is.na(etaopetuksessa_hyvaa_49_4), etaopetuksessa_hyvaa_49_4, etaopetuksessa_hyvaa_13_3)]
DT[, pos5 := ifelse(!is.na(etaopetuksessa_hyvaa_49_5), etaopetuksessa_hyvaa_49_5, etaopetuksessa_hyvaa_13_4)]
DT[, pos6 := ifelse(!is.na(etaopetuksessa_hyvaa_49_6), etaopetuksessa_hyvaa_49_6, etaopetuksessa_hyvaa_13_5)]
DT[, pos7 := ifelse(!is.na(etaopetuksessa_hyvaa_49_7), etaopetuksessa_hyvaa_49_7, etaopetuksessa_hyvaa_13_6)]
DT[, posno := ifelse(!is.na(etaopetuksessa_hyvaa_49_8), etaopetuksessa_hyvaa_49_8, etaopetuksessa_hyvaa_13_7)]
DT[, pos9 := etaopetuksessa_hyvaa_49_9]
DT[, positop := etaopetuksessa_hyvaa_49_mita]
# drop originals
DT = DT[, !names(DT) %like% "etaopetuksessa", with = F]

# get new column names from the codebook
names <- read.csv(here("data/names.csv"), header = F, fileEncoding = "UTF-8-BOM")
names(DT) <- as.vector(names[,1])

##### CREATE NEW VARIABLES

# add conditional columns
DT[, nofriend01 := ifelse(friend == 1, 1, 0)]
DT[, nokamrad01 := ifelse(kamrad == 1, 1, 0)]
DT[, alone01all := ifelse(alone16 %in% 3 | !is.na(alone79) & alone79 > 3 , 1, 0)]
DT[, vict01 := ifelse(vict > 2, 1, 0)]
DT[, victpre01 := ifelse(victpre > 2, 1, 0)]
DT[, adhome01 := ifelse(adhome == 1, 1, 0)]
DT[, cdhome01 := ifelse(chhome == 1, 1, 0)]
DT[, worry901 := ifelse(worry9 == 2, 1, 0)]
DT[, likesch01 := ifelse(likesch > 2, 1, 0)]
DT[, morearg01 := ifelse(ad2 == 2, 1, 0)]
suppmen <- c('suppmen1', 'suppmen2', 'suppmen3', 'suppmen4')
DT[, suppneed01 := ifelse( rowSums(DT[,c('suppmen1', 'suppmen2', 'suppmen3', 'suppmen4')]) == 0, 0, 1)]
DT[, noteacher01 := ifelse(teacher3 < 2, 1, 0)] 
DT[, bullstopped := ifelse(victpre01 == 1 & vict01 == 0, 1,0)]

# count scales, removing NAs
DT$diff <- rowMeans(DT[, c(17:22)], na.rm = T)
DT$diffsub <- rowMeans(DT[, c(35:40)], na.rm = T)
DT$mental <- rowMeans(DT[, names(DT) %like% "mental", with = F], na.rm = T)
DT$anxiety <- rowMeans(DT[, c(53:59)], na.rm = T)
DT$worry <- rowMeans(DT[, c(71:81)], na.rm = T)
DT$teacher <- rowMeans(DT[, c(82:85)], na.rm = T)
DT$pos <- rowMeans(DT[, c(106:112)], na.rm = T)
DT$peers <- rowMeans(DT[, c(90:93)], na.rm = T)
DT$depr <- rowMeans(DT[, c(61:66)], na.rm = T)
DT$stress <- rowMeans(DT[, c(71,72,73,79,80,81)], na.rm = T)

# create timeusec, variable telling how much time used for school differs from the age/class average
grpmeans <- DT %>% group_by(class) %>% summarize(mean(timeuse))
DT <- inner_join(DT, grpmeans, join_by = class)
DT$timeusec <- DT$timeuse - DT$`mean(timeuse)`
DT$`mean(timeuse)` <- NULL

# save the dataset
write.csv(DT, here("data/processed/korona2020a1.csv"), row.names = FALSE, fileEncoding = 'UTF-8')
