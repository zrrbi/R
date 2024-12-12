# cg

library(readxl)
library(tidyverse)

setwd("")
closed <- read_excel("")
revoked <- read_excel("")
# Revoked/Discontinued FFLS 2021-22." Add the month and year of revocation if applicable.
revoked <- revoked[revoked$State == 'IL', ]
# Date Inspection Closed = License revoked? (month/year) 

# All IRs as of Aug
# Date Produced = Inspection Report
irs <- read_excel("IRs.xlsx")
closed$APP_LICENSE_NAME <- gsub("[^A-Za-z0-9 ]", "", closed$APP_LICENSE_NAME)
closed$BUSINESS_NAME <- gsub("[^A-Za-z0-9 ]", "", closed$BUSINESS_NAME)
irs$`Operator Name` <- toupper(gsub("[^A-Za-z0-9 ]", "", irs$`Operator Name`))

irs <- irs %>%
  filter(State %in% c('IL', 'Illinois'))

library(dplyr)

irs_matched1 <- irs %>%
  filter(`Operator Name` %in% closed$APP_LICENSE_NAME)

irs_matched2 <- irs %>%
  filter(`Operator Name` %in% closed$BUSINESS_NAME)

irs_matched2_not_in_matched1 <- anti_join(irs_matched2, irs_matched1, by = 'Operator Name')

irs_matched3 <- bind_rows(irs_matched1, irs_matched2)

# Update
closed$`Inspection Report? (year)` <- ifelse(
  (closed$APP_LICENSE_NAME %in% irs_matched3$`Operator Name` | closed$BUSINESS_NAME %in% irs_matched3$`Operator Name`) & 
    is.na(closed$`Inspection Report? (year)`),
  irs_matched3$`Date Produced`,
  closed$`Inspection Report? (year)`
)

# Revocation Violations
rev <- read_excel("Revocation.xlsx")
rev$`Dealer Name` <- toupper(gsub("[^A-Za-z0-9 ]", "", rev$`Dealer Name`))

rev_matched <- rev %>%
  filter(`Dealer Name` %in% closed$APP_LICENSE_NAME)

# 手动
closed$`Revocation violations`[closed$APP_LICENSE_NAME == "TOP OF THE LINE LLC"] <- '8/1/2022'

# Cross check against the lists of dealers named in cases, 
# tabs "CG courts docs FFLs" and "Defendant FFLs." Add links to court docs. 

# CG
cg <- read.csv ("CG.csv")
cg <- cg %>%
  filter(State %in% c('IL'))
rows_to_keep <- c(1, 6, 11, 12, 13, 20)
cg <- cg[rows_to_keep, ]

# FFL
ffl <- read.csv("FFL.csv")
ffl <- ffl %>%
  filter(State %in% c('IL'))


# Cross check against the lists of dealers with DLs in the tabs
# "Demand Letter 2 FFLs" and "DL2s CY23." Add years when DL2 received. 

# DL FFL
dlffl <- read_excel("DL FFL.xlsx")
dlffl <- dlffl %>%
  filter(State1 %in% c('IL'))

dlffl$`Name` <- toupper(gsub("[^A-Za-z0-9 ]", "", dlffl$`Name`))
dlffl$`Trade Name` <- toupper(gsub("[^A-Za-z0-9 ]", "", dlffl$`Trade Name`))

dlffl_matched1 <- dlffl %>%
  filter(`Name` %in% closed$APP_LICENSE_NAME)
dlffl_matched2 <- dlffl %>%
  filter(`Trade Name` %in% closed$APP_LICENSE_NAME)

dlffl_matched3 <- dlffl %>%
  filter(`Name` %in% closed$BUSINESS_NAME)
dlffl_matched4 <- dlffl %>%
  filter(`Trade Name` %in% closed$BUSINESS_NAME)

dlffl_matched5 <- full_join(dlffl_matched1, dlffl_matched4, by = 'Name')

# 手动 #

dlffl_matched6 <- dlffl_matched5 %>%
  select(Name, `Trade Name.x`, `DL2 Date.x`)

# DL2
dl2 <- read_excel("DL2.xlsx")
dl2 <- dl2 %>%
  filter(State %in% c('IL'))

dl2$`Name` <- toupper(gsub("[^A-Za-z0-9 ]", "", dl2$`Name`))
dl2$`Trade Name` <- toupper(gsub("[^A-Za-z0-9 ]", "", dl2$`Trade Name`))

dl2_matched1 <- dl2 %>%
  filter(`Name` %in% closed$APP_LICENSE_NAME)
dl2_matched2 <- dl2 %>%
  filter(`Trade Name` %in% closed$BUSINESS_NAME)
dl2_matched3 <- full_join(dl2_matched1, dl2_matched2, by = 'Name')

library(writexl)
write.csv(closed, "output.csv")