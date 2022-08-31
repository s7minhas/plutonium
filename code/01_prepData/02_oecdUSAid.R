rm(list=ls())
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))

library(readr)
library(dplyr)

######################
# Aid (ODA) disbursements to countries and regions [DAC2a] from US
# aidD <- read_csv(paste0(pathIn, "/aidDisbursements.csv"))

# Aid (ODA) commitments to countries and regions [DAC3a] from US
aidC <- read_csv(paste0(pathIn, "/aidCommitments.csv"))
######################


# aidD <- aidD %>%
#   select(Recipient, Year, Value) %>%
#   rename(disbursement = Value) 

aidC <- aidC %>%
  select(Recipient, Year, Value) %>%
  rename(aidCommitment = Value,
         cname1 = Recipient,
         year = Year) %>%
  mutate(cname2 = "UNITED STATES")


# aidD$Recipient <- cname(aidD$Recipient)
# aidD <- na.omit(aidD)
aidC$cname1 <- cname(aidC$cname1)
aidC$cname2 <- cname(aidC$cname2)

aidC$cowc1 <- countrycode(aidC$cname1, 'country.name', 'cowc')
aidC$cowc2 <- countrycode(aidC$cname2, 'country.name', 'cowc')

aidC <- na.omit(aidC)

aidC <- aidC %>% dplyr::select(cname1, cowc1, cname2, cowc2, year, aidCommitment)

save(aidC, file = paste0(pathIn, "USaidC.rda"))






