rm(list=ls())
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))

# china's public diplomacy dataset v2
# https://www.aiddata.org/data/chinas-public-diplomacy-dashboard-dataset-version-2

chi_dip = readr::read_csv(paste0(pathIn, "ChinesePublicDiplomacy.csv"))
names(chi_dip)

chi_dip = chi_dip %>%
  mutate(cname2 = cname("China")) %>%
  rename(cname1 = receiving_country) %>%
  select(cname1, cname2, year, everything())

chi_dip$cname1 = countrycode(chi_dip$cname1, 'country.name', 'country.name')
chi_dip$cname2 = countrycode(chi_dip$cname2, 'country.name', 'country.name')

save(chi_dip, file = paste0(pathIn, "china_dip.rda"))
