rm(list=ls())
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))

# chinese aid from aiddata
# https://www.aiddata.org/data/aiddatas-global-chinese-development-finance-dataset-version-2-0

china_aid = readxl::read_excel(paste0(pathIn, 'AidDatasGlobalChineseDevelopmentFinanceDataset_v2.0.xlsx'),
                               sheet = 5)
names(china_aid)

china_aid = china_aid %>%
  select(`Financier Country`, Recipient, `Commitment Year`, Intent, `Sector Name`, `Amount (Constant USD2017)`, `Amount (Nominal)`) %>%
  rename(cname1 = Recipient,
         cname2 = `Financier Country`,
         year = `Commitment Year`,
         intent = Intent,
         aid_sector = `Sector Name`,
         aid_constant = `Amount (Constant USD2017)`,
         aid_nominal = `Amount (Nominal)`)


china_aid$cname1 = countrycode(china_aid$cname1, 'country.name', 'country.name')
china_aid$cname2 = countrycode(china_aid$cname2, 'country.name', 'country.name')


save(china_aid, file = paste0(pathIn, "china_aid.rda"))

