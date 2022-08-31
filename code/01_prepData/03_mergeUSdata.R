rm(list=ls())
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))

# load panel data
load(paste0(pathIn, "panel.rda"))

# extend the panel.rda to include additional years 2020, 2021, 2022
yrs = 2020:2022

slice = panel[panel$year==2019,]

toAdd = do.call('rbind', 
                lapply( yrs, function(yr){
                  slice$year = yr
                  slice$ccodeYear = with(slice, paste(ccode, year))
                  slice$cnameYear = with(slice, paste(cname, year))
                  return(slice)
                }  )
)

panel = rbind(panel, toAdd)


##### AID DATA
load(paste0(pathIn, "USaidC.rda"))
# create ccdoe var
aidC$ccode = countrycode(aidC$cname1, 'country.name', 'cown')

# merge aid into panel
panel_aid = left_join(panel, aidC, by= c('ccode', 'year'))

panel_aid = panel_aid %>%
  select(cname, ccode, year, ccodeYear, cnameYear, aidCommitment)

panel_aid$cname1 = cname(panel_aid$cname)
panel_aid$cname2 = "UNITED STATES"

panel_aid = panel_aid %>%
  select(cname1, cname2, ccode, year, aidCommitment)

## DESTA

# load desta
load(paste0(pathIn, 'desta.rda'))

# filter to include only US pairs
desta_us = desta %>%
  filter(cname2 %in% "UNITED STATES")

# create ccode for merging
desta_us$ccode = countrycode(desta_us$cname1, 'country.name', 'cown')

desta_us = desta_us %>%
  select(cname1, cname2, ccode, year, id, ptaCnt, pta)

# merge panel_aid with desta
panel_aidDesta = left_join(panel_aid, desta_us, by = c("cname1", "cname2", "ccode", "year"))


# filter to include only year 2000-2022
panel_aidDesta = panel_aidDesta %>%
  filter(year %in% c(2000:2022)) %>%
  arrange(cname1)

# desta = dest(dest$cname = = "china")
# 
# china$ptacount = desta$ptacnt(match(china$cname2, cname$cname2))

# polity1



######################
# imf for trade vars

load(paste0(pathIn, "imfData.rda"))

# only US pairs
imf_us = imfData %>%
  filter(cname2 %in% "UNITED STATES")

# create ccode
imf_us$ccode = countrycode(imf_us$cname1, 'country.name', 'cown')

imf_us = imf_us %>%
  select(cname1, cname2, ccode, year, tradeBal, importsCIF,
         exportsCIF, importsFOB, exportsFOB)

# merge imf data
panel_aidDesta_imf = left_join(panel_aidDesta, imf_us, by = c("cname1", "cname2", "ccode", "year"))


######################
# inbound international students

load(paste0(pathIn, "inboundStudent_US.rda"))

inboundStudent_US$cname2 = "UNITED STATES"

inboundStudent_US = inboundStudent_US %>%
  select(cname1, cname2, ccode, year, in_student)

panel_aidDesta_imf_student = left_join(panel_aidDesta_imf, inboundStudent_US, by = c("cname1", "cname2", "ccode", "year"))


######################
# polity

library(psData)
polity <- PolityGet(vars = "polity2")

polity = polity %>%
  filter(year %in% c(2000:2015)) # data exists until 2015


names(polity)

polity$cname1 = cname(polity$standardized_country)

polity = polity %>%
  select(cname1, year, polity2) %>%
  rename(polity = polity2)

US_data = left_join(panel_aidDesta_imf_student, polity, by = c('cname1', 'year'))

save(US_data, file = paste0(pathIn, 'US_data.rda'))

# desta = dest(dest$cname = = "china")
# 
# china$ptacount = desta$ptacnt(match(china$cname2, cname$cname2))

# polity1



