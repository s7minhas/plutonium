rm(list=ls())
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))

## inbound internationally mobile students by country of origin
## http://data.uis.unesco.org/

inboundStudent_US = read_csv("inboundStudents_FromUS.csv")
inboundStudent_China = read_csv("inboundStudents_Fromchina.csv")
#enrolment_US = read_csv("enrolmentIntStudents_US.csv") from OECD

unique(inboundStudent_China$TIME)
unique(inboundStudent_US$TIME)
#unique(enrolment_US$YEAR)

inboundStudent_US = inboundStudent_US %>%
  select(Country, Time, Value) %>%
  rename(cname = Country,
         year = Time,
         in_student = Value)

inboundStudent_China = inboundStudent_China %>%
  select(Country, Time, Value) %>%
  rename(cname = Country,
         year = Time,
         in_student = Value)
  

# fix czechia
inboundStudent_US$cname[inboundStudent_US$cname=='Czechia'] = "CZECH REPUBLIC"
inboundStudent_China$cname[inboundStudent_China$cname=='Czechia'] = "CZECH REPUBLIC"

# fix congo
inboundStudent_US$cname[
  inboundStudent_US$cname=='Congo - Kinshasa'] = "CONGO, THE DEMOCRATIC REPUBLIC OF"

inboundStudent_China$cname[
  inboundStudent_China$cname=='Congo - Kinshasa'] = "CONGO, THE DEMOCRATIC REPUBLIC OF"

# get country names right and add country code
inboundStudent_US$cname1 = countrycode(inboundStudent_US$cname, 'country.name', 'country.name')
inboundStudent_US$ccode = countrycode(inboundStudent_US$cname, 'country.name', 'cown')

inboundStudent_China$cname1 = countrycode(inboundStudent_China$cname, 'country.name', 'country.name')
inboundStudent_China$ccode = countrycode(inboundStudent_China$cname, 'country.name', 'cown')

save(inboundStudent_US, file = "inboundStudent_US.rda")
save(inboundStudent_China, file = "inboundStudent_China.rda")

