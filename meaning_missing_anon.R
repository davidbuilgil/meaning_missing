
#########################################
#
# The Meaning of Missing: 
# The Hidden Power of Police Data Recording Practices in Rape Cases
#
# Authors: Gordana Uzelac, David Buil-Gil, Katrin Hohl, Jo Lovett
# Description: Replication code for the analysis of missing data 
#              across victim, suspect, and case characteristics.
#
# Version: Public / anonymised
# Requirements: R >= 4.3.0
# Date: 2025-08-01
#
#########################################

options(scipen=999)

rm(list = ls())
setwd("PATH/TO/PROJECT/FOLDER")

#load packages
library(stringr)
library(fastDummies)
library(viridis)
library(hrbrthemes)
library(rstatix)
library(ggplot2)
library(ggpubr)
library(sf)
library(ggalluvial)
library(dplyr)

#load data
PF_B <- read.csv("path/to/dataset_B.csv")
PF_A <- read.csv("path/to/dataset_A.csv")
PF_C <- read.csv("path/to/dataset_C.csv")
PF_E <- read.csv("path/to/dataset_E.csv")
PF_D <- read.csv("path/to/dataset_D.csv")

#remove duplicates
PF_B <- PF_B %>%
  filter(Disposal.Code != 'C3' & Disposal.Code != 'C4')
PF_A <- PF_A %>%
  filter(Duplicate.CrimeURN. == "Original")
PF_C <- PF_C %>%
  filter(Duplicate.crime.ref.number..0.original. == 0)
PF_E <- PF_E %>%
  filter(Duplicate.Crime.Vic.Sus...1.yes. != 1)
PF_D <- PF_D %>%
  filter(Duplicate.crime.ref.no. == "No")

#select cases reported between 2018 and 2020
PF_B <- PF_B %>%
  mutate(year.report = format(as.Date(Created.Date, format="%d/%m/%Y"),"%Y")) %>%
  filter(year.report == 2018 | year.report == 2019 | year.report == 2020)
PF_A <- PF_A %>%
  filter(Reported.year == 2018 | Reported.year == 2019 | Reported.year == 2020)
PF_C <- PF_C %>%
  filter(Recorded.year == 2018 | Recorded.year == 2019 | Recorded.year == 2020)
PF_E <- PF_E %>%
  filter(Reported.Year == 2018 | Reported.Year == 2019 | Reported.Year == 2020)
PF_D <- PF_D %>%
  filter(Reported.Year == 2018 | Reported.Year == 2019 | Reported.Year == 2020)

#select rapes only
PF_B <- PF_B %>%
  filter(str_detect(HO.Text, "Rape"))
PF_A <- PF_A %>%
  filter(Minor.text...update == "Rape")
PF_C <- PF_C %>%
  filter(Offence.Sub.Group == "RAPE")
PF_E <- PF_E %>%
  filter(Rape.or.SSO. == "Rape")
PF_D <- PF_D %>%
  filter(ONS_GROUP == "Rape")

#recode outcomes to following categories:
##Charged
##Evidential difficulties: Attributed to victim
##Evidential difficulties: Due to investigation
##Prosecution prevented or not in the public interest
##Outcome unknown
##No crime
##Transferred to another agency
##Other

PF_B <- PF_B %>%
  mutate(outcome = recode(Disposal.Code,
                          ' ' = 'Other',
                          '1' = 'Charged',
                          '10' = 'Prosecution prevented',
                          '11' = 'Prosecution prevented',
                          '12' = 'Prosecution prevented',
                          '13' = 'Prosecution prevented',
                          '14' = 'Attributed to victim',
                          '15' = 'Due to investigation',
                          '16' = 'Attributed to victim',
                          '18' = 'Due to investigation',
                          '2'  = 'Other',
                          '20' = 'Transferred to another agency',
                          '21' = 'Prosecution prevented',
                          '22' = 'Prosecution prevented',
                          '3'  = 'Other',
                          '5'  = 'Prosecution prevented',
                          '8'  = 'Other',
                          '9'  = 'Prosecution prevented',
                          'C1' = 'Transferred to another agency',
                          'C2' = 'No crime',
                          'C5' = 'No crime',
                          'N1001' = 'No crime',
                          'N1003' = 'Transferred to another agency'),
         outcome = ifelse(outcome == "", "Other", outcome))

PF_A <- PF_A %>%
  mutate(outcome = recode(Outcome,
                          '0' = 'Outcome unknown',
                          '1' = 'Charged',
                          '2' = 'Other',
                          '3' = 'Other',
                          '5' = 'Prosecution prevented',
                          '8' = 'Other',
                          '9' = 'Other',
                          '10' = 'Prosecution prevented',
                          '11' = 'Prosecution prevented',
                          '12' = 'Prosecution prevented',
                          '13' = 'Prosecution prevented',
                          '14' = 'Attributed to victim',
                          '15' = 'Due to investigation',
                          '16' = 'Attributed to victim',
                          '17' = 'Prosecution prevented',
                          '18' = 'Due to investigation',
                          '20' = 'Transferred to another agency',
                          '21' = 'Prosecution prevented',
                          '66' = 'Prosecution prevented',
                          '99' = 'No crime',
                          'Blank' = 'Outcome unknown',
                          '1A' = 'Charged',
                          '2A' = 'Charged',
                          '3A' = 'Charged'))

PF_C <- PF_C %>%
  mutate(outcome = recode(X...Outcome.grouped,
                          'a. Missing, unknown, not recorded, outcome pending' = 'Outcome unknown',
                          'b. Charged/summonsed (OC1)' = 'Charged',
                          'c. Charged or cautioned for alternate offence (1d, 1e, 2a, 3a)' = 'Charged',
                          'd. Other sanctions (OC2/3/8)' = 'Other',
                          'e. Prosecution prevented or not in the public interest (OC5/9/10/11/12/13/17/21/22)' = 'Prosecution prevented',
                          'f. Evidential difficulties: suspect not identified; victim does not support further action (OC14)' = 'Attributed to victim',
                          'g. Evidential difficulties: suspect identified; victim supports action (OC15)' = 'Due to investigation',
                          'h. Evidential difficulties: suspect identified; victim does not support further action (OC16)' = 'Attributed to victim',
                          'i. Investigation complete – no suspect identified (OC18)' = 'Due to investigation',
                          'i. Investigation complete � no suspect identified (OC18)' = 'Due to investigation',
                          'i. Investigation complete \x96 no suspect identified (OC18)' = 'Due to investigation',
                          'i. Investigation complete ????" no suspect identified (OC18)' = 'Due to investigation',
                          'j. Transferred to External Agency (20)' = 'Transferred to another agency',
                          'k. No crime' = 'No crime'))

PF_E <- PF_E %>%
  mutate(outcome = recode(X.Outcome.recoded,
                          '#N/A' = 'Outcome unknown',
                          'a. Missing, unknown, not recorded, outcome pending' = 'Outcome unknown',
                          'b. Charged/summonsed (OC1)' = 'Charged',
                          'd. Other sanctions (OC2/3/8)' = 'Other',
                          'e. Prosecution prevented or not in the public interest (OC5/9/10/11/12/13/17/21/22)' = 'Prosecution prevented',
                          'f. Evidential difficulties: suspect not identified; victim does not support further action (OC14)' = 'Attributed to victim',
                          'g. Evidential difficulties: suspect identified; victim supports action (OC15)' = 'Due to investigation',
                          'h. Evidential difficulties: suspect identified; victim does not support further action (OC16)' = 'Attributed to victim',
                          'i. Investigation complete - no suspect identified (OC18)' = 'Due to investigation',
                          'i. Investigation complete \x96 no suspect identified (OC18)' = 'Due to investigation',
                          'j. Transferred to External Agency (OC20)' = 'Transferred to another agency',
                          'l. Filed as incident/CRI' = 'No crime',
                          'm. Taken into consideration (OC4)' = 'Other'))

PF_D <- PF_D %>%
  mutate(outcome = recode(X..Outcome.Type...amended...recoded,
                          'a. Missing, unknown, not recorded, outcome pending' = 'Outcome unknown',
                          'b. Charged/summonsed (OC1)' = 'Charged',
                          'c. Charged or cautioned for alternate offence (1a, 2a, 3a)' = 'Charged',
                          'd. Other sanctions (OC2/3/6/8)' = 'Other',
                          'e. Prosecution prevented or not in the public interest (OC5/9/10/11/12/13/17/21/22)' = 'Prosecution prevented',
                          'f. Evidential difficulties: suspect not identified; victim does not support further action (OC14)' = 'Attributed to victim',
                          'g. Evidential difficulties: suspect identified; victim supports action (OC15)' = 'Due to investigation',
                          'h. Evidential difficulties: suspect identified; victim does not support further action (OC16)' = 'Attributed to victim',
                          'i. Investigation complete - no suspect identified (OC18)' = 'Due to investigation',
                          'i. Investigation complete \x96 no suspect identified (OC18)' = 'Due to investigation',
                          'j. Transferred to External Agency (OC20, C1)' = 'Transferred to another agency',
                          'k. Admin (OC-C3/4)' = 'Other',
                          'l. No crime (OC-C2)' = 'No crime'))

#remove other outcomes from databases
#PF_B <- PF_B %>%
#  filter()
PF_A <- PF_A %>%
  filter(outcome != 'Outcome unknown')
PF_C <- PF_C %>%
  filter(outcome != 'Outcome unknown')
PF_E <- PF_E %>%
  filter(outcome != 'Outcome unknown')
PF_D <- PF_D %>%
  filter(outcome != 'Outcome unknown')

#add PFA variable in dataset
PF_B <- PF_B %>%
  mutate(PFA = 'Police Force B')
PF_A <- PF_A %>%
  mutate(PFA = 'Police Force A')
PF_C <- PF_C %>%
  mutate(PFA = 'Police Force C')
PF_E <- PF_E %>%
  mutate(PFA = 'Police Force E')
PF_D <- PF_D %>%
  mutate(PFA = 'Police Force D')

#create list of variables to keep
vars.keep <- as.list(c('outcome', 'PFA'))

#recode demographic characterstics of victim
PF_B <- PF_B %>%
  mutate(sex.victim = recode(Gender,
                             'Female / Transgender Female' = 'female',
                             'Male / Transgender Male' = 'male',
                             'None recorded' = 'unknown',
                             'Not Specified' = 'unknown',
                             'Decoy' = 'unknown'),
         age.victim = na_if(Age..Diff..between.Date.from.and.date.of.birth., 'Decoy'),
         age.victim = na_if(age.victim, 'None recorded'),         
         age.victim = as.numeric(age.victim),
         ethnic.victim = recode(Ethnic.description,
                                'Asian' = 'asian',
                                'Black' = 'black',
                                'Chinese, Japanese or South East Asian' = 'asian',
                                'Decoy' = 'unknown',
                                'Middle Eastern' = 'other',
                                'None recorded' = 'unknown',
                                'Unknown' = 'unknown',
                                'White - North European' = 'white',
                                'White - South European' = 'white')
  )

PF_A <- PF_A %>%
  mutate(sex.victim = recode(Sex,
                             'F' = 'female',
                             'M' = 'male',
                             'NULL' = 'unknown',
                             'U' = 'unknown',
                             ' ' = 'unknown'),
         sex.victim = ifelse(sex.victim == "", 'unknown', sex.victim),
         age.victim = na_if(Vic.age.at.offence...integer, '#VALUE!'),
         age.victim = as.numeric(age.victim),
         age.victim = ifelse(age.victim < 0, NA, age.victim),
         ethnic.victim = recode(Victim.ethnicity..IC.,
                                '#N/A' = 'unknown',
                                '0' = 'unknown',
                                '1' = 'white',
                                '2' = 'white',
                                '3' = 'black',
                                '4' = 'asian',
                                '5' = 'asian',
                                '6' = 'other',
                                ' ' = 'unknown'))

PF_C <- PF_C %>%
  mutate(sex.victim = recode(X..Offence.code...victim.gender,
                             'Female' = 'female',
                             'Male' = 'male'),
         age.victim = na_if(Vic...Age.at.Offence.Committed, '#VALUE!'),
         age.victim = as.numeric(age.victim),
         ethnic.victim = recode(X..Vic.ethnic.appearance.grouped,
                                '99 - Other' = 'other',
                                'IC1 - White N European' = 'white',
                                'IC2 - White S European' = 'white',
                                'IC3' = 'black',
                                'IC3 - Black' = 'black',
                                'IC4 - Asian' = 'asian',
                                'IC5 - Oriental' = 'asian',
                                'IC6 - Middle Eastern' = 'other',
                                'Missing/not known' = 'unknown'))

PF_E <- PF_E %>%
  mutate(sex.victim = recode(Victim...Gender,
                             ' ' = 'unknown',
                             'Female' = 'female',
                             'Indeterminate' = 'unknown',
                             'Male' = 'male',
                             'Unknown' = 'unknown'),
         sex.victim = ifelse(sex.victim == "", 'unknown', sex.victim),
         age.victim = na_if(Victim...Age.on.Created.date, ''),
         age.victim = as.numeric(age.victim),
         ethnic.victim = recode(Victim...Ethnicity,
                                '#MULTIVALUE' = 'other',
                                'IC1 - White' = 'white',
                                'IC2 - Dark European' = 'white',
                                'IC3 - African Caribbean' = 'black',
                                'IC4 - Asian' = 'asian',
                                'IC5 - Chinese, Japanese or other Southeast Asian' = 'asian',
                                'IC6 - Arabic' = 'other',
                                'Other' = 'other',
                                'Unknown' = 'unknown',
                                ' ' = 'unknown'),
         ethnic.victim = ifelse(ethnic.victim != "asian" &
                                  ethnic.victim != "black" &
                                  ethnic.victim != "other" &
                                  ethnic.victim != "unknown" &
                                  ethnic.victim != "white", 
                                "unknown", 
                                ethnic.victim))

PF_D <- PF_D %>%
  mutate(sex.victim = recode(Vic...sex,
                             '#N/A' = 'unknown',
                             'Female' = 'female',
                             'Indeterminate' = 'unknown',
                             'Male' = 'male',
                             'Not recorded' = 'unknown',
                             'NULL' = 'unknown'),
         age.victim = na_if(Vic...age.at.committed.from.date, '#N/A'),
         age.victim = na_if(Vic...age.at.committed.from.date, 'NULL'),
         age.victim = as.numeric(age.victim),
         ethnic.victim = recode(X..Vic...police.defined.ethnicity...recoded,
                                '#N/A' = 'unknown',
                                '0. Missing/not known' = 'unknown',
                                '1. IC1 - White N European' = 'white',
                                '2. IC2 - White S European' = 'white',
                                '3. IC3 - Black' = 'black',
                                '4. IC4 - Asian' = 'asian',
                                '5. IC5 - Oriental' = 'asian',
                                '6. IC6 - Middle Eastern' = 'other',
                                '7. No identified victim' = 'unknown'))

vars.keep <- vars.keep %>%
  append(c('sex.victim', 'age.victim', 'ethnic.victim'))

#recode demographic characterstics of suspect
PF_B <- PF_B %>%
  mutate(sex.suspect = recode(Gender.1,
                              'Female / Transgender Female' = 'female',
                              'Male / Transgender Male' = 'male',
                              'None recorded' = 'unknown',
                              'Not Specified' = 'unknown',
                              'Decoy' = 'unknown',
                              ' ' = 'unknown'),
         sex.suspect = ifelse(sex.suspect == "", 'unknown', sex.suspect),
         age.suspect = na_if(Age..Diff..between.Date.from.and.date.of.birth..1, ''),
         age.suspect = na_if(age.suspect, 'Not recorded'),         
         age.suspect = as.numeric(age.suspect),
         ethnic.suspect = recode(Ethnic.description.1,
                                 'Asian' = 'asian',
                                 'Black' = 'black',
                                 'Chinese, Japanese or South East Asian' = 'asian',
                                 'Not recorded' = 'unknown',
                                 'Middle Eastern' = 'other',
                                 'None recorded' = 'unknown',
                                 'Unknown' = 'unknown',
                                 'White - North European' = 'white',
                                 'White - South European' = 'white',
                                 ' ' = 'unknown'),
         ethnic.suspect = ifelse(ethnic.suspect == "", 'unknown', ethnic.suspect))

PF_A <- PF_A %>%
  mutate(sex.suspect = recode(SuspectSex2,
                              'F' = 'female',
                              'M' = 'male',
                              'NULL' = 'unknown',
                              'U' = 'unknown',
                              ' ' = 'unknown'),
         sex.suspect = ifelse(sex.suspect == "", 'unknown', sex.suspect),
         age.suspect = na_if(Suspect.age.at.committed.from.date..FINAL..integer, '#VALUE!'),
         age.suspect = na_if(age.suspect, '#N/A'),
         age.suspect = as.numeric(age.suspect),
         age.suspect = ifelse(age.suspect < 0, NA, age.suspect),
         age.suspect = ifelse(age.suspect > 99, NA, age.suspect),
         ethnic.suspect = recode(Suspect.ethnicity,
                                 '#N/A' = 'unknown',
                                 '0' = 'unknown',
                                 '1' = 'white',
                                 '2' = 'white',
                                 '3' = 'black',
                                 '4' = 'asian',
                                 '5' = 'asian',
                                 '6' = 'other',
                                 ' ' = 'unknown'))

PF_C <- PF_C %>%
  mutate(sex.suspect = recode(Susp...gender,
                              '-' = 'unknown',
                              '#N/A' = 'unknown',
                              'Female' = 'female',
                              'Male' = 'male',
                              'Unknown' = 'unknown',
                              'Unspecified' = 'unknown'),
         age.suspect = na_if(Susp...age.at.offence.committed, '#N/A'),
         age.suspect = na_if(Susp...age.at.offence.committed, '-'),
         age.suspect = as.numeric(age.suspect),
         ethnic.suspect = recode(X..Susp.ethnic.appearance.grouped,
                                 '#N/A' = 'unknown',
                                 '99 - Other' = 'other',
                                 'IC1 - White N European' = 'white',
                                 'IC2 - White S European' = 'white',
                                 'IC3' = 'black',
                                 'IC3 - Black' = 'black',
                                 'IC4 - Asian' = 'asian',
                                 'IC5 - Oriental' = 'asian',
                                 'IC6 - Middle Eastern' = 'other',
                                 'Missing/not known' = 'unknown'))

PF_E <- PF_E %>%
  mutate(sex.suspect = recode(Offender.Gender,
                              ' ' = 'unknown',
                              'Female' = 'female',
                              'Male' = 'male',
                              'Unknown' = 'unknown'),
         sex.suspect = ifelse(sex.suspect == "", 'unknown', sex.suspect),
         age.suspect = as.numeric(Offender.Age.on.Occurrence.Created.Date),
         age.suspect = ifelse(age.suspect < 0, NA, age.suspect),
         ethnic.suspect = recode(Offender...Merged.Latest.Ethnicity.IC.Code.and.Desc,
                                 'IC1 - White' = 'white',
                                 'IC2 - Dark European' = 'white',
                                 'IC3 - African Caribbean' = 'black',
                                 'IC4 - Asian' = 'asian',
                                 'IC5 - Chinese, Japanese or other Southeast Asian' = 'asian',
                                 'IC6 - Arabic' = 'other',
                                 'Other' = 'other',
                                 'Unknown' = 'unknown'))

PF_D <- PF_D %>%
  mutate(sex.suspect = recode(Sus...gender,
                              'Female' = 'female',
                              'Male' = 'male',
                              'Not recorded' = 'unknown',
                              'NULL' = 'unknown',
                              'Unknown' = 'unknown',
                              'Indeterminate' = 'unknown'),
         age.suspect = na_if(Sus...age.at.committed.from.date, 'NULL'),
         age.suspect = as.numeric(age.suspect),
         ethnic.suspect = recode(X..Sus...officer.perceived.ethnicity...recoded,
                                 '0. Missing/not known' = 'unknown',
                                 '1. IC1 - White N European' = 'white',
                                 '2. IC2 - White S European' = 'white',
                                 '3. IC3 - Black' = 'black',
                                 '4. IC4 - Asian' = 'asian',
                                 '5. IC5 - Oriental' = 'asian',
                                 '6. IC6 - Middle Eastern' = 'other',
                                 '7. No identified suspect' = 'unknown'))

vars.keep <- vars.keep %>%
  append(c('sex.suspect', 'age.suspect', 'ethnic.suspect'))

#recode victim-suspect relationship
PF_B <- PF_B %>%
  mutate(relation = recode(Relationship,
                           'Associate' = 'acquaintance, friend',
                           'Criminal Associate' = 'acquaintance, friend',
                           'Family' = 'family',
                           'Guardian - Ward' = 'family',
                           'In Dispute with' = 'unknown',
                           'None recorded' = 'unknown',
                           'Other' = 'unknown',
                           'Person of Trust' = 'acquaintance, friend',
                           'Professional' = 'acquaintance, friend',
                           'Relationship' = 'intimate',
                           'Relative' = 'family',
                           'Stranger' = 'stranger',
                           'Unknown' = 'unknown',
                           'Uses Particulars Of' = 'acquaintance, friend',
                           ' ' = 'unknown'),
         relation = ifelse(relation == "", 'unknown', relation))

PF_A <- PF_A %>%
  mutate(relation = recode(Relationship.from.SexOffs.Screen.data...renamed,
                           '1. Stranger 1' = 'stranger',
                           '2. Stranger 2' = 'stranger',
                           '3. Familial' = 'family',
                           '4. Friend/acquaintance' = 'acquaintance, friend',
                           '5. Intimate/previous intimate' = 'intimate',
                           '6. Not recorded or unknown' = 'unknown'))

PF_C <- PF_C %>%
  mutate(relation = recode(X...Relationship.type...FINAL...RAPES.2018.20.only,
                           "0. 'None'" = 'stranger', #check
                           '1. Stranger' = 'stranger',
                           '2. Familial' = 'family',
                           '3. Friend/acquaintance' = 'acquaintance, friend',
                           '4. Intimate/former intimate' = 'intimate',
                           '5. Other' = 'other',
                           '6. Not recorded or unknown' = 'unknown'))

PF_E <- PF_E %>%
  mutate(relation = recode(X.Current.Offender.Victim.Relationship...recoded,
                           '1. Stranger' = 'stranger',
                           '2. Familial' = 'family',
                           '4. Acquaintance' = 'acquaintance, friend',
                           '5.\xa0Intimate/previous intimate' = 'intimate',
                           '6. Not recorded/unknown' = 'unknown',
                           '7. Victimless/state crime' = 'other'),
         relation = ifelse(grepl('Intimate', relation), 'intimate', relation))

PF_D <- PF_D %>%
  mutate(relation = recode(Victim.Suspect.Relationship,
                           'Acquaintance' = 'acquaintance, friend',
                           'Boyfriend' = 'intimate',
                           'Carer of' = 'family',
                           'Child of' = 'family',
                           'Colleague' = 'acquaintance, friend',
                           'Employer/employee' = 'acquaintance, friend',
                           'Ex partner' = 'intimate',
                           'Family' = 'family',
                           'Girlfriend' = 'intimate',
                           'Known by sight' = 'stranger', #check
                           'Neighbour' = 'acquaintance, friend',
                           'Not seen by victim' = 'unknown',
                           'NULL' = 'unknown',
                           'Parent of' = 'family',
                           'Sibling of' = 'family',
                           'Spouse/partner' = 'intimate',
                           'Stranger' = 'stranger',
                           'Teacher/pupil' = 'other', #check
                           'Victim refuses to identify' = 'unknown'))

vars.keep <- vars.keep %>%
  append(c('relation'))

#recode offence characteristics
PF_B <- PF_B %>%
  mutate(Reported.Date = na_if(Reported.Date, "No incident linked"),
         Reported.Date = as.Date(Reported.Date, "%d/%m/%y"),
         Incident.Date = na_if(Date.From, "No incident linked"),
         Incident.Date = as.Date(Date.From, "%d/%m/%y"),
         days = Reported.Date - Incident.Date,
         days = ifelse(days < 0, 0, days),
         days = as.numeric(days))

PF_A <- PF_A %>%
  mutate(Time.elapsed.between.committed.from.and.reported.dates..days. = na_if(Time.elapsed.between.committed.from.and.reported.dates..days., "#VALUE!"),
         days = Time.elapsed.between.committed.from.and.reported.dates..days.,
         days = as.numeric(days))

PF_C <- PF_C %>%
  mutate(days = as.numeric(Time.from.offence.to.record..days.))

PF_E <- PF_E %>%
  mutate(days = as.numeric(Days.from.occurrence.from.to.reported.dates),
         days = ifelse(days < 0, 0, days))

PF_D <- PF_D %>%
  mutate(Days.from.committed.from.to.reported.dates = na_if(Days.from.committed.from.to.reported.dates, '#VALUE!'),
         days = as.numeric(Days.from.committed.from.to.reported.dates),
         days = ifelse(days < 0, 0, days))

vars.keep <- vars.keep %>%
  append(c('days'))

#code date between report and completed
PF_B <- PF_B %>%
  mutate(Completed.Date = na_if(Date.Detected, "CANCELLED"),
         Completed.Date = na_if(Completed.Date, ""),
         Completed.Date = as.Date(Completed.Date, "%d/%m/%y"),
         days.investigation = Completed.Date - Reported.Date,
         days.investigation = ifelse(days.investigation < 0, 0, days.investigation),
         days.investigation = as.numeric(days.investigation))

PF_A <- PF_A %>%
  mutate(days.investigation = na_if(Time.elapsed.between.reported.date.and.latest.completed.status.update.date..days.,
                                    "#VALUE!"),
         days.investigation = as.numeric(days.investigation))

PF_E <- PF_E %>%
  mutate(days.investigation = na_if(Days.from.reported.to.outcome.dates,
                                    "#VALUE!"),
         days.investigation = na_if(days.investigation,
                                    "No outcome date"),
         days.investigation = as.numeric(days.investigation))
 
PF_C <- PF_C %>%
  mutate(days.investigation = na_if(Time.from.recorded.to.outcome..days.,
                                    "#VALUE!"),
         days.investigation = as.numeric(days.investigation),
         days.investigation = ifelse(days.investigation < 0, 0, days.investigation))

PF_D <- PF_D %>%
  mutate(days.investigation = na_if(Days.from.reported.date.to.outcome.date,
                                    "#VALUE!"),
         days.investigation = as.numeric(days.investigation))

vars.keep <- vars.keep %>%
  append(c('days.investigation'))

#create variable of no suspect identified
PF_B <- PF_B %>%
  mutate(NSI = ifelse(Disposal.Code == '14' | Disposal.Code == '18', 1, 0))
table(PF_B$NSI)

PF_A <- PF_A %>%
  mutate(NSI = ifelse(Outcome == '14' | Outcome == '18', 1, 0))
table(PF_A$NSI)

PF_C <- PF_C %>%
  mutate(NSI = ifelse(X...Outcome.grouped == 'f. Evidential difficulties: suspect not identified; victim does not support further action (OC14)' | 
                        X...Outcome.grouped == 'i. Investigation complete – no suspect identified (OC18)' |
                        X...Outcome.grouped == 'i. Investigation complete � no suspect identified (OC18)' |
                        X...Outcome.grouped == 'i. Investigation complete \x96 no suspect identified (OC18)' |
                        X...Outcome.grouped == 'i. Investigation complete ????" no suspect identified (OC18)', 1, 0))
table(PF_C$NSI)

PF_E <- PF_E %>%
  mutate(NSI = ifelse(X.Outcome.recoded == 'f. Evidential difficulties: suspect not identified; victim does not support further action (OC14)' | 
                        X.Outcome.recoded == 'i. Investigation complete - no suspect identified (OC18)' |
                        X.Outcome.recoded == 'i. Investigation complete \x96 no suspect identified (OC18)', 1, 0))
table(PF_E$NSI)

PF_D <- PF_D %>%
  mutate(NSI = ifelse(X..Outcome.Type...amended...recoded == 'f. Evidential difficulties: suspect not identified; victim does not support further action (OC14)' | 
                        X..Outcome.Type...amended...recoded == 'i. Investigation complete - no suspect identified (OC18)' |
                        X..Outcome.Type...amended...recoded == 'i. Investigation complete \x96 no suspect identified (OC18)', 1, 0))
table(PF_D$NSI)

vars.keep <- vars.keep %>%
  append(c('NSI'))

#select variables for analysis
PF_B.analysis <- PF_B %>%
  select(paste(vars.keep))
PF_A.analysis <- PF_A %>%
  select(paste(vars.keep))
PF_C.analysis <- PF_C %>%
  select(paste(vars.keep))
PF_E.analysis <- PF_E %>%
  select(paste(vars.keep))
PF_D.analysis <- PF_D %>%
  select(paste(vars.keep))

#recode to binary outcomes
PF_B.analysis <- PF_B.analysis %>%
  dummy_cols(paste(vars.keep[c(1, 3, 5, 6, 8, 9)])) %>%
  mutate(age.victim_under18 = ifelse(age.victim < 18, 1, 0),
         age.victim_under18 = ifelse(is.na(age.victim_under18), 0, age.victim_under18),
         age.victim_18to25 = ifelse(age.victim >= 18 & age.victim < 26, 1, 0),
         age.victim_18to25 = ifelse(is.na(age.victim_18to25), 0, age.victim_18to25),
         age.victim_26to40 = ifelse(age.victim >= 26 & age.victim < 41, 1, 0),
         age.victim_26to40 = ifelse(is.na(age.victim_26to40), 0, age.victim_26to40),
         age.victim_over40 = ifelse(age.victim >= 41, 1, 0),
         age.victim_over40 = ifelse(is.na(age.victim_over40), 0, age.victim_over40),
         age.victim_unknown = ifelse(is.na(age.victim), 1, 0),
         age.suspect_under18 = ifelse(age.suspect < 18, 1, 0),
         age.suspect_under18 = ifelse(is.na(age.suspect_under18), 0, age.suspect_under18),
         age.suspect_18to25 = ifelse(age.suspect >= 18 & age.suspect < 26, 1, 0),
         age.suspect_18to25 = ifelse(is.na(age.suspect_18to25), 0, age.suspect_18to25),
         age.suspect_26to40 = ifelse(age.suspect >= 26 & age.suspect < 41, 1, 0),
         age.suspect_26to40 = ifelse(is.na(age.suspect_26to40), 0, age.suspect_26to40),
         age.suspect_over40 = ifelse(age.suspect >= 41, 1, 0),
         age.suspect_over40 = ifelse(is.na(age.suspect_over40), 0, age.suspect_over40),
         age.suspect_unknown = ifelse(is.na(age.suspect), 1, 0),
         report_sameday = ifelse(days == 0, 1, 0),
         report_sameday = ifelse(is.na(report_sameday), 0, report_sameday),
         report_1to2days = ifelse(days >= 1 & days < 3, 1, 0),
         report_1to2days = ifelse(is.na(report_1to2days), 0, report_1to2days),
         report_3to10days = ifelse(days >= 3 & days < 11, 1, 0),
         report_3to10days = ifelse(is.na(report_3to10days), 0, report_3to10days),
         report_11to100days = ifelse(days >= 11 & days < 101, 1, 0),
         report_11to100days = ifelse(is.na(report_11to100days), 0, report_11to100days),
         report_over100days = ifelse(days >= 101, 1, 0),
         report_over100days = ifelse(is.na(report_over100days), 0, report_over100days),
         report_unknown = ifelse(is.na(days), 1, 0),
         outcome_sameday = ifelse(days.investigation == 0, 1, 0),
         outcome_sameday = ifelse(is.na(outcome_sameday), 0, outcome_sameday),
         outcome_1to2days = ifelse(days.investigation >= 1 & days.investigation < 3, 1, 0),
         outcome_1to2days = ifelse(is.na(outcome_1to2days), 0, outcome_1to2days),
         outcome_3to10days = ifelse(days.investigation >= 3 & days.investigation < 11, 1, 0),
         outcome_3to10days = ifelse(is.na(outcome_3to10days), 0, outcome_3to10days),
         outcome_11to100days = ifelse(days.investigation >= 11 & days.investigation < 101, 1, 0),
         outcome_11to100days = ifelse(is.na(outcome_11to100days), 0, outcome_11to100days),
         outcome_over100days = ifelse(days.investigation >= 101, 1, 0),
         outcome_over100days = ifelse(is.na(outcome_over100days), 0, outcome_over100days),
         outcome_unknown = ifelse(is.na(days.investigation), 1, 0))

PF_A.analysis <- PF_A.analysis %>%
  dummy_cols(paste(vars.keep[c(1, 3, 5, 6, 8, 9)])) %>%
  mutate(age.victim_under18 = ifelse(age.victim < 18, 1, 0),
         age.victim_under18 = ifelse(is.na(age.victim_under18), 0, age.victim_under18),
         age.victim_18to25 = ifelse(age.victim >= 18 & age.victim < 26, 1, 0),
         age.victim_18to25 = ifelse(is.na(age.victim_18to25), 0, age.victim_18to25),
         age.victim_26to40 = ifelse(age.victim >= 26 & age.victim < 41, 1, 0),
         age.victim_26to40 = ifelse(is.na(age.victim_26to40), 0, age.victim_26to40),
         age.victim_over40 = ifelse(age.victim >= 41, 1, 0),
         age.victim_over40 = ifelse(is.na(age.victim_over40), 0, age.victim_over40),
         age.victim_unknown = ifelse(is.na(age.victim), 1, 0),
         age.suspect_under18 = ifelse(age.suspect < 18, 1, 0),
         age.suspect_under18 = ifelse(is.na(age.suspect_under18), 0, age.suspect_under18),
         age.suspect_18to25 = ifelse(age.suspect >= 18 & age.suspect < 26, 1, 0),
         age.suspect_18to25 = ifelse(is.na(age.suspect_18to25), 0, age.suspect_18to25),
         age.suspect_26to40 = ifelse(age.suspect >= 26 & age.suspect < 41, 1, 0),
         age.suspect_26to40 = ifelse(is.na(age.suspect_26to40), 0, age.suspect_26to40),
         age.suspect_over40 = ifelse(age.suspect >= 41, 1, 0),
         age.suspect_over40 = ifelse(is.na(age.suspect_over40), 0, age.suspect_over40),
         age.suspect_unknown = ifelse(is.na(age.suspect), 1, 0),
         report_sameday = ifelse(days == 0, 1, 0),
         report_sameday = ifelse(is.na(report_sameday), 0, report_sameday),
         report_1to2days = ifelse(days >= 1 & days < 3, 1, 0),
         report_1to2days = ifelse(is.na(report_1to2days), 0, report_1to2days),
         report_3to10days = ifelse(days >= 3 & days < 11, 1, 0),
         report_3to10days = ifelse(is.na(report_3to10days), 0, report_3to10days),
         report_11to100days = ifelse(days >= 11 & days < 101, 1, 0),
         report_11to100days = ifelse(is.na(report_11to100days), 0, report_11to100days),
         report_over100days = ifelse(days >= 101, 1, 0),
         report_over100days = ifelse(is.na(report_over100days), 0, report_over100days),
         report_unknown = ifelse(is.na(days), 1, 0),
         outcome_sameday = ifelse(days.investigation == 0, 1, 0),
         outcome_sameday = ifelse(is.na(outcome_sameday), 0, outcome_sameday),
         outcome_1to2days = ifelse(days.investigation >= 1 & days.investigation < 3, 1, 0),
         outcome_1to2days = ifelse(is.na(outcome_1to2days), 0, outcome_1to2days),
         outcome_3to10days = ifelse(days.investigation >= 3 & days.investigation < 11, 1, 0),
         outcome_3to10days = ifelse(is.na(outcome_3to10days), 0, outcome_3to10days),
         outcome_11to100days = ifelse(days.investigation >= 11 & days.investigation < 101, 1, 0),
         outcome_11to100days = ifelse(is.na(outcome_11to100days), 0, outcome_11to100days),
         outcome_over100days = ifelse(days.investigation >= 101, 1, 0),
         outcome_over100days = ifelse(is.na(outcome_over100days), 0, outcome_over100days),
         outcome_unknown = ifelse(is.na(days.investigation), 1, 0))

PF_C.analysis <- PF_C.analysis %>%
  dummy_cols(paste(vars.keep[c(1, 3, 5, 6, 8, 9)])) %>%
  mutate(age.victim_under18 = ifelse(age.victim < 18, 1, 0),
         age.victim_under18 = ifelse(is.na(age.victim_under18), 0, age.victim_under18),
         age.victim_18to25 = ifelse(age.victim >= 18 & age.victim < 26, 1, 0),
         age.victim_18to25 = ifelse(is.na(age.victim_18to25), 0, age.victim_18to25),
         age.victim_26to40 = ifelse(age.victim >= 26 & age.victim < 41, 1, 0),
         age.victim_26to40 = ifelse(is.na(age.victim_26to40), 0, age.victim_26to40),
         age.victim_over40 = ifelse(age.victim >= 41, 1, 0),
         age.victim_over40 = ifelse(is.na(age.victim_over40), 0, age.victim_over40),
         age.victim_unknown = ifelse(is.na(age.victim), 1, 0),
         age.suspect_under18 = ifelse(age.suspect < 18, 1, 0),
         age.suspect_under18 = ifelse(is.na(age.suspect_under18), 0, age.suspect_under18),
         age.suspect_18to25 = ifelse(age.suspect >= 18 & age.suspect < 26, 1, 0),
         age.suspect_18to25 = ifelse(is.na(age.suspect_18to25), 0, age.suspect_18to25),
         age.suspect_26to40 = ifelse(age.suspect >= 26 & age.suspect < 41, 1, 0),
         age.suspect_26to40 = ifelse(is.na(age.suspect_26to40), 0, age.suspect_26to40),
         age.suspect_over40 = ifelse(age.suspect >= 41, 1, 0),
         age.suspect_over40 = ifelse(is.na(age.suspect_over40), 0, age.suspect_over40),
         age.suspect_unknown = ifelse(is.na(age.suspect), 1, 0),
         report_sameday = ifelse(days == 0, 1, 0),
         report_sameday = ifelse(is.na(report_sameday), 0, report_sameday),
         report_1to2days = ifelse(days >= 1 & days < 3, 1, 0),
         report_1to2days = ifelse(is.na(report_1to2days), 0, report_1to2days),
         report_3to10days = ifelse(days >= 3 & days < 11, 1, 0),
         report_3to10days = ifelse(is.na(report_3to10days), 0, report_3to10days),
         report_11to100days = ifelse(days >= 11 & days < 101, 1, 0),
         report_11to100days = ifelse(is.na(report_11to100days), 0, report_11to100days),
         report_over100days = ifelse(days >= 101, 1, 0),
         report_over100days = ifelse(is.na(report_over100days), 0, report_over100days),
         report_unknown = ifelse(is.na(days), 1, 0),
         outcome_sameday = ifelse(days.investigation == 0, 1, 0),
         outcome_sameday = ifelse(is.na(outcome_sameday), 0, outcome_sameday),
         outcome_1to2days = ifelse(days.investigation >= 1 & days.investigation < 3, 1, 0),
         outcome_1to2days = ifelse(is.na(outcome_1to2days), 0, outcome_1to2days),
         outcome_3to10days = ifelse(days.investigation >= 3 & days.investigation < 11, 1, 0),
         outcome_3to10days = ifelse(is.na(outcome_3to10days), 0, outcome_3to10days),
         outcome_11to100days = ifelse(days.investigation >= 11 & days.investigation < 101, 1, 0),
         outcome_11to100days = ifelse(is.na(outcome_11to100days), 0, outcome_11to100days),
         outcome_over100days = ifelse(days.investigation >= 101, 1, 0),
         outcome_over100days = ifelse(is.na(outcome_over100days), 0, outcome_over100days),
         outcome_unknown = ifelse(is.na(days.investigation), 1, 0))

PF_E.analysis <- PF_E.analysis %>%
  dummy_cols(paste(vars.keep[c(1, 3, 5, 6, 8, 9)])) %>%
  mutate(age.victim_under18 = ifelse(age.victim < 18, 1, 0),
         age.victim_under18 = ifelse(is.na(age.victim_under18), 0, age.victim_under18),
         age.victim_18to25 = ifelse(age.victim >= 18 & age.victim < 26, 1, 0),
         age.victim_18to25 = ifelse(is.na(age.victim_18to25), 0, age.victim_18to25),
         age.victim_26to40 = ifelse(age.victim >= 26 & age.victim < 41, 1, 0),
         age.victim_26to40 = ifelse(is.na(age.victim_26to40), 0, age.victim_26to40),
         age.victim_over40 = ifelse(age.victim >= 41, 1, 0),
         age.victim_over40 = ifelse(is.na(age.victim_over40), 0, age.victim_over40),
         age.victim_unknown = ifelse(is.na(age.victim), 1, 0),
         age.suspect_under18 = ifelse(age.suspect < 18, 1, 0),
         age.suspect_under18 = ifelse(is.na(age.suspect_under18), 0, age.suspect_under18),
         age.suspect_18to25 = ifelse(age.suspect >= 18 & age.suspect < 26, 1, 0),
         age.suspect_18to25 = ifelse(is.na(age.suspect_18to25), 0, age.suspect_18to25),
         age.suspect_26to40 = ifelse(age.suspect >= 26 & age.suspect < 41, 1, 0),
         age.suspect_26to40 = ifelse(is.na(age.suspect_26to40), 0, age.suspect_26to40),
         age.suspect_over40 = ifelse(age.suspect >= 41, 1, 0),
         age.suspect_over40 = ifelse(is.na(age.suspect_over40), 0, age.suspect_over40),
         age.suspect_unknown = ifelse(is.na(age.suspect), 1, 0),
         report_sameday = ifelse(days == 0, 1, 0),
         report_sameday = ifelse(is.na(report_sameday), 0, report_sameday),
         report_1to2days = ifelse(days >= 1 & days < 3, 1, 0),
         report_1to2days = ifelse(is.na(report_1to2days), 0, report_1to2days),
         report_3to10days = ifelse(days >= 3 & days < 11, 1, 0),
         report_3to10days = ifelse(is.na(report_3to10days), 0, report_3to10days),
         report_11to100days = ifelse(days >= 11 & days < 101, 1, 0),
         report_11to100days = ifelse(is.na(report_11to100days), 0, report_11to100days),
         report_over100days = ifelse(days >= 101, 1, 0),
         report_over100days = ifelse(is.na(report_over100days), 0, report_over100days),
         report_unknown = ifelse(is.na(days), 1, 0),
         outcome_sameday = ifelse(days.investigation == 0, 1, 0),
         outcome_sameday = ifelse(is.na(outcome_sameday), 0, outcome_sameday),
         outcome_1to2days = ifelse(days.investigation >= 1 & days.investigation < 3, 1, 0),
         outcome_1to2days = ifelse(is.na(outcome_1to2days), 0, outcome_1to2days),
         outcome_3to10days = ifelse(days.investigation >= 3 & days.investigation < 11, 1, 0),
         outcome_3to10days = ifelse(is.na(outcome_3to10days), 0, outcome_3to10days),
         outcome_11to100days = ifelse(days.investigation >= 11 & days.investigation < 101, 1, 0),
         outcome_11to100days = ifelse(is.na(outcome_11to100days), 0, outcome_11to100days),
         outcome_over100days = ifelse(days.investigation >= 101, 1, 0),
         outcome_over100days = ifelse(is.na(outcome_over100days), 0, outcome_over100days),
         outcome_unknown = ifelse(is.na(days.investigation), 1, 0))

PF_D.analysis <- PF_D.analysis %>%
  dummy_cols(paste(vars.keep[c(1, 3, 5, 6, 8, 9)])) %>%
  mutate(age.victim_under18 = ifelse(age.victim < 18, 1, 0),
         age.victim_under18 = ifelse(is.na(age.victim_under18), 0, age.victim_under18),
         age.victim_18to25 = ifelse(age.victim >= 18 & age.victim < 26, 1, 0),
         age.victim_18to25 = ifelse(is.na(age.victim_18to25), 0, age.victim_18to25),
         age.victim_26to40 = ifelse(age.victim >= 26 & age.victim < 41, 1, 0),
         age.victim_26to40 = ifelse(is.na(age.victim_26to40), 0, age.victim_26to40),
         age.victim_over40 = ifelse(age.victim >= 41, 1, 0),
         age.victim_over40 = ifelse(is.na(age.victim_over40), 0, age.victim_over40),
         age.victim_unknown = ifelse(is.na(age.victim), 1, 0),
         age.suspect_under18 = ifelse(age.suspect < 18, 1, 0),
         age.suspect_under18 = ifelse(is.na(age.suspect_under18), 0, age.suspect_under18),
         age.suspect_18to25 = ifelse(age.suspect >= 18 & age.suspect < 26, 1, 0),
         age.suspect_18to25 = ifelse(is.na(age.suspect_18to25), 0, age.suspect_18to25),
         age.suspect_26to40 = ifelse(age.suspect >= 26 & age.suspect < 41, 1, 0),
         age.suspect_26to40 = ifelse(is.na(age.suspect_26to40), 0, age.suspect_26to40),
         age.suspect_over40 = ifelse(age.suspect >= 41, 1, 0),
         age.suspect_over40 = ifelse(is.na(age.suspect_over40), 0, age.suspect_over40),
         age.suspect_unknown = ifelse(is.na(age.suspect), 1, 0),
         report_sameday = ifelse(days == 0, 1, 0),
         report_sameday = ifelse(is.na(report_sameday), 0, report_sameday),
         report_1to2days = ifelse(days >= 1 & days < 3, 1, 0),
         report_1to2days = ifelse(is.na(report_1to2days), 0, report_1to2days),
         report_3to10days = ifelse(days >= 3 & days < 11, 1, 0),
         report_3to10days = ifelse(is.na(report_3to10days), 0, report_3to10days),
         report_11to100days = ifelse(days >= 11 & days < 101, 1, 0),
         report_11to100days = ifelse(is.na(report_11to100days), 0, report_11to100days),
         report_over100days = ifelse(days >= 101, 1, 0),
         report_over100days = ifelse(is.na(report_over100days), 0, report_over100days),
         report_unknown = ifelse(is.na(days), 1, 0),
         outcome_sameday = ifelse(days.investigation == 0, 1, 0),
         outcome_sameday = ifelse(is.na(outcome_sameday), 0, outcome_sameday),
         outcome_1to2days = ifelse(days.investigation >= 1 & days.investigation < 3, 1, 0),
         outcome_1to2days = ifelse(is.na(outcome_1to2days), 0, outcome_1to2days),
         outcome_3to10days = ifelse(days.investigation >= 3 & days.investigation < 11, 1, 0),
         outcome_3to10days = ifelse(is.na(outcome_3to10days), 0, outcome_3to10days),
         outcome_11to100days = ifelse(days.investigation >= 11 & days.investigation < 101, 1, 0),
         outcome_11to100days = ifelse(is.na(outcome_11to100days), 0, outcome_11to100days),
         outcome_over100days = ifelse(days.investigation >= 101, 1, 0),
         outcome_over100days = ifelse(is.na(outcome_over100days), 0, outcome_over100days),
         outcome_unknown = ifelse(is.na(days.investigation), 1, 0))

#merge data from all forces
dataset.unique <- PF_E.analysis %>%
  bind_rows(PF_B.analysis, PF_C.analysis, PF_A.analysis, PF_D.analysis) %>%
  replace(is.na(.), 0)

#count number of 'missings' in total
dataset.unique <- dataset.unique %>%
  mutate(all_missing = sex.victim_unknown + ethnic.victim_unknown + age.victim_unknown +
           sex.suspect_unknown + ethnic.suspect_unknown + age.suspect_unknown + 
           relation_unknown + report_unknown + outcome_unknown,
         victim_missing = sex.victim_unknown + ethnic.victim_unknown + age.victim_unknown,
         suspect_missing = sex.suspect_unknown + ethnic.suspect_unknown + age.suspect_unknown,
         case_missing = relation_unknown + report_unknown + outcome_unknown)
table(dataset.unique$all_missing)
table(dataset.unique$victim_missing)
table(dataset.unique$suspect_missing)
table(dataset.unique$case_missing)

#print frequency and percentage of missing data for each key variable for each PFA
missing_aggregates <- dataset.unique %>%
  group_by(PFA) %>%
  summarise(cases = n(),
            freq_missing_sex.victim = sum(sex.victim_unknown),
            perc_missing_sex.victim = freq_missing_sex.victim / cases * 100,
            freq_missing_ethnic.victim = sum(ethnic.victim_unknown),
            perc_missing_ethnic.victim = freq_missing_ethnic.victim / cases * 100,
            freq_missing_age.victim = sum(age.victim_unknown),
            perc_missing_age.victim = freq_missing_age.victim / cases * 100, 
            freq_missing_sex.suspect = sum(sex.suspect_unknown),
            perc_missing_sex.suspect = freq_missing_sex.suspect / cases * 100,
            freq_missing_ethnic.suspect = sum(ethnic.suspect_unknown),
            perc_missing_ethnic.suspect = freq_missing_ethnic.suspect / cases * 100,
            freq_missing_age.suspect = sum(age.suspect_unknown),
            perc_missing_age.suspect = freq_missing_age.suspect / cases * 100, 
            freq_missing_relation = sum(relation_unknown),
            perc_missing_relation = freq_missing_relation / cases * 100,    
            freq_missing_report = sum(report_unknown),
            perc_missing_report = freq_missing_report / cases * 100,
            freq_missing_outcome = sum(outcome_unknown),
            perc_missing_outcome = freq_missing_outcome / cases * 100,
            freq_missing_at_least_one = length(all_missing[all_missing > 0]),
            perc_missing_at_least_one = freq_missing_at_least_one / cases * 100,
            average_total_missing = mean(all_missing))

# Add workforce and population data
# Workforce data: https://www.gov.uk/government/statistics/police-workforce-england-and-wales-30-september-2020
# Population: https://www.ons.gov.uk/peoplepopulationandcommunity/crimeandjustice/datasets/policeforceareadatatables (Table P3)

missing_aggregates <- missing_aggregates %>%
  mutate(workforce = c(33177, 1168, 6846, 3081, 2886),
         population = c(8952300, 636900, 2928600, 1339400, 1719000))

missing_aggregates_total <- dataset.unique %>%
  summarise(cases = n(),
            freq_missing_sex.victim = sum(sex.victim_unknown),
            perc_missing_sex.victim = freq_missing_sex.victim / cases * 100,
            freq_missing_ethnic.victim = sum(ethnic.victim_unknown),
            perc_missing_ethnic.victim = freq_missing_ethnic.victim / cases * 100,
            freq_missing_age.victim = sum(age.victim_unknown),
            perc_missing_age.victim = freq_missing_age.victim / cases * 100, 
            freq_missing_sex.suspect = sum(sex.suspect_unknown),
            perc_missing_sex.suspect = freq_missing_sex.suspect / cases * 100, 
            freq_missing_ethnic.suspect = sum(ethnic.suspect_unknown),
            perc_missing_ethnic.suspect = freq_missing_ethnic.suspect / cases * 100, 
            freq_missing_ethnic.suspect = sum(ethnic.suspect_unknown),
            perc_missing_ethnic.suspect = freq_missing_ethnic.suspect / cases * 100,
            freq_missing_age.suspect = sum(age.suspect_unknown),
            perc_missing_age.suspect = freq_missing_age.suspect / cases * 100, 
            freq_missing_relation = sum(relation_unknown),
            perc_missing_relation = freq_missing_relation / cases * 100,    
            freq_missing_report = sum(report_unknown),
            perc_missing_report = freq_missing_report / cases * 100,
            freq_missing_outcome = sum(outcome_unknown),
            perc_missing_outcome = freq_missing_outcome / cases * 100,
            freq_missing_at_least_one = length(all_missing[all_missing > 0]),
            perc_missing_at_least_one = freq_missing_at_least_one / cases * 100,
            average_total_missing = mean(all_missing))

#summary statistics of total missing data
dataset.unique %>%
  select(PFA, all_missing) %>%
  group_by(PFA) %>% 
  summarise(n = n(),
            min = fivenum(all_missing)[1],
            Q1 = fivenum(all_missing)[2],
            mean = mean(all_missing),
            median = fivenum(all_missing)[3],
            Q3 = fivenum(all_missing)[4],
            max = fivenum(all_missing)[5])

dataset.unique %>%
  select(PFA, all_missing) %>%
  summarise(n = n(),
            min = fivenum(all_missing)[1],
            Q1 = fivenum(all_missing)[2],
            mean = mean(all_missing),
            median = fivenum(all_missing)[3],
            Q3 = fivenum(all_missing)[4],
            max = fivenum(all_missing)[5])

dataset.unique %>%
  select(PFA, all_missing) %>%
  summarise(n = n(),
            missing9 = length(all_missing[all_missing == 9]),
            prop = missing9 / n * 100)

dataset.unique %>%
  select(PFA, all_missing) %>%
  group_by(PFA) %>% 
  summarise(n = n(),
            missing9 = length(all_missing[all_missing == 9]),
            prop = missing9 / n * 100)

dataset.unique %>%
  select(victim_missing) %>%
  summarise(n = n(),
            min = fivenum(victim_missing)[1],
            Q1 = fivenum(victim_missing)[2],
            mean = mean(victim_missing),
            median = fivenum(victim_missing)[3],
            Q3 = fivenum(victim_missing)[4],
            max = fivenum(victim_missing)[5])

dataset.unique %>%
  select(suspect_missing) %>%
  summarise(n = n(),
            min = fivenum(suspect_missing)[1],
            Q1 = fivenum(suspect_missing)[2],
            mean = mean(suspect_missing),
            median = fivenum(suspect_missing)[3],
            Q3 = fivenum(suspect_missing)[4],
            max = fivenum(suspect_missing)[5])

dataset.unique %>%
  select(case_missing) %>%
  summarise(n = n(),
            min = fivenum(case_missing)[1],
            Q1 = fivenum(case_missing)[2],
            mean = mean(case_missing),
            median = fivenum(case_missing)[3],
            Q3 = fivenum(case_missing)[4],
            max = fivenum(case_missing)[5])

dataset.unique_constructs <- dataset.unique %>%
  select(victim_missing, suspect_missing, case_missing)
Hmisc::rcorr(as.matrix(dataset.unique_constructs), type="pearson")

# Log transform for visualisation
missing_aggregates <- missing_aggregates %>%
  mutate(cases_log = log(cases),
         average_total_missing_log = log(average_total_missing),
         perc_missing_at_least_one_log = log(perc_missing_at_least_one),
         workforce_log = log(workforce),
         population_log = log(population))

#is there more missing data in police forces with more cases, workforce or population?
cor.test(missing_aggregates$cases_log, missing_aggregates$average_total_missing_log, method = "pearson")
cor.test(missing_aggregates$cases_log, missing_aggregates$perc_missing_at_least_one_log, method = "pearson")

cor.test(missing_aggregates$workforce_log, missing_aggregates$average_total_missing_log, method = "pearson")
cor.test(missing_aggregates$workforce_log, missing_aggregates$perc_missing_at_least_one_log, method = "pearson")

cor.test(missing_aggregates$population_log, missing_aggregates$average_total_missing_log, method = "pearson")
cor.test(missing_aggregates$population_log, missing_aggregates$perc_missing_at_least_one_log, method = "pearson")

p1 <- ggplot(missing_aggregates, aes(x = cases_log, y = average_total_missing_log)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  stat_cor(method = "pearson", 
           label.x.npc = 0.98,
           label.y.npc = 0.995,
           hjust = 1,
           vjust = -3.8) +
  ylab("Log average missing\nvariables per case") +
  xlab("Log number of reports") +
  theme_classic() +
  coord_cartesian(clip = "off")

p2 <- ggplot(missing_aggregates, aes(x = cases_log, y = perc_missing_at_least_one_log)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  stat_cor(method = "pearson", 
           label.x.npc = 0.98,
           label.y.npc = 0.995,
           hjust = 1,
           vjust = -3.8) +
  ylab("Log % cases with at\nleast one missing variable") +
  xlab("Log number of reports") +
  theme_classic() +
  coord_cartesian(clip = "off")

p3 <- ggplot(missing_aggregates, aes(x = workforce_log, y = average_total_missing_log)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  stat_cor(method = "pearson", 
           label.x.npc = 0.98,
           label.y.npc = 0.995,
           hjust = 1,
           vjust = -3.8) +
  ylab("Log average missing\nvariables per case") +
  xlab("Log police workforce") +
  theme_classic() +
  coord_cartesian(clip = "off")

p4 <- ggplot(missing_aggregates, aes(x = workforce_log, y = perc_missing_at_least_one_log)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  stat_cor(method = "pearson", 
           label.x.npc = 0.98,
           label.y.npc = 0.995,
           hjust = 1,
           vjust = -3.8) +
  ylab("Log % cases with at\nleast one missing variable") +
  xlab("Log police workforce") +
  theme_classic() +
  coord_cartesian(clip = "off")

p5 <- ggplot(missing_aggregates, aes(x = population_log, y = average_total_missing_log)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  stat_cor(method = "pearson", 
           label.x.npc = 0.98,
           label.y.npc = 0.995,
           hjust = 1,
           vjust = -3.8) +
  ylab("Log average missing\nvariables per case") +
  xlab("Log population") +
  theme_classic() +
  coord_cartesian(clip = "off")

p6 <- ggplot(missing_aggregates, aes(x = population_log, y = perc_missing_at_least_one_log)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  stat_cor(method = "pearson", 
           label.x.npc = 0.98,
           label.y.npc = 0.995,
           hjust = 1,
           vjust = -3.8) +
  ylab("Log % cases with at\nleast one missing variable") +
  xlab("Log population") +
  theme_classic() +
  coord_cartesian(clip = "off")

combined_plot <- ggarrange(
  p1, p2,
  p3, p4,
  p5, p6,
  ncol = 2, nrow = 3,
  labels = c("A", "B", "C", "D", "E", "F")
)
combined_plot

#ggsave("plots/all_scatter_plots_with_cor.jpg", combined_plot,
#       width = 17, height = 20, units = "cm", bg = "white")

#does missing data correlate with charge outcomes
missing_charged <- dataset.unique %>%
  group_by(PFA) %>%
  summarise(cases = n(),
            freq_missing_at_least_one = length(all_missing[all_missing > 0]),
            perc_missing_at_least_one = freq_missing_at_least_one / cases * 100,
            freq_charged = sum(outcome_Charged),
            perc_charged = freq_charged / cases * 100,
            average_total_missing = mean(all_missing))

ggplot(missing_charged, aes(x = perc_charged, y = average_total_missing)) + 
  geom_point() +
  geom_smooth(method = lm) +
  ylab("Average missing variables per case") +
  xlab("Percentage charged") +
  theme_classic()
#ggsave("plots/scatter_plot_3.jpg", width = 12, height = 12, units = "cm", 
#       bg = "white")

cor.test(missing_charged$perc_charged, missing_charged$average_total_missing, method = "spearman")

ggplot(missing_charged, aes(x = perc_charged, y = perc_missing_at_least_one)) + 
  geom_point() +
  geom_smooth(method = lm) +
  ylab("Percentage of cases in which at least one variable is missing") +
  xlab("Percentage charged") +
  theme_classic()
#ggsave("plots/scatter_plot_4.jpg", width = 12, height = 12, units = "cm", 
#       bg = "white")

cor.test(missing_charged$perc_charged, missing_charged$perc_missing_at_least_one, method = "spearman")

#boxplots charge and missingness
ggboxplot(dataset.unique, x = "outcome_Charged", y = "all_missing", 
          color = "outcome_Charged", palette = c("#00AFBB", "#E7B800"),
          ylab = "Missing variables", xlab = "Outcome 'charged'") +
          theme(legend.position = "none")
#ggsave("plots/missingness_boxplots.jpg", width = 12, height = 7, units = "cm", 
#       bg = "white")

dataset.unique <- dataset.unique %>%
  mutate(outcome_Charged2 = recode(outcome_Charged,
                                    '1' = 'Charged',
                                   '0' = 'Not charged'))

ggplot2::ggplot(dataset.unique, aes(x = outcome_Charged2, y = all_missing, fill = outcome_Charged2, group = outcome_Charged2)) +
  geom_violin(adjust = 3) +  # Use adjust to change the smoothness of the violin plot
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +  # Custom colors
  labs(y = "Missing variables", x = "Outcome 'charged'") +
  theme_bw() + # Labels
  theme(legend.position = "none")
#ggsave("plots/missingness_violinplots.jpg", width = 12, height = 7, units = "cm", 
#       bg = "white")

#F-test to test for homogeneity in variances
var.test(all_missing ~ outcome_Charged, data = dataset.unique)
#pvalue smaller than 0.05 so we can assume equal variances

#t test
t.test(all_missing ~ outcome_Charged, 
       data = dataset.unique,
       alternative = "two.sided", var.equal = TRUE)
#pvalue less than 0.05 - we can conclude that differences are significant

#does mean missing data vary across individual characteristics
boxplot_sex_victim <- ggboxplot(dataset.unique[dataset.unique$sex.victim != "unknown",], 
          x = "sex.victim", y = "all_missing", 
          ylab = "Missing variables", xlab = "Sex victim") +
  coord_flip()
#boxplot_sex_victim
var.test(all_missing ~ sex.victim, 
         data = dataset.unique[dataset.unique$sex.victim != "unknown",])
t.test(all_missing ~ sex.victim, 
       data = dataset.unique[dataset.unique$sex.victim != "unknown",],
       alternative = "two.sided", var.equal = TRUE)

boxplot_sex_suspect <- ggboxplot(dataset.unique[dataset.unique$sex.suspect != "unknown",], 
                                x = "sex.suspect", y = "all_missing",
                                ylab = "Missing variables", xlab = "Sex suspect") +
  coord_flip()
#boxplot_sex_suspect
var.test(all_missing ~ sex.suspect, 
         data = dataset.unique[dataset.unique$sex.suspect != "unknown",])
t.test(all_missing ~ sex.suspect, 
       data = dataset.unique[dataset.unique$sex.suspect != "unknown",],
       alternative = "two.sided", var.equal = TRUE)

boxplot_ethnic_victim <- ggboxplot(dataset.unique[dataset.unique$ethnic.victim != "unknown",], 
                                x = "ethnic.victim", y = "all_missing",
                                ylab = "Missing variables", xlab = "Ethnicity victim") +
  coord_flip()
#boxplot_ethnic_victim
anova_test(all_missing ~ ethnic.victim,
           data = dataset.unique[dataset.unique$ethnic.victim != "unknown",])
dataset.unique %>%
  filter(ethnic.victim != "unknown") %>%
  group_by(ethnic.victim) %>%
  summarise(mean = mean(all_missing))

boxplot_ethnic_suspect <- ggboxplot(dataset.unique[dataset.unique$ethnic.suspect != "unknown",], 
                                   x = "ethnic.suspect", y = "all_missing",
                                   ylab = "Missing variables", xlab = "Ethnicity suspect") +
  coord_flip()
#boxplot_ethnic_suspect 
anova_test(all_missing ~ ethnic.suspect,
           data = dataset.unique[dataset.unique$ethnic.suspect != "unknown",])
dataset.unique %>%
  filter(ethnic.suspect != "unknown") %>%
  group_by(ethnic.suspect) %>%
  summarise(mean = mean(all_missing))

boxplot_relation <- ggboxplot(dataset.unique[dataset.unique$relation != "unknown",], 
                                    x = "relation", y = "all_missing",
                                    ylab = "Missing variables", xlab = "Victim-suspect relation") +
  coord_flip()
boxplot_relation
anova_test(all_missing ~ relation,
           data = dataset.unique[dataset.unique$relation != "unknown",])
dataset.unique %>%
  filter(relation != "unknown") %>%
  group_by(relation) %>%
  summarise(mean = mean(all_missing))

dataset.unique <- dataset.unique %>%
  mutate(age.suspect_grouped = ifelse(age.suspect_under18 == 1, "under18", NA),
         age.suspect_grouped = ifelse(age.suspect_18to25 == 1, "18to25", age.suspect_grouped),
         age.suspect_grouped = ifelse(age.suspect_26to40 == 1, "26to40", age.suspect_grouped),
         age.suspect_grouped = ifelse(age.suspect_over40 == 1, "over40", age.suspect_grouped),
         age.suspect_grouped = ifelse(age.suspect_unknown == 1, "unknown", age.suspect_grouped),
         age.victim_grouped = ifelse(age.victim_under18 == 1, "under18", NA),
         age.victim_grouped = ifelse(age.victim_18to25 == 1, "18to25", age.victim_grouped),
         age.victim_grouped = ifelse(age.victim_26to40 == 1, "26to40", age.victim_grouped),
         age.victim_grouped = ifelse(age.victim_over40 == 1, "over40", age.victim_grouped),
         age.victim_grouped = ifelse(age.victim_unknown == 1, "unknown", age.victim_grouped),
         outcome_grouped = ifelse(outcome_sameday == 1, "sameday", NA),
         outcome_grouped = ifelse(outcome_1to2days == 1, "1to2days", outcome_grouped),
         outcome_grouped = ifelse(outcome_3to10days == 1, "3to10days", outcome_grouped),
         outcome_grouped = ifelse(outcome_11to100days == 1, "11to100days", outcome_grouped),
         outcome_grouped = ifelse(outcome_over100days == 1, "over100days", outcome_grouped),
         outcome_grouped = ifelse(outcome_unknown == 1, "unknown", outcome_grouped),
         report_grouped = ifelse(report_sameday == 1, "sameday", NA),
         report_grouped = ifelse(report_1to2days == 1, "1to2days", report_grouped),
         report_grouped = ifelse(report_3to10days == 1, "3to10days", report_grouped),
         report_grouped = ifelse(report_11to100days == 1, "11to100days", report_grouped),
         report_grouped = ifelse(report_over100days == 1, "over100days", report_grouped),
         report_grouped = ifelse(report_unknown == 1, "unknown", report_grouped))
         
boxplot_age_victim <- ggboxplot(dataset.unique[dataset.unique$age.victim_grouped != "unknown",], 
                                 x = "age.victim_grouped", y = "all_missing", 
                                 #color = "outcome_Charged", palette = c("#00AFBB", "#E7B800"),
                                 ylab = "Missing variables", xlab = "Age victim") +
  coord_flip()
#boxplot_age_victim
anova_test(all_missing ~ age.victim_grouped,
           data = dataset.unique[dataset.unique$age.victim_grouped != "unknown",])
dataset.unique %>%
  filter(age.victim_grouped != "unknown") %>%
  group_by(age.victim_grouped) %>%
  summarise(mean = mean(all_missing))

boxplot_age_suspect <- ggboxplot(dataset.unique[dataset.unique$age.suspect_grouped != "unknown",], 
                              x = "age.suspect_grouped", y = "all_missing",
                              ylab = "Missing variables", xlab = "Age suspect") +
  coord_flip()
#boxplot_age_suspect
anova_test(all_missing ~ age.suspect_grouped,
           data = dataset.unique[dataset.unique$age.suspect_grouped != "unknown",])
dataset.unique %>%
  filter(age.suspect_grouped != "unknown") %>%
  group_by(age.suspect_grouped) %>%
  summarise(mean = mean(all_missing))

boxplot_report <- ggboxplot(dataset.unique[dataset.unique$report_grouped != "unknown",], 
                                 x = "report_grouped", y = "all_missing",
                                 ylab = "Missing variables", xlab = "Days offence to report") +
  coord_flip()
#boxplot_report
anova_test(all_missing ~ report_grouped,
           data = dataset.unique[dataset.unique$report_grouped != "unknown",])
dataset.unique %>%
  filter(report_grouped != "unknown") %>%
  group_by(report_grouped) %>%
  summarise(mean = mean(all_missing))

boxplot_outcome <- ggboxplot(dataset.unique[dataset.unique$outcome_grouped != "unknown",], 
                             x = "outcome_grouped", y = "all_missing",
                             ylab = "Missing variables", xlab = "Days offence to outcome") +
  coord_flip()
#boxplot_outcome
anova_test(all_missing ~ outcome_grouped,
           data = dataset.unique[dataset.unique$outcome_grouped != "unknown",])
dataset.unique %>%
  filter(outcome_grouped != "unknown") %>%
  group_by(outcome_grouped) %>%
  summarise(mean = mean(all_missing))

ggarrange(boxplot_sex_victim, boxplot_sex_suspect, 
          boxplot_age_victim, boxplot_age_suspect,
          boxplot_ethnic_victim, boxplot_ethnic_suspect,
          boxplot_relation, boxplot_report,
          boxplot_outcome,
          ncol = 2, nrow = 5)

#ggsave("plots/missingness_vars_boxplots.jpg", width = 12, height = 12, units = "cm", 
#       bg = "white")

dataset.unique <- dataset.unique %>%
  mutate(outcome_single = ifelse(`outcome_Attributed to victim` == 1, 'Attributed to victim', NA),
         outcome_single = ifelse(`outcome_Charged` == 1, 'Charged', outcome_single),
         outcome_single = ifelse(`outcome_Due to investigation` == 1, 'Due to investigation', outcome_single),
         outcome_single = ifelse(`outcome_Prosecution prevented` == 1, 'Prosecution prevented', outcome_single),
         outcome_single = ifelse(is.na(outcome_single), "Other", outcome_single))
table(dataset.unique$outcome_single)

anova_test(all_missing ~ outcome_single,
           data = dataset.unique)
dataset.unique %>%
  group_by(outcome_single) %>%
  summarise(mean = mean(all_missing))

ggboxplot(dataset.unique, x = "outcome_Charged", y = "all_missing", 
          color = "outcome_Charged", palette = c("#00AFBB", "#E7B800"),
          ylab = "Missing variables", xlab = "outcome_Charged")
#ggsave("plots/missingness_boxplots.jpg", width = 12, height = 12, units = "cm", 
#       bg = "white")

#does missing data at the case level predict charges - estimate models
model.missing <- glm(`outcome_Charged` ~ sex.victim_unknown + ethnic.victim_unknown +
                                age.victim_unknown + sex.suspect_unknown + ethnic.suspect_unknown +
                                age.suspect_unknown + relation_unknown +
                                report_unknown + outcome_unknown +
                                PFA - 1,
                              family = binomial(link = 'logit'),
                              data = dataset.unique)

model.missing_all <- glm(`outcome_Charged` ~ all_missing +
                       PFA - 1,
                     family = binomial(link = 'logit'),
                     data = dataset.unique)

summary(model.missing)

sjPlot::tab_model(model.missing, model.missing_all,
          show.std = T, show.est = F,
          file = "plots/missing_models.doc")

DescTools::PseudoR2(model.missing, which = "all")
DescTools::PseudoR2(model.missing_all, which = "all")

#replicate analysis after removing no suspect identified
dataset.unique_NSI <- dataset.unique %>%
  filter(NSI == 0)

model.missing_NSI <- glm(`outcome_Charged` ~ sex.victim_unknown + ethnic.victim_unknown +
                       age.victim_unknown + sex.suspect_unknown + ethnic.suspect_unknown +
                       age.suspect_unknown + relation_unknown +
                       report_unknown + outcome_unknown + #all_missing +
                       PFA - 1,
                     family = binomial(link = 'logit'),
                     data = dataset.unique_NSI)

model.missing_all_NSI <- glm(`outcome_Charged` ~ all_missing +
                           PFA - 1,
                         family = binomial(link = 'logit'),
                         data = dataset.unique_NSI)

summary(model.missing_NSI)

sjPlot::tab_model(model.missing_NSI, model.missing_all_NSI,
                  show.std = T, show.est = F,
                  file = "plots/missing_models_NSI.doc")

DescTools::PseudoR2(model.missing_NSI, which = "all")
DescTools::PseudoR2(model.missing_all_NSI, which = "all")

#do individual characteristics at the case level predict missingess - estimate models
hist(dataset.unique$all_missing) #poisson?

dataset.unique <- dataset.unique %>%
  mutate(one_missing = ifelse(all_missing > 0, 1, 0))

model.missing_dv <- glm(one_missing ~ sex.victim_male +
                          ethnic.victim_asian + ethnic.victim_black +
                          ethnic.victim_other +
                          age.victim_18to25 + age.victim_26to40 +
                          age.victim_over40 +
                          sex.suspect_female +
                          ethnic.suspect_asian + ethnic.suspect_black +
                          ethnic.suspect_other +
                          age.suspect_18to25 + age.suspect_26to40 +
                          age.suspect_over40 +
                          `relation_acquaintance, friend` + relation_family +
                          relation_stranger + relation_other +
                          report_1to2days + report_3to10days + report_11to100days +
                          report_over100days +
                          outcome_1to2days + outcome_3to10days + outcome_11to100days +
                          outcome_over100days +
                       PFA - 1,
                     family = binomial(link = 'logit'),
                     data = dataset.unique)
sjPlot::tab_model(model.missing_dv)

model.missing_poisson <- glm(all_missing ~ sex.victim_male +
                    ethnic.victim_asian + ethnic.victim_black +
                    ethnic.victim_other +
                    age.victim_18to25 + age.victim_26to40 +
                    age.victim_over40 +
                    sex.suspect_female +
                    ethnic.suspect_asian + ethnic.suspect_black +
                    ethnic.suspect_other +
                    age.suspect_18to25 + age.suspect_26to40 +
                    age.suspect_over40 +
                    `relation_acquaintance, friend` + relation_family +
                    relation_stranger + relation_other +
                    report_1to2days + report_3to10days + report_11to100days +
                    report_over100days +
                      outcome_1to2days + outcome_3to10days + outcome_11to100days +
                      outcome_over100days +
                    PFA - 1,
                  family = "poisson",
                  data = dataset.unique)
#sjPlot::tab_model(model.missing_poisson)

#check overdispersion
deviance(model.missing_poisson)/model.missing_poisson$df.residual
AER::dispersiontest(model.missing_poisson)

sjPlot::tab_model(model.missing_dv, model.missing_poisson,
                  show.std = T, show.est = F,
                  file = "plots/missing_dv_models.doc")

DescTools::PseudoR2(model.missing_dv, which = "all")
DescTools::PseudoR2(model.missing_poisson, which = "all")

#alluvial graph
dataset.unique_comb <- dataset.unique %>%
  mutate(`Victim characteristics missing` = ifelse(victim_missing >= 1, 'Yes', 'No'),
         `Suspect characteristics missing` = ifelse(suspect_missing >= 1, 'Yes', 'No'),
         `Case characteristics missing` = ifelse(case_missing >= 1, 'Yes', 'No'),
         `Charged` = ifelse(outcome_single == "Charged", 'Charged', 'Not charged')) %>%
  group_by(`Victim characteristics missing`, `Suspect characteristics missing`, 
           `Case characteristics missing`, `Charged`
           ) %>%
  summarise(Freq = n())

ggplot(data = dataset.unique_comb,
       aes(axis1 = `Victim characteristics missing`, 
           axis2 = `Suspect characteristics missing`, 
           axis3 = `Case characteristics missing`, 
           y = Freq)) +
  geom_alluvium(aes(fill = `Charged`),
                curve_type = "cubic") +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_continuous(breaks = 1:3, labels = c("Victim\ncharacteristics\nmissing", 
                                              "Suspect\ncharacteristics\nmissing",
                                              "Offense\ncharacteristics\nmissing")) +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
  
ggsave("plots/alluvial.jpg", width = 20, height = 12, units = "cm", 
       bg = "white")
