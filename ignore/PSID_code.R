library(readxl)
raw_PSID <- read_excel("J325858.xlsx")

# label variables
library(tidyverse)
renamed_PSID <- raw_PSID |>
  rename(
    "2005_release_num"=ER25001, 
    "2005_interview_id"=ER25002,     
    "2005_state_code"=ER25003,            
    "2005_current_state"=ER25004,
    "2005_family_id"=ER25009,
    "2005_religious_pref"=ER27442,             
    "2005_donations"=ER27451,   
    "2005_attendance"=ER27708,      
    "2005_income"=ER28037,       
    
    "2007_release_num"=ER36001,                          
    "2007_interview_id"=ER36002,     
    "2007_state_code"=ER36003,            
    "2007_current_state"=ER36004,
    "2007_family_id"=ER36009,
    "2007_religious_pref"=ER40614,             
    "2007_donations"=ER40622,   
    "2007_income"=ER41027,        
    
    "2009_release_num"=ER42001,                          
    "2009_interview_id"=ER42002,     
    "2009_state_code"=ER42003,            
    "2009_current_state"=ER42004,
    "2009_family_id"=ER42009,
    "2009_religious_pref"=ER46592,             
    "2009_donations"=ER46600,   
    "2009_income"=ER46935, 
    
    "2011_release_num"=ER47301,                          
    "2011_interview_id"=ER47302,     
    "2011_state_code"=ER47303,            
    "2011_current_state"=ER47304,
    "2011_family_id"=ER47309,
    "2011_religious_pref"=ER51953,             
    "2011_donations"=ER51961,   
    "2011_attendance"=ER52046,      
    "2011_income"=ER52343, 
    
    "2013_release_num"=ER53001,                          
    "2013_interview_id"=ER53002,     
    "2013_state_code"=ER53003,            
    "2013_current_state"=ER53004,
    "2013_family_id"=ER53009,
    "2013_religious_pref"=ER57709,             
    "2013_donations"=ER57730,   
    "2013_income"=ER58152,
    
    "2015_release_num"=ER60001,                          
    "2015_interview_id"=ER60002,     
    "2015_state_code"=ER60003,            
    "2015_current_state"=ER60004,
    "2015_family_id"=ER60009,
    "2015_religious_pref"=ER64869,             
    "2015_donations"=ER64910,   
    "2015_income"=ER65349,
    
    "2017_release_num"=ER66001,                          
    "2017_interview_id"=ER66002,     
    "2017_state_code"=ER66003,            
    "2017_current_state"=ER66004,
    "2017_family_id"=ER66009,
    "2017_religious_pref"=ER70941,             
    "2017_donations"=ER71042,   
    "2017_attendance"=ER71064,      
    "2017_income"=ER71426, 
    
    "2019_release_num"=ER72001,                          
    "2019_interview_id"=ER72002,     
    "2019_state_code"=ER72003,            
    "2019_current_state"=ER72004,
    "2019_family_id"=ER72009,
    "2019_religious_pref"=ER76960,             
    "2019_donations"=ER77064,   
    "2019_attendance"=ER77086,      
    "2019_income"=ER77448,
    
    "2021_release_num"=ER78001,                          
    "2021_interview_id"=ER78002,     
    "2021_state_code"=ER78003,            
    "2021_current_state"=ER78004,
    "2021_family_id"=ER78009,
    "2021_religious_pref"=ER81188,             
    "2021_donations"=ER81340,   
    "2021_attendance"=ER81422,      
    "2021_income"=ER81775
  )

# change from wide to tidy format
donationsincome <- renamed_PSID |>
  pivot_longer(cols=c("2005_income", "2007_income", "2009_income", "2011_income", "2013_income", "2015_income", "2017_income", "2019_income", "2021_income"), names_to="year", values_to="income") |>
  pivot_longer(cols=c("2005_donations", "2007_donations", "2009_donations", "2011_donations", "2013_donations", "2015_donations", "2017_donations", "2019_donations", "2021_donations"), names_to="year2", values_to="donations") |>
  select(donations, income, year)

library(ggplot2)
library(skimr)
skim(donationsincome$donations)
cleaneddonationsincome <- donationsincome |>
  filter(!donations == 999997) |>
  filter(!donations == 999998) |>
  filter(!donations == 999999) |>
  filter(!is.na(donations)) |>
  filter(!income == -999999) |>
  filter(!income == 9999999) |>
  filter(!is.na(income))
ggplot(cleaneddonationsincome, aes(donations)) + geom_histogram()
ggplot(cleaneddonationsincome, aes(income, donations)) + geom_point()

attendanceincome <- renamed_PSID |>
  pivot_longer(cols=c("2005_income", "2007_income", "2009_income", "2011_income", "2013_income", "2015_income", "2017_income", "2019_income", "2021_income"), names_to="year", values_to="income") |>
  pivot_longer(cols=c("2005_attendance", "2011_attendance", "2017_attendance", "2019_attendance", "2021_attendance"), names_to="year9", values_to="attendance") |>
  select(attendance, income, year)

skim(attendanceincome$attendance)
cleanedattendanceincome <- attendanceincome |>
  filter(!attendance == 98) |>
  filter(!attendance == 99) |>
  filter(!is.na(attendance)) |>
  filter(!income == -999999) |>
  filter(!income == 9999999) |>
  filter(!is.na(income))
ggplot(cleanedattendanceincome, aes(attendance)) + geom_histogram()
ggplot(cleanedattendanceincome, aes(income, attendance)) + geom_point()

affiliationtime <- renamed_PSID |>
  pivot_longer(cols=c("2005_religious_pref", "2007_religious_pref", "2009_religious_pref", "2011_religious_pref", "2013_religious_pref", "2015_religious_pref", "2017_religious_pref", "2019_religious_pref", "2021_religious_pref"), names_to="year8", values_to="religious_pref") |>
  select(religious_pref, year8)

cleanedaffiliationtime <- affiliationtime |>
  filter(!is.na(religious_pref)) |>
  mutate(affiliated = if_else((religious_pref == 1|religious_pref == 2|religious_pref == 8|religious_pref == 10|religious_pref == 13), 1, 0)) |>
  group_by(year8) |>
  summarize(mean_affiliated = mean(affiliated))

ggplot(cleanedaffiliationtime, aes(year8, mean_affiliated)) + geom_point()

cleaneddonationsincome2 <- cleaneddonationsincome |>
  group_by(year) |>
  summarize(mean_income = mean(income)) |>
  group_by(year) |>
  summarize(mean_donations = mean(donations))
cleanedattendanceincome2 <- cleanedattendanceincome |>
  group_by(year) |>
  summarize(mean_attendance = mean(attendance))

ggplot(cleaneddonationsincome2, aes(year, mean_income)) + geom_point()
ggplot(cleaneddonationsincome2, aes(year, mean_donations)) + geom_point()
ggplot(cleanedattendanceincome2, aes(year, mean_attendance)) + geom_point()

# for later
# pivot_longer(cols=c("2005_family_id", "2007_family_id", "2009_family_id", "2011_family_id", "2013_family_id", "2015_family_id", "2017_family_id", "2019_family_id", "2021_family_id"), names_to="year7", values_to="family_id")

# pivot_longer(cols=c("2005_religious_pref", "2007_religious_pref", "2009_religious_pref", "2011_religious_pref", "2013_religious_pref", "2015_religious_pref", "2017_religious_pref", "2019_religious_pref", "2021_religious_pref"), names_to="year8", values_to="religious_pref")

# pivot_longer(cols = c("2005_state_code", "2007_state_code", "2009_state_code", "2011_state_code", "2013_state_code", "2015_state_code", "2017_state_code", "2019_state_code", "2021_state_code"), names_to = "year3", values_to = "state")

# renaming observations
# renamed_PSID$year <- sub("_income", "", renamed_PSID$year)

# merge datasets
# final_data <- merge(tidy_PSID, clean_enrollment, by=c("state","year"))
# treatment_data <- merge(tidy_PSID, small_enrollment, by=c("state","year"))