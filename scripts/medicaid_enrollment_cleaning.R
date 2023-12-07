library(tidyverse)
library(stringr)
library(cdlTools)
library(readxl)

# setwd("/ecns_560_project")

#psid cleaning
#read in data
raw_PSID <- read_excel("raw data/J325858.xlsx")

# label variables
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

# make tidy
attendancePSID <- renamed_PSID |>
  pivot_longer(cols=c("2005_attendance", "2011_attendance", "2017_attendance", "2019_attendance", "2021_attendance"), names_to="year", values_to="attendance") |>
  pivot_longer(cols = c("2005_current_state", "2007_current_state", "2009_current_state", "2011_current_state", "2013_current_state", "2015_current_state", "2017_current_state", "2019_current_state", "2021_current_state"), names_to = "year4", values_to = "state") |>
  select(attendance, year, state)

attendancePSID$year <- sub("_attendance", "", attendancePSID$year)
attendancePSID$state <- as.character(attendancePSID$state)

cleanattendancePSID <- attendancePSID |>
  filter(!attendance == 98) |>
  filter(!attendance == 99) |>
  filter(!is.na(attendance)) |>
  filter(!is.na(state)) |>
  filter(!state == 99) |>
  filter(!state == 0)

attendance_merged <- left_join(cleanattendancePSID, final_enrollment, by=c("state","year"))

attendance_merged <- attendance_merged |>
  group_by(state) |>
  mutate(treat = as.numeric(any(treatpost == 1))) |>
  ungroup()

attendance_merged$treat <- replace(attendance_merged$treat, is.na(attendance_merged$treat), 0)
attendance_merged$treatpost <- replace(attendance_merged$treatpost, is.na(attendance_merged$treatpost), 0)

# create dummy variable indicating regular services attendance
attendance_merged$reg_attend <- ifelse(attendance_merged$attendance > 5 , 1, 0)

#medicaid cleaning
#reading in data
enrollment <- read.csv("raw data/medicaid_enrollment.csv")

# replace state names with FIPS codes
enrollment$State <- fips(enrollment$State)

# delete extraneous columns
clean_enrollment <- subset(enrollment, select = -c(Total.VIII.Group.Newly.Eligible.Enrollees:Updated.Month, Enrollment.Month, Notes))

# variable name cleaning
clean_enrollment = clean_enrollment |>
  rename(
    medicaid_enrollees=Total.Medicaid.Enrollees,
    expanded_enrollees=Total.VIII.Group.Enrollees,
    state=State,
    year=Enrollment.Year
  )

# make number of expanded enrollees numeric variable and make N/A values 0
clean_enrollment$expanded_enrollees <- as.numeric(gsub(",","",clean_enrollment$expanded_enrollees))
clean_enrollment$expanded_enrollees[is.na(clean_enrollment$expanded_enrollees)] <- 0
clean_enrollment$year <- as.character(clean_enrollment$year)
clean_enrollment$state <- as.character(clean_enrollment$state)

# create dummy variable indicating medicaid expansion
clean_enrollment$treatpost <- ifelse(clean_enrollment$expanded_enrollees > 0 , 1, 0)

# create dummy variable indicating treatment group
clean_enrollment <- clean_enrollment |>
  group_by(state) |>
  mutate(treat = as.numeric(any(treatpost == 1))) |>
  ungroup()

small_enrollment <- subset(clean_enrollment, select=c("state","year","treatpost"))

# filter duplicate rows
final_enrollment <- unique(small_enrollment)



    
    
  