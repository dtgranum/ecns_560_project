library(dplyr)
library(stringr)
enrollment = read.csv("medicaid_enrollment.csv")

# replace state names with state codes from PSID dataset <- switch to FIPS here (use fips fcn)
rep_states = c('Alabama'='1', 'Arizona'='2', 'Arkansas'='3', 'California'='4', 'Colorado'='5', 'Connecticut'='6', 'Delaware'='7', 'Dist. Of Col.'='8', 'Florida'='9', 'Georgia'='10', 'Idaho'='11', 'Illinois'='12', 'Indiana'='13', 'Iowa'='14', 'Kansas'='15', 'Kentucky'='16', 'Louisiana'='17', 'Maine'='18', 'Maryland'='19', 'Massachusetts'='20', 'Michigan'='21', 'Minnesota'='22', 'Mississippi'='23', 'Missouri'='24', 'Montana'='25', 'Nebraska'='26', 'Nevada'='27', 'New Hampshire'='28', 'New Jersey'='29', 'New Mexico'='30', 'New York'='31', 'North Carolina'='32', 'North Dakota'='33', 'Ohio'='34', 'Oklahoma'='35', 'Oregon'='36', 'Pennsylvania'='37', 'Rhode Island'='38', 'South Carolina'='39','South Dakota'='40', 'Tennessee'='41', 'Texas'='42', 'Utah'='43', 'Vermont'='44', 'Virginia'='45', 'Washington'='46', 'West Virginia'='47', 'Wisconsin'='48', 'Wyoming'='49', 'Alaska'='50', 'Hawaii'='51')
enrollment$State <- str_replace_all(enrollment$State, rep_states)

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

# create dummy variable indicating medicaid expansion
clean_enrollment$expansion <- ifelse(clean_enrollment$expanded_enrollees > 0 , 1, 0)
small_enrollment <- subset(clean_enrollment, select=c("state","year","expansion"))
# filter duplicate rows
final_enrollment <- unique(small_enrollment)

# create post variable <- rewrite this code so it's not hardcoded
final_enrollment <- final_enrollment |>
  mutate(post = case_when(
    state == 2  & year >= 2014 ~ 1,
    state == 3 & year >= 2014 ~ 1,
    state == 4 & year >= 2014 ~ 1,
    state == 5 & year >= 2014 ~ 1,
    state == 6 & year >= 2014 ~ 1,
    state == 7 & year >= 2014 ~ 1,
    state == 8 & year >= 2014 ~ 1,
    state == 11 & year >= 2020 ~ 1,
    state == 12 & year >= 2014 ~ 1,
    state == 13 & year >= 2015 ~ 1,
    state == 14 & year >= 2014 ~ 1,
    state == 16 & year >= 2014 ~ 1,
    state == 17 & year >= 2016 ~ 1,
    state == 18 & year >= 2019 ~ 1,
    state == 19 & year >= 2014 ~ 1,
    state == 20 & year >= 2014 ~ 1,
    state == 21 & year >= 2014 ~ 1,
    state == 22 & year >= 2014 ~ 1,
    state == 24 & year >= 2021 ~ 1,
    state == 25 & year >= 2016 ~ 1,
    state == 26 & year >= 2020 ~ 1,
    state == 27 & year >= 2014 ~ 1,
    state == 28 & year >= 2014 ~ 1,
    state == 29 & year >= 2014 ~ 1,
    state == 30 & year >= 2014 ~ 1,
    state == 31 & year >= 2014 ~ 1,
    state == 33 & year >= 2014 ~ 1,
    state == 34 & year >= 2014 ~ 1,
    state == 35 & year >= 2021 ~ 1,
    state == 36 & year >= 2014 ~ 1,
    state == 37 & year >= 2015 ~ 1,
    state == 38 & year >= 2014 ~ 1,
    state == 43 & year >= 2020 ~ 1,
    state == 44 & year >= 2014 ~ 1,
    state == 45 & year >= 2019 ~ 1,
    state == 46 & year >= 2014 ~ 1,
    state == 47 & year >= 2014 ~ 1,
    state == 50 & year >= 2015 ~ 1,
    state == 51 & year >= 2014 ~ 1,
    TRUE ~ 0
  ))

final_enrollment <- final_enrollment |>
  mutate(treat = ifelse(
    state %in% c(2, 3, 4, 5, 6, 7, 8, 11, 12, 13, 14, 16, 17, 18, 19, 20, 21, 22, 24, 25, 26, 27, 28, 29, 30, 31, 33, 34, 35, 36, 37, 38, 43, 44, 45, 46, 47, 50, 51),
    1,
    0))
  


    
    
  