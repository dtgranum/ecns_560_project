library(dplyr)
library(stringr)
library(cdlTools)
# setwd("/ecns_560_project")
enrollment = read.csv("raw data/medicaid_enrollment.csv")

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



    
    
  