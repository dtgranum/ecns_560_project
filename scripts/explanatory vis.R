pacman::p_load(tidyverse, estimatr, broom, summarytools, fixest, binsreg)
library(usmap)
library(ggplot2)
library(sf)
library(tmap)
library(viridis)


maps_data <- cleandonationsPSID |>
  mutate(fips <- as.character(cleandonationsPSID$fips))
new$fips <- new$`fips <- as.character(cleandonationsPSID$fips)`

av_don <- aggregate(new$donations, list(new$fips), FUN=mean)
av_don <- av_don |>
  rename(
    fips = Group.1,
    average_donations = x
  )

plot_usmap(data=av_don, values="average_donations")

# do this in a better way when treat variable is in psid dataset
av_don <- av_don |>
  mutate(expanded_state = ifelse(
    fips %in% c(4, 5, 6, 8, 9, 10, 11, 16, 17, 18, 19, 21, 22, 23, 24, 
                25, 26, 27, 29, 30, 31, 32, 33, 34, 35, 36, 38, 39, 40, 41, 42, 43, 44, 49, 50, 51, 52, 53, 54),
    1,
    0))

expanded = av_don |>
  select(fips, expanded_state)

plot_usmap(data=av_don, values="expanded_state")
