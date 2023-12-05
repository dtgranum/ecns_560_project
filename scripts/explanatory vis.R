pacman::p_load(tidyverse, estimatr, broom, summarytools, fixest, binsreg)
library(usmap)
library(ggplot2)
library(sf)
library(tmap)
new <- cleandonationsPSID |>
  mutate(fips <- as.character(cleandonationsPSID$state))
new$fips <- new$`fips <- as.character(cleandonationsPSID$state)`

av_don <- aggregate(new$donations, list(new$fips), FUN=mean)
av_don <- av_don |>
  rename(
    fips = Group.1,
    values = x
  )

plot_usmap(data=av_don, values="values")
