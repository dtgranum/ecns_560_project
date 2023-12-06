pacman::p_load(tidyverse, estimatr, broom, summarytools, fixest, binsreg)
library(usmap)
library(ggplot2)
library(sf)
library(tmap)
library(RColorBrewer)

# create map of attendance
mean_attend <- aggregate(attendance_merged$attendance, list(attendance_merged$state), FUN=mean)
mean_attend <- mean_attend |>
  rename(
    fips = Group.1,
    average_attendance = x
  )
mean_attend$average_attendance <- as.character(mean_attend$average_attendance)
plot_usmap(data=mean_attend, values="average_attendance") + scale_fill_continuous(name = "Number of Religious Services Attended") + theme(legend.position = "right" ) + labs(title = "Average Yearly Attendance of Religious Services (2005-2021)")

# create map of medicaid expansion
expanded_states <- attendance_merged |>
  select(state, treat)
expanded_states <- unique(expanded_states)
expanded_states <- expanded_states |>
  rename(
    fips = state
  )
expanded_states$treat <- as.character(expanded_states$treat)
expanded_states$treat <- replace(expanded_states$treat, expanded_states$treat=="1", "Expanded Medicaid")
expanded_states$treat <- replace(expanded_states$treat, expanded_states$treat=="0", "Did Not Expand Medicaid")
plot_usmap(data=expanded_states, values="treat") + scale_fill_brewer(type="div", "Expansion Status (2021)", palette= "Greens") + theme(legend.position = "right" ) + labs(title = "Medicaid Expansion")

#create event study
event_study_treat <- attendance_merged |>
  mutate(year1 = as.numeric(year)) |>
  mutate(time = (year1-2014)) |>
  filter(treat==1) |>
  group_by(time) |>
  mutate(meanattendance = mean(attendance))

event_study_untreat <- attendance_merged |>
  mutate(year1 = as.numeric(year)) |>
  mutate(time = (year1-2014)) |>
  filter(treat==0) |>
  group_by(time) |>
  mutate(meanattendance = mean(attendance))

legend_colors <- c("Treated" = "blue", "Untreated" = "red")

ggplot() + geom_line(data=event_study_treat,aes(x=time,y=meanattendance,color="Treated")) + geom_line(data=event_study_untreat,aes(x=time,y=meanattendance,color="Untreated")) + labs(x="Time", y="Average Attendance", color = "Legend") + scale_color_manual(values = legend_colors) + ggtitle("Event Study") + theme_classic()

#attendance overtime
attendance_overtime <- attendance_merged |>
  group_by(year) |>
  summarize(meanattendance = mean(attendance)) |>
  ungroup()

ggplot(data=attendance_overtime,aes(x=year,y=meanattendance,group=1)) + geom_line() + labs(x="Average Attendance", y="Year") + ggtitle("Average Religious Event Attendance Overtime")
  
