# create histogram
ggplot(attendance_merged, aes(attendance)) +
  geom_histogram(color = "#000000", fill = "#1f78b4") +
  geom_vline(aes(xintercept = mean(attendance)), color = "#000000", size = 1.25) +
  geom_vline(aes(xintercept = mean(attendance) + sd(attendance)), color = "#000000", size = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = mean(attendance) - sd(attendance)), color = "#000000", size = 1, linetype = "dashed") + labs(
    title = "Histogram of Attendance of Religious Services (2005-2021)",
    x = "Number of Services Attended in Past Year", y="") + theme(axis.title.y=element_blank(),
                                                                  axis.text.y=element_blank(),
                                                                  axis.ticks.y=element_blank()) + theme(panel.background = element_rect(colour = "black")) + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank())
# create map of attendance
mean_attend <- aggregate(attendance_merged$attendance, list(attendance_merged$state), FUN=mean)
mean_attend <- mean_attend |>
  rename(
    fips = Group.1,
    average_attendance = x
  )
mean_attend$average_attendance <- as.numeric(mean_attend$average_attendance)
plot_usmap(data=mean_attend, values="average_attendance") + scale_fill_continuous("Number of Religious Services Attended") + theme(legend.position = "right" ) + labs(title = "Average Yearly Attendance of Religious Services (2005-2021)") + theme(panel.background = element_rect(colour = "black"))
# create map of percent regularly attended
mean_reg_attend <- aggregate(attendance_merged$reg_attend, list(attendance_merged$state), FUN=mean)
mean_reg_attend <- mean_reg_attend |>
  rename(
    fips = Group.1,
    average_reg_attendance = x
  )
mean_reg_attend$average_reg_attendance <- as.numeric(mean_reg_attend$average_reg_attendance)
plot_usmap(data=mean_reg_attend, values="average_reg_attendance") + scale_fill_continuous("% Attending More than 5 Services", limits=c(0,.15), labels = scales::percent_format(accuracy = 1)) + theme(legend.position = "right" ) + labs(title = "% Regularly Attending Religious Services (2005-2021)") + theme(panel.background = element_rect(colour = "black"))

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
plot_usmap(data=expanded_states, values="treat") + scale_fill_brewer(type="div", "Expansion Status (2021)", palette= "Blues") + theme(legend.position = "right" ) + labs(title = "Medicaid Expansion")

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

legend_colors <- c("Expanded Medicaid" = "#1f78b4", "Did Not Expand Medicaid" = "#b2df8a")

ggplot() + geom_line(data=event_study_treat,aes(x=time,y=meanattendance,color="Expanded Medicaid")) + geom_line(data=event_study_untreat,aes(x=time,y=meanattendance,color="Did Not Expand Medicaid")) + labs(x="Year (Relative to 2014)", y="Average Yearly Attendance", color = "Expansion Status") + scale_color_manual(values = legend_colors) + ggtitle("Event Study") + theme_classic() + geom_vline(xintercept=0)

#attendance overtime
attendance_overtime <- attendance_merged |>
  group_by(year) |>
  summarize(meanattendance = mean(attendance)) |>
  ungroup()

ggplot(data=attendance_overtime,aes(x=year,y=meanattendance,group=1)) + geom_line() + labs(x="Average Attendance", y="Year") + ggtitle("Average Religious Event Attendance Over Time")

#medicaid percent change
medicaid_percent_change <- read.csv("raw data/percent_change_in_enrollment.csv")
medicaid_percent_change_before <- medicaid_percent_change |>
  select(Location, Pre.ACA.Average.Monthly.Enrollment, Percent.Change) |>
  mutate(year=2013) |>
  rename(Enrollment = Pre.ACA.Average.Monthly.Enrollment)

medicaid_percent_change_after <- medicaid_percent_change |>
  select(Location, Total.Monthly.Medicaid.CHIP.Enrollment, Percent.Change) |>
  mutate(year=2023) |>
  rename(Enrollment = Total.Monthly.Medicaid.CHIP.Enrollment)

medicaid_percent_change_final <- rbind(medicaid_percent_change_before, medicaid_percent_change_after) |>
  filter(!Location=="District of Columbia") |>
  filter(!Location=="United States") |>
  filter(!Location=="Connecticut") |>
  filter(!Location=="Maine") |>
  rename(State = Location)

medicaid_percent_change_final <- medicaid_percent_change_final |>
  mutate(ln_enrollment = log(as.numeric(Enrollment)))

new_plot <- ggplot(data=medicaid_percent_change_final, aes(x=ln_enrollment,y=State)) + geom_line(aes(group=State)) + geom_point(size=2, aes(color = factor(year))) + scale_x_discrete(limits=10:17, breaks=c(11, 12, 13, 14, 15, 16, 17), labels = scales::comma_format(big.mark = ',', decimal.mark = '.')) + ggtitle("Medicaid Enrollment Before and After Expansion by State")
legend_colors <- c("2013" = "#1f78b4", "2023" = "#b2df8a")
new_plot + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + labs(x="Log(Medicaid Enrollment)", y="State", color = "Year") + scale_color_manual(values = legend_colors)
