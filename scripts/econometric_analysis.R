ols_fe_attendance <- feols(attendance ~ treatpost | year + state, data = attendance_merged)
etable(ols_fe_attendance, ols_fe_attendance)

# other regressions to try: income subset, log attendance, reg_attend as outcome variable, political & population control variables
attendance_merged_eligible <- attendance_merged |>
  filter(income < 21000)

ols <- feols(attendance ~ treatpost | year + state, data = attendance_merged_eligible)
etable(ols, ols_fe_attendance)

