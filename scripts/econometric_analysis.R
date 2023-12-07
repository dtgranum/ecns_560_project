ols_fe_attendance <- feols(attendance ~ treatpost | year + state, data = attendance_merged)
etable(ols_fe_attendance, ols_fe_attendance)

# other regressions to try: income subset, log attendance, reg_attend as outcome variable, political control variable

# pivot_longer(cols=c("2005_income", "2007_income", "2009_income", "2011_income", "2013_income", "2015_income", "2017_income", "2019_income", # "2021_income"), names_to="year1", values_to="income") |>
