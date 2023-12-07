# attendance regression with all income levels
ols_attendance <- feols(attendance ~ treatpost | year + state, data = attendance_merged)
table1 <- tbl_regression(ols_attendance)
table1 <- table1 |>
  modify_header(label = "**Variable**") |>
  as_gt() |>
  gt::tab_header(title = "Regression 1. Attendance Regression Model")

# attendance regression with restricted income levels
attendance_merged_eligible <- attendance_merged |>
  filter(income < 40000)
ols_eligible <- feols(attendance ~ treatpost | year + state, data = attendance_merged_eligible)
table2 <- tbl_regression(ols_eligible)
table2 <- table2 |>
  modify_header(label = "**Variable**") |>
  as_gt() |>
  gt::tab_header(title = "Regression 2. Low-Income Attendance Regression Model")

# regular attendance regression with all income levels
ols_reg_attendance <- feols(reg_attend ~ treatpost | year + state, data = attendance_merged)
table3 <- tbl_regression(ols_reg_attendance)
table3 <- table3 |>
  modify_header(label = "**Variable**") |>
  as_gt() |>
  gt::tab_header(title = "Regression 3. Regular Attendance Regression Model")

