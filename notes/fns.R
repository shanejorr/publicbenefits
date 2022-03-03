devtools::load_all()

# to do
#   snap_check_parameters - vectorize the checks
#   NULL values are really NA

benefit <- snap_benefits(
  year = c(2022, 2022),
  household_size = c(3, 1),
  monthly_earned_income = c(2000, 10000),
  shelter_expenses = 500,
  monthly_unearned_income = 0,
  state_gross_income_limit = 'NC',
  custom_gross_income_limit = NULL,
  custom_net_income_limit = NULL,
  tfp_region = NULL,
  federal_poverty_guidelines_region = "Contiguous US",
  elderly_disabled_household_member = FALSE,
  medical_expenses = 100,
  dependent_care_deduction = 300,
  child_support_deduction = 0,
  use_homeless_shelter_deduction = FALSE
)

# function ----------------------------

# check parameters
# snap_check_parameters(.data, 'year', 'household_size')

# TFP regions have 3 regions for Alaska, while poverty guideline regions only have one AK region
# if the TFP region is one in alaska, change to simply alaska
poverty_guideline_region <- ifelse(grepl('^[A|a]laska', tfp_region), 'Alaska', tfp_region)

net_income_limit <- federal_poverty_guidelines(year, poverty_guideline_region, household_size) / 12

rm(poverty_guideline_region)

gross_income_limit <- net_income_limit * gross_income_limit

# calculate net income

# you have to calculate net income prior to the shelter deduction before calculating the shelter deduction
net_income_before_shelter <- snap_net_income_prior_shelter(
  year = year, net_income_limit = net_income_limit, monthly_earned_income = monthly_earned_income,
  monthly_unearned_income = monthly_unearned_income, elderly_disabled_household_member = elderly_disabled_household_member,
  excess_medical_deduction = medical_expenses, dependent_care_deduction = dependent_care_deduction,
  child_support_deduction = child_support_deduction
)

net_income <- snap_calculate_net_income(net_income_before_shelter, shelter_expenses, use_homeless_shelter_deduction, year)

rm(net_income_before_shelter)

# determine eligibility
snap_eligibility <- snap_determine_eligibility(
  net_income, monthly_earned_income, monthly_unearned_income, net_income_limit, gross_income_limit, elderly_disabled_household_member
)

# calculate benefit level

# amount is zero if household is not eligible
# so, only calculate SNAP amount for eligible households
snap_benefit_amount <- ifelse(snap_eligibility, snap_benefit_amount(net_income, year, tfp_region, household_size), 0)

return(snap_benefit_amount)

