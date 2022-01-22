devtools::load_all()

# parameters -------------------------------
year <- 2021

gross_income_limit <- 2

monthly_earned_income <- 1200

monthly_unearned_income <- 0

household_size <- 3

# age threshold 60
# can meet with other criteria as well (7 CFR 271.2)
elderly_disabled_household_member <- FALSE

# for excess medical deduction (7 CFR 273.9(d)(3))
medical_expenses <- 0

# (7 CFR 273.9(d)(4))
dependent_care_deduction <- 200

# only available in states that provide a deduction instead of income exclusion
child_support_deduction <- 0

# the homeless shelter deduction is a set amount and a state must elect to allow its use.
# if it is used, you cannot deduct shelter expenses (7 CFR 273.9(d)(6)(i))
use_homeless_shelter_deduction <- FALSE

# can deduct a percentage of shelter expenses, with the percentage coming from percent of net income after deductions.
# expenses cannot exceed area limit, unless elderly or disabeled person is in home
shelter_expenses <- 600

# function ----------------------------

net_income_limit <- federal_poverty_guidelines(year, 'contiguous us', household_size) / 12

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

# determine eligibility
snap_eligibility <- snap_determine_eligibility(
  net_income, monthly_earned_income, monthly_unearned_income, net_income_limit, gross_income_limit, elderly_disabled_household_member
)

# calculate benefit level
# (A) Except as provided in paragraphs (a)(1), (e)(2)(iii) and (e)(2)(vi) of this section,
# the household's monthly allotment shall be equal to the maximum SNAP allotment for the
# household's size reduced by 30 percent of the household's net monthly income as calculated in
# paragraph (e)(1) of this section.

# Maximum benefit is based on Thrift Food Plan values. 7 CFR 273.10(e)(1)(i)
