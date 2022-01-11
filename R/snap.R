#' State gross income limits for SNAP
#'
#' For SNAP benefits, states can elect broad-based categorical eligibility. With this election, states
#' can increase the gross income limit from 130% of the poverty guideline to a higher value.
#' This function returns a state's gross income limit under broad-based categorical eligibility.
#' An NA value is returned for states that have not elected broad-based
#' categorical eligibility.
#'
#' @param year Year to use to calculate threshold, as numeric. Data for 2021 is available.
#' @param state Two letter abbreviation of the state, capitalized. Use 'DC' for Washington, DC;
#'      'GU' for Guam, and 'VI' for the US Virgin Islands.
#'
#' @return A single number representing the state's gross income limit, based on the percentage of the
#'      federal poverty guidelines. For example, 2 represents 200% of the federal poverty guidelines,
#'      while 1.85 represents 185% of the federal poverty guidelines.
#'
#' @examples
#' state_gross_income_limits(2021, 'NC')
#'
#' @section Source:
#' FNS Broad-Based Categorical Eligibility Chart: \url{https://fns-prod.azureedge.net/sites/default/files/resource-files/BBCE%20States%20Chart%20(July%202021).pdf}
#'
#' @export
state_gross_income_limits <- function(year, state) {

  # check parameters

  # make sure the year parameter is an available year
  years <- 2021
  if (!(year %in% years)) stop(paste0("`year` must be either ", paste0(years, collapse = ", "), "."))

  check_state(state)

  year <- as.character(year)

  state <- toupper(state)

  state_gross_income_limits <- list(
    '2021' = list(
      AL = 1.3, AZ = 1.85, CA = 2, CO = 2, CT = 1.85, DE = 2, DC = 2, FL = 200,
      GA = 1.3, GU = 1.65, HI = 2, ID = 1.3, IL = 1.65, IN = 1.3, IA = 1.6,
      KY = 2, LA = 1.3, ME = 1.85, MA = 2, MI = 2, MN = 1.65, MT = 2, NE = 1.3,
      NV = 2, NJ = 1.85, NM = 1.65, NC = 2, ND = 2, OH = 1.3, OK = 1.3, OR = 1.85,
      PA = 1.6, RI = 1.85, SC = 1.3, TX = 1.65, VI = 1.85, VA = 2, WA = 2, WV = 2,
      WI = 2
    )
  )

  # check and see if state has broad based categorical eligibility
  # if it does not, return NA, if it does, return the value
  broad_based_states <- names(state_gross_income_limits[[year]])

  if (!state %in% broad_based_states) {
    state_limit <-  NA_real_
  } else if (state %in% broad_based_states) {
    state_limit <- state_gross_income_limits[[year]][[state]]
  } else {
    stop("There was an unexpected error in locating the state. Sorry.")
  }

  return(state_limit)

}

#' Calculate net income prior to shelter deduction
#'
#' @keywords internal
net_income_prior_shelter <- function(
  year, net_income_limit, monthly_earned_income, monthly_unearned_income, elderly_disabled_household_member,
  excess_medical_deduction, dependent_care_deduction, child_support_deduction
  ) {

  year <- as.character(year)

  # standard deduction is a percent of the net income threshold (7 CFR 273.9(d)(1))
  std_deduction <- net_income_limit * income_deduction(year, 'standard')

  # Earned Income Deduction is a percent of earned income (7 CFR 273.9(d)(2))
  earned_income_deduction <- monthly_earned_income * income_deduction(year, 'earned_income')

  # Excess medical deduction is medical costs exceeding a threshold (7 CFR 273.9(d)(3))
  # only applies to elderly or disabled
  if (elderly_disabled_household_member) {

    excess_medical_deduction <- medical_expenses - income_deduction(year, 'excess_medical')
    excess_medical_deduction <- if (excess_medical_deduction < 0) 0 else excess_medical_deduction

  } else {
    excess_medical_deduction <- 0
  }

  # total gross income
  net_income_before_shelter <- monthly_earned_income + monthly_unearned_income

  # remove all deductions except shelter deduction
  net_income_before_shelter <- net_income_before_shelter - std_deduction - earned_income_deduction - excess_medical_deduction - dependent_care_deduction - child_support_deduction

  return(net_income_before_shelter)

}

#' SNAP income deductions
#'
#' Values that control income deductions. These values need to be checked yearly at 7 CFR 273.9(d).
#'
#' @param year Numeric. Year calculating benefits for.
#' @param deduction String. Income deduction. One of 'standard', 'earned_income', 'excess_medical',
#'    'dependent_care', 'child_support', 'homeless_shelter', 'excess_shelter'
#'
#' @return A single number representing a value used to compute the income deduction.
#' @keywords internal
income_deduction <- function(year, deduction) {

  deduction_value <- list(
  # standard deduction is a percentage of the net income eligibility value. 7 CFR 273.9(d)(1)
    'standard' = c(
      '2021' = .0831
    ),
    # percentage of earned income (7 CFR 273.9(d)(2))
    'earned_income' = c(
      '2021' = .2
    ),
    # medical expenses exceeding a threshold (7 CFR 273.9(d)(3))
    'excess_medical' = c(
      '2021' = 35
    ),
    # homeless shelter deduction is a set amount each year (7 CFR 273.9(d)(6)(i))
    'homeless_shelter' = c(
      '2021' = 143
    ),
    # excess shelter deduction is a percentage of net income, after deductions (7 CFR 273.9(d)(6)(ii))
    'excess_shelter' = c(
      '2021' = .5
    )
  )

  deduction_value[[deduction]][year]

}

