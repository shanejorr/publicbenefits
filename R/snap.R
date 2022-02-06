#' SNAP benefit amounts for households.
#'
#' Calculates the total SNAP benefits for households.
#'
#' @export
snap_benefits <- function(
  .data, year, household_size, state_gross_income_limit = NULL, custom_gross_income_limit = NULL, custom_net_income_limit = NULL,
  tfp_region = NULL, federal_poverty_guideleines_region = "Contiguous US"
  ) {

  # check parameters
  snap_check_parameters(.data, 'year', 'household_size')

  # users can either define their own gross income limit or specify a state and use it
  # gross income limits depends on the state because some states elect categorical eligibility
  # and this raises the gross income limit. 7 CFR § 273.9(a)
  if (!is.null(state_gross_income_limit)) {
    gross_income_limit <- snap_state_gross_income_limits(year, state_gross_income_limit)
  } else if (!is.null(custom_gross_income_limit)) {
    gross_income_limit <- custom_gross_income_limit
  } else {
    stop("`state_gross_income_limit` and `custom_gross_income_limit` cannot both be NULL.")
  }

  tfp_region <- tfp_region_rules(tfp_region, state_gross_income_limit)

  # calculate federal poverty region based on TFP region
  # will use TFP region, with minor corrections, if it is specified, otherwise, use state
  poverty_guideline_region <- poverty_guidelines_region_rules(tfp_region)

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

}

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
#' snap_state_gross_income_limits(2021, 'NC')
#'
#' @section Source:
#' FNS Broad-Based Categorical Eligibility Chart: \url{https://fns-prod.azureedge.net/sites/default/files/resource-files/BBCE%20States%20Chart%20(July%202021).pdf}
#'
#' @export
snap_state_gross_income_limits <- function(year, state) {

  check_state(state)

  year <- as.character(year)

  state <- toupper(state)

  state_gross_income_limits <- list(
    '2022' = list(
      AL = 1.3, AZ = 1.85, CA = 2, CO = 2, CT = 1.85, DE = 2, DC = 2, FL = 200,
      GA = 1.3, GU = 1.65, HI = 2, ID = 1.3, IL = 1.65, IN = 1.3, IA = 1.6,
      KY = 2, LA = 1.3, ME = 1.85, MA = 2, MI = 2, MN = 1.65, MT = 2, NE = 1.3,
      NV = 2, NJ = 1.85, NM = 1.65, NC = 2, ND = 2, OH = 1.3, OK = 1.3, OR = 1.85,
      PA = 1.6, RI = 1.85, SC = 1.3, TX = 1.65, VI = 1.85, VA = 2, WA = 2, WV = 2,
      WI = 2, Federal = 1.3
    )
  )

  # check and see if state has broad based categorical eligibility
  # if it does not, return NA, if it is, return the value
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
#' Overall net income calculations are in 7 CFR 273.10(e)(1)
#'
#' @keywords internal
snap_net_income_prior_shelter <- function(
  year, net_income_limit, monthly_earned_income, monthly_unearned_income, elderly_disabled_household_member,
  excess_medical_deduction, dependent_care_deduction, child_support_deduction
  ) {

  year <- as.character(year)

  # standard deduction is a percent of the net income threshold (7 CFR 273.9(d)(1))
  std_deduction <- net_income_limit * snap_income_deduction(year, 'standard')

  # Earned Income Deduction is a percent of earned income (7 CFR 273.9(d)(2))
  earned_income_deduction <- monthly_earned_income * snap_income_deduction(year, 'earned_income')

  # Excess medical deduction is medical costs exceeding a threshold (7 CFR 273.9(d)(3))
  # only applies to elderly or disabled
  if (elderly_disabled_household_member) {

    excess_medical_deduction <- medical_expenses - snap_income_deduction(year, 'excess_medical')
    excess_medical_deduction <- if (excess_medical_deduction < 0) 0 else excess_medical_deduction

  } else {
    excess_medical_deduction <- 0
  }

  # total gross income
  total_gross_income <- monthly_earned_income + monthly_unearned_income

  # remove all deductions except shelter deduction
  net_income_before_shelter <- total_gross_income - std_deduction - earned_income_deduction - excess_medical_deduction - dependent_care_deduction - child_support_deduction

  return(net_income_before_shelter)

}

#' Calculate net income
#'
#' Net income is calculated by taking earned and unearned income, subtracting deductions, and then
#' calculating and subtracting the excess shelter deduction. Steps to calculate net income are in 7 CFR 273.10(e)(1)(i).
#'
#' @return Net income value.
#' @keywords internal
snap_calculate_net_income <- function(net_income_before_shelter, shelter_expenses, use_homeless_shelter_deduction, year) {

  # shelter deduction
  # deduction is expenses in excess of 50% of shelter costs ((7 CFR 273.9(d)(6)(ii)))
  amount_to_subtract_from_shelter_expense <- net_income_before_shelter * snap_income_deduction(year, 'excess_shelter')

  excess_shelter_deduction <- shelter_expenses - amount_to_subtract_from_shelter_expense

  excess_shelter_deduction <- ifelse(excess_shelter_deduction < 0, 0, excess_shelter_deduction)

  # use the homeless shelter deduction instead of excess shelter if specified
  excess_shelter_deduction <- ifelse(use_homeless_shelter_deduction , snap_income_deduction(year, 'homeless_shelter'), excess_shelter_deduction)

  # subtract excess shelter costs from net income before shelter deduction to arrive at net income
  # 7 CFR 273.10(e)(1)(i)(I)
  net_income <- net_income_before_shelter - excess_shelter_deduction

  return(net_income)

}

#' Calculate Eligibility
#'
#' @section Eligibility Criteria:
#' Disables or elderly compare their net income with net income limit (7 CFR 273.10(e)(2)(i)(A)).
#' Non-elderly compare their net and gross incomes (7 CFR 273.10(e)(2)(i)(A)). to the limits.
#'
#' @return Boolean representing whether household is eligible for benefits
#' @keywords internal
snap_determine_eligibility <- function(net_income, monthly_earned_income, monthly_unearned_income, net_income_limit, gross_income_limit, elderly_disabled_household_member) {

  # total gross income is earned and unearned income (7 CFR 273.10(e)(1)(i)(A))
  total_gross_income <- monthly_earned_income + monthly_unearned_income

  # check whether net and gross incomes are under the income limits
  meet_net_income <- net_income < net_income_limit
  meet_gross_income <- total_gross_income < gross_income_limit

  eligibility <- ifelse(
    elderly_disabled_household_member, meet_net_income, meet_net_income & meet_gross_income
  )

  return(eligibility)
}

#' Maximum benefit values for SNAP
#'
#' Maximum SNAP benefits depend on year, region, and household size. They are based on Thrift Food Plan (TFP) amounts.
#' This function returns maximum SNAP benefits for a given household size and TFP region.
#'
#' @param year Fiscal year to use to calculate threshold, as numeric. Fiscal year starts Oct. 1.
#' @param region Region to calculate maximum benefits for. Must be one of :
#'      'Contiguous US', 'Guam', 'Virgin Islands', 'Hawaii', 'Alaska Urban', 'Alaska Rural 1', 'Alaska Rural 2'.
#'
#' @return A single number representing the maximum amout a household size can receive in SNPA benefits.
#'
#' @examples
#' snap_maximum_benefit_amounts(2022, 'Contiguous US')
#'
#' @section Source:
#' Maximum SNAP benefits: \url{https://fns-prod.azureedge.net/sites/default/files/resource-files/2022-SNAP-COLA-%20Maximum-Allotments.pdf}
#'
#' @export
snap_maximum_benefit_amounts <- function(year, region) {

  year <- as.character(year)

  # check geographies
  required_regions <- c('Contiguous US', 'Guam', 'Virgin Islands', 'Hawaii', 'Alaska Urban', 'Alaska Rural 1', 'Alaska Rural 2')
  accepted_regions <- tolower(required_regions)

  if (!(region %in% c(required_regions, accepted_regions))) {
    stop(paste0("`region` must be either ", paste0(required_regions, collapse = ", "), "."))
  }

  region <- tolower(region)

  snap_maximums <- list(
    # benefits based on TFP region
    # https://fns-prod.azureedge.net/sites/default/files/resource-files/2022-SNAP-COLA-%20Maximum-Allotments.pdf
    '2022' = list(

      'contiguous us' = list(
        households = c('1' = 250, '2' = 459, '3' = 658, '4' = 835, '5' = 992, '6' = 1190, '7' = 1316, '8' = 1504),
        additional_person = 188
      ),

      'guam' = list(
        households = c('1' = 369, '2' = 677, '3' = 969, '4' = 1231, '5' = 1462, '6' = 1754, '7' = 1939, '8' = 2216),
        additional_person = 277
      ),

      'virgin islands' = list(
        households = c('1' = 322, '2' = 590, '3' = 845, '4' = 1074, '5' = 1275, '6' = 1530, '7' = 1691, '8' = 1933),
        additional_person = 242
      ),

      'alaska urban' = list(
        households = c('1' = 322, '2' = 591, '3' = 846, '4' = 1074, '5' = 1276, '6' = 1531, '7' = 1692, '8' = 1934),
        additional_person = 242
      ),

      'alaska rural 1' = list(
        households = c('1' = 411, '2' = 753, '3' = 1079, '4' = 1370, '5' = 1627, '6' = 1952, '7' = 2158, '8' = 2466),
        additional_person = 308
      ),

      'alaska rural 2' = list(
        households = c('1' = 500, '2' = 917, '3' = 1313, '4' = 1667, '5' = 1980, '6' = 2376, '7' = 2626, '8' = 3002),
        additional_person = 375
      ),

      'hawaii' = list(
        households = c('1' = 472, '2' = 865, '3' = 1239, '4' = 1573, '5' = 1868, '6' = 2242, '7' = 2478, '8' = 2832),
        additional_person = 354
      )
    )
  )

  snap_maximums[[year]][[region]]

}

#' Maximum SNAP Benefits
#'
#' Calculates the maximum SNAP benefits for a given year, region, and household size.
#' Maximum SNAP benefits are based on Thrift Food Plan (TFP) amounts. 7 CFR 273.10(e)(1)(i).
#'
#' @param year Whole number representing the fiscal year. Fiscal years start Oct. 1st.
#' @param region Region for calculating max benefits. One of:
#'      'Contiguous US', 'Guam', 'Virgin Islands', 'Hawaii', 'Alaska Urban', 'Alaska Rural 1', 'Alaska Rural 2'
#' @param household_size Whole number represent household size.
#'
#' @section Source:
#' TFP amounts: \url{https://fns-prod.azureedge.net/sites/default/files/resource-files/2022-SNAP-COLA-%20Maximum-Allotments.pdf}
#'
#' @export
snap_maximum_benefits <- function(year, region, household_size) {

  # convert to character so we can look up numbers in named list
  household_size_character <- as.character(household_size)

  # list with maximum benefits per household size and additional benefit value per person
  max_benefits_list <- snap_maximum_benefit_amounts(year, region)

  if (household_size %in% 1:8) {
    # max benefit for households with 8 or fewer people
    max_snap_benefit <- max_benefits_list[['households']][[household_size_character]]
  } else {
    # max benefit for households with more than 8 people
    household_eight_benefit <- max_benefits_list[['households']][['8']]
    additional_person_benefit <- max_benefits_list[['additional_person']]
    num_beyond_eight <- household_size - 8
    max_snap_benefit <- household_eight_benefit + (additional_person_benefit * num_beyond_eight)
  }

  return(max_snap_benefit)
}

#' SNAP benefit amount
#'
#' Calculate the amount of benefits for SNAP households. Return 0 if household is not eligible for benefits.
#'      Except as provided in paragraphs (a)(1), (e)(2)(iii) and (e)(2)(vi) of this section,
#'      the household's monthly allotment shall be equal to the maximum SNAP allotment for the
#'      household's size reduced by 30 percent of the household's net monthly income as calculated in
#'      paragraph (e)(1) of this section. 7 CFR 273.10(e)(2)(ii)(A)
#'
#' @keywords internal
snap_benefit_amount <- function(net_income, year, tfp_region, household_size) {

  # maximum benefit amount household is eligible for
  maximum_snap_benefit <- snap_maximum_benefits(year, tfp_region, household_size)

  return(maximum_snap_benefit - (net_income * .3))

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
snap_income_deduction <- function(year, deduction) {

  year <- as.character(year)

  deduction_value <- list(
  # standard deduction is a percentage of the net income eligibility value. 7 CFR 273.9(d)(1)
    'standard' = c(
      '2022' = .0831
    ),
    # percentage of earned income (7 CFR 273.9(d)(2))
    'earned_income' = c(
      '2022' = .2
    ),
    # medical expenses exceeding a threshold (7 CFR 273.9(d)(3))
    'excess_medical' = c(
      '2022' = 35
    ),
    # homeless shelter deduction is a set amount each year (7 CFR 273.9(d)(6)(i))
    'homeless_shelter' = c(
      '2022' = 143
    ),
    # excess shelter deduction is a percentage of net income, after deductions (7 CFR 273.9(d)(6)(ii))
    'excess_shelter' = c(
      '2022' = .5
    )
  )

  deduction_value <- unname(deduction_value[[deduction]][year])

  if (is.na(deduction)) stop(paste0("Could not calculate ", deduction, " please check that the year is correct."), call. = FALSE)

  return(deduction_value)

}

#' Check parameters for SNAP function
#'
#' @keywords internal
snap_check_parameters <- function(year, household_size, state_gross_income_limit, custom_gross_income_limit) {

  # year
  if (!is.numeric(year)) stop("`year` must be numeric.", call. = FALSE)

  first_year <- 2022
  if (any(year < first_year)) stop(paste0(first_year, " is the first year available. You have a year earlier in your data."), call. = FALSE)

  # household_size
  if (!is.numeric(.data[[household_size]])) stop("The `household_size` column must be numeric.", call. = FALSE)
  if (any(.data[[household_size]] %% 1 != 0)) stop("All `household_size` values must be integers. They cannot be decimals.", call. = FALSE)
  if (any(is.na(.data[[household_size]]))) stop("You have NA `household_size` values. All rows must have a household size.", call. = FALSE)

  # cannot have state and custom gross income limits
  if (!is.null(state_gross_income_limits) & !is.null(custom_gross_income_limit)) {
    stop('Cannot have values for both `state_gross_income_limits` and `custom_gross_income_limit`. One must be NULL', call. = FALSE)
  }

  # must have at least one gross income column
  if (is.null(state_gross_income_limits) & is.null(custom_gross_income_limit)) {
    stop('`state_gross_income_limits` and `custom_gross_income_limit` cannot both be NULL. You must have values for one of them.', call. = FALSE)
  }

  # the following parameters cannot have missing values
  if (any(is.na(year))) stop("`year` has missing values. It cannot have missing values.", call. = FALSE)
  if (any(is.na(household_size))) stop("`household_size` has missing values. It cannot have missing values.", call. = FALSE)

  if (!is.null(state_gross_income_limits)){
    if(any(is.na(state_gross_income_limits))) stop("`state_gross_income_limits` has missing values. It cannot have missing values.", call. = FALSE)
  }

  if (!is.null(custom_gross_income_limit)){
    if(any(is.na(custom_gross_income_limit))) stop("`custom_gross_income_limit` has missing values. It cannot have missing values.", call. = FALSE)
  }

  if (!is.null(tfp_region)) {
    if (any(is.na(tfp_region))) stop("`custom_gross_income_limit` has missing values. It cannot have missing values.", call. = FALSE)
  }

  return(NULL)
}

#' TFP region
#'
#' If TFP is specified, use this value. If it is not specified, infer it from the state. If the state
#' is not specified, default to Contiguous US.
tfp_region_rules <- function(tfp_region, state) {

  if (!is.null(tfp_region)) {

    tfp <- tfp_region

  } else if (!is.null(state)) {

    tfp <- map_states_regions(state)

  }  else {

    tfp <- 'Contiguous US'

    warning(paste0(
      "Both the `state_gross_income_limit` and `tfp_region` are NULL. ",
      "TFP region cannot be calculate when both are NULL. ",
      "Therefore, the default of 'Contiguous US' is assumed for the TFP region. ",
      "If you do not want this default, please include values for either `state_gross_income_limit` or `tfp_region`.")
    )

  }

  return(tfp)

}

#' Poverty Guidelines Regions
#'
#' Business rules for calculating federal poverty guidelines region. If the TFP region is specified, use it
#' since it largely mirrors federal poverty regions. If TFP region is not specified, use state.
#' We don't need to recalculate for state here because TFP region already uses state if there is no
#' TFP region. Therefore, for this function, we only need to calculate based on TFP region, knowing
#' that we will be relying on state if there is no TFP region.
#'
#' @keywords internal
poverty_guidelines_region_rules <- function(tfp_region) {

  # Guam and Virgin Islands are contiguous US
  # Alaska only has one region
  dplyr::case_when(
    stringr::str_detect(tfp_region, "^Conti|^Gua|^Virg") ~ 'Contiguous US',
    stringr::str_detect(tfp_region, "^Alaska") ~ 'Alaska',
    TRUE ~ stop('Could not calcualte poverty guidelines region. Please the `state_gross_income_limit` and / or `tfp_region` values are correct.')
  )

}
