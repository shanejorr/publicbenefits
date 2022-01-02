#' State gross income limits for SNAP
#'
#' For SNAP benefits, states can elect broad-based categorical elibibility. With this election, states
#' can increase the gross income limit from 130% of the poverthy guideline to a higher value.
#' This function returns a state's gross income limit under broad-based categorical eligibility.
#' An NA value is returned for states that have not elected broad-based
#' categorical elibgibility.
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
