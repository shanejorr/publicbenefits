#' Federal poverty guideline amounts
#'
#' Calculate the federal poverty guidelines from the year, geographic area, and household size.
#' Poverty guidelines are used to calculate eligibility for public benefits. These are not the poverty
#' thresholds, which are the statistics used to calculate official poverty levels.
#'
#' @param year Year to use to calculate threshold, as numeric. Data for 2016 to 2021 is available.
#' @param geography There are different poverty guidelines for the contiguous 48 states, Alaska, and Hawaii.
#'      Use 'Contiguous US' for the contiguous 48 states, 'Alaska' for Alaska, or 'Hawaii' for Hawaii.
#' @param household_size Household sizes have different poverty threshold. Specify household size with a
#'     whole number, as a numeric data type, representing the household size.
#'
#' @return A single number representing the poverty threshold for the given year, geographic area, and family size.
#'
#' @examples
#' federal_poverty_guidelines('2021', 'Contiguous US', 4)
#'
#' ## You can calculate multiple household sizes at once
#' federal_poverty_guidelines('2021', 'Hawaii', 1:5)
#'
#' ## SNAP requires an eligibility threshold that is 130% of the federal poverty line
#' federal_poverty_guidelines('2021', 'Contiguous US', 3) * 1.3
#'
#' \dontrun{
#' ## This does not work, however.
#' federal_poverty_guidelines('2021', c('Contiguous US', 'Hawaii'), 3)
#' }
#'
#' @section Source:
#' HHS Poverty Guidelines: \url{https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines}
#'
#' @export
federal_poverty_guidelines <- function(year, geography, household_size) {

  # check parameters

  # make sure the year parameter is an available year
  years <- 2017:2021
  if (!(year %in% years)) stop(paste0("`year` must be either ", paste0(years, collapse = ", "), "."))

  # check geographies
  required_geographies <- c('Contiguous US', 'Hawaii', 'Alaska')
  accepted_geograhies <- tolower(required_geographies)

  if (!(geography %in% c(required_geographies, accepted_geograhies))) {
    stop(paste0("`geography` must be either ", paste0(required_geographies, collapse = ", "), "."))
  }

  # check family size, must be whole number
  if (!is.numeric(household_size)) stop("`household_size` must be numeric.")
  if (!(all(household_size %% 1 == 0))) stop("`household_size` must be a whole number.")

  year <- as.character(year)

  guideline_amounts <- list(

    '2021' = list(
      'contiguous us' = list(
        base = 12880,
        additional_amount = 4540
      ),
      'hawaii' = list(
        base = 14820,
        additional_amount = 5220
      ),
      'alaska' = list(
        base = 16090,
        additional_amount = 5680
      )
    ),

    '2020' = list(
      'contiguous us' = list(
        base = 12760,
        additional_amount = 4480
      ),
      'hawaii' = list(
        base = 14680,
        additional_amount = 5150
      ),
      'alaska' = list(
        base = 15950,
        additional_amount = 5600
      )
    ),

    '2019' = list(
      'contiguous us' = list(
        base = 12490,
        additional_amount = 4420
      ),
      'hawaii' = list(
        base = 14380,
        additional_amount = 5080
      ),
      'alaska' = list(
        base = 15600,
        additional_amount = 5530
      )
    ),

    '2018' = list(
      'contiguous us' = list(
        base = 12140,
        additional_amount = 4320
      ),
      'hawaii' = list(
        base = 13960,
        additional_amount = 4810
      ),
      'alaska' = list(
        base = 15180,
        additional_amount = 5400
      )
    ),

    '2017' = list(
      'contiguous us' = list(
        base = 12060,
        additional_amount = 4180
      ),
      'hawaii' = list(
        base = 13860,
        additional_amount = 4810
      ),
      'alaska' = list(
        base = 15060,
        additional_amount = 5230
      )
    )
  )

  year_geo_amounts <- guideline_amounts[[year]][[geography]]

  return(year_geo_amounts$base + (year_geo_amounts$additional_amount * (household_size - 1)))

}

#' Check state column
#'
#' State should be either a two letter abbreviation or full state name. Check to make sure this is true.
#'
#' @param state_name Two letter abbreviation of state name or full name
#'
#' @keywords internal
check_state <- function(state_name) {

  proper_states <- c(
    datasets::state.abb, "DC", "VI", "GU"
  )

  # make state list and entered data lower case to ensure a state is not recognized simply because of capitalization
  proper_states <- tolower(proper_states)
  state_name_lower <- tolower(state_name)

  # state should either be the two letter abbreviation or full name
  if (!state_name_lower %in% proper_states) {
    stop("One of your state names is unrecognizable. Names should be the two letter abbreviation.", call. = FALSE)
  }

  return(NULL)

}
