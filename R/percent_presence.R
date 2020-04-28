#' Calculate the percent presence of a value
#'
#' Function that calculates the percent presence of all unique values in a column in relation to the total number of rows in the data frame (samples)
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@protonmail.com}
#'
#' @export
#' @importFrom dplyr group_by
#' @importFrom dplyr count
#' @importFrom dplyr mutate
#' @importFrom dplyr sym
#'
percent_presence <- function(df, group_col) {
  # Get the total number of samples
  n_samples <- nrow(df)

  # calculate the percent presence
  # of each value
  df %>%
    group_by(!! sym(group_col)) %>%
    count() %>%
    mutate(total = n_samples,
           percent = round(n/total*100, 1))
}
