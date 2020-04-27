#' Calculate percent presence of genes
#'
#' Function that calculates the percent presence of all genes in the input report
#'
#' @param df The data frame holding the processed ARIBA results in long format
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@protonmail.com}
#'
#' @export
#' @import dplyr
#'
calc_stats <- function(df) {
  df %>%
    group_by(gene, result) %>%
    count() %>%
    ungroup() %>%
    mutate(result = if_else(result == 1,
                            "Present",
                            "Absent")) %>%
    spread(result, n, fill = 0) %>%
    rowwise() %>%
    mutate(
      Absent = ifelse("Absent" %in% names(.),
                      Absent,
                      0)) %>%
    mutate(
      Total = Present + Absent,
      Percent = round(Present / Total * 100, 1)
    ) %>%
    ungroup()
}
