#' Create a presence/absence report
#'
#' Function that creates a presence, 1, absence, 0, report, one row per sample, one column per gene
#'
#' @param df The data frame holding the processed ARIBA results in long format
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@protonmail.com}
#'
#' @export
#' @import dplyr
#' @importFrom tidyr spread
#'
create_report <- function(df, mut = FALSE) {
  if (mut == FALSE) {
    if ("mut" %in% names(df)) {
      df <- select(df, -mut)
    }
    df %>%
      group_by(ref) %>%
      mutate(id = 1:n()) %>%
      spread(gene, result) %>%
      summarise_all(list(func_paste)) %>%
      select(-id)
  } else {
    df %>%
      group_by(ref) %>%
      mutate(id = 1:n()) %>%
      spread(gene, mut) %>%
      summarise_all(list(func_paste)) %>%
      select(-c(id, result))
  }
}
