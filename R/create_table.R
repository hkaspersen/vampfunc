#' Generate overview table of genes
#'
#' Function that filters raw ARIBA data into a table with presence/absence results
#'
#' @param df The data frame holding the raw ARIBA data
#' @param acquired logical, if the data is from acquired gene analysis then this should be set to TRUE
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@protonmail.com}
#'
#' @export
#' @import dplyr
#' @importFrom tidyr spread
#' @importFrom tidyr gather
#'
create_table <- function(df, acquired = TRUE) {
  acq_flags <- c(
    "27",
    "155",
    "411",
    "923",
    "795",
    "539",
    "667",
    "795"
  )

  int_flags <- c(
    "19",
    "27",
    "147",
    "155",
    "403",
    "411",
    "915",
    "923",
    "787",
    "795",
    "531",
    "539",
    "659",
    "667",
    "787",
    "795"
  )

  if (acquired == TRUE) {
    df %>%
      select(ref, gene_names, flag, ref_ctg_change) %>%
      filter(flag %in% acq_flags) %>%
      mutate(id = 1:n()) %>%
      spread(gene_names, ref_ctg_change) %>%
      group_by(ref) %>%
      summarise_all(list(func_paste)) %>%
      select(-c(id, flag)) %>%
      gather(gene, result, -ref) %>%
      mutate(
        result_total = ifelse(result == "", 0, 1),
        result_total = as.character(result_total)
      ) %>%
      select(-result) %>%
      rename("result" = result_total)
  } else {
    df %>%
      select(ref, gene_names, flag, ref_ctg_change) %>%
      filter(flag %in% int_flags) %>%
      mutate(id = 1:n()) %>%
      spread(gene_names, ref_ctg_change) %>%
      group_by(ref) %>%
      summarise_all(list(func_paste)) %>%
      select(-c(id, flag)) %>%
      gather(gene, mut, -ref) %>%
      mutate(
        mut = ifelse(mut == "" | mut == "." | is.na(mut) == TRUE, 0, mut),
        result = ifelse(mut != 0, 1, 0),
        result = as.integer(result)
      )
  }
}
