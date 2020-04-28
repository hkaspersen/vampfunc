#' ARIBA Flag evaluation function
#'
#' Function that evaluates the flags reported by ARIBA as "1", accepted, or "0", not accepted.
#'
#' @param df The data frame holding the flag values
#' @param acquired if the data is from acquired resistance screening, set this to TRUE, as different flags are accepted for the acquired genes compared to mutations in intrinsic genes.
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@protonmail.com}
#'
#' @export
#' @import dplyr
#'
check_flags <- function(df, acquired = TRUE) {
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
    df <- df %>%
      select(ref, gene_names, cluster, flag, ref_ctg_change) %>%
      mutate(flag_result = as.integer(flag %in% acq_flags)) %>%
      rename("gene" = gene_names)
  } else {
    df <- df %>%
      select(ref, gene_names, cluster, flag, ref_ctg_change) %>%
      mutate(flag_result = as.integer(flag %in% int_flags)) %>%
      rename("gene" = gene_names)
  }
}
