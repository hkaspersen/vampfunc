#' Filter out non-QRDR mutations
#'
#' Function that filters out all non-QRDR mutations detected in the dataset, only for gyrA, gyrB, parC, and parE
#'
#' @param df The data frame holding the processed ARIBA results in long format
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@protonmail.com}
#'
#' @export
#' @import dplyr
#' @import reprex
#' @importFrom purrr map
#' @importFrom purrr map_lgl
#'
fix_gyr_par_results <- function(df) {
  df %>%
    mutate(gyrA_result = mut %>% # control for mutation within QRDR for gyrA
             str_extract_all("\\d+") %>% # from reprex package
             map(as.integer) %>% # converts all to integer
             map_lgl(~ any(.x >= 67L & .x <= 106L)), # returns TRUE/FALSE whether value is within range or not
           gyrA_result = if_else(gene != "gyrA", NA, gyrA_result), # filters out results for all other genes
           gyrA_result = as.integer(gyrA_result),
           gyrB_result = mut %>% # control for mutation within QRDR for gyrB
             str_extract_all("\\d+") %>%
             map(as.integer) %>%
             map_lgl(~ any(.x >= 333L & .x <= 481L)),
           gyrB_result = if_else(gene != "gyrB", NA, gyrB_result),
           gyrB_result = as.integer(gyrB_result),
           parC_result = mut %>% # control for mutation within QRDR for parC
             str_extract_all("\\d+") %>%
             map(as.integer) %>%
             map_lgl(~ any(.x >= 51L & .x <= 170L)),
           parC_result = if_else(gene != "parC", NA, parC_result),
           parC_result = as.integer(parC_result),
           parE_result = mut %>% # control for mutation within QRDR for parE
             str_extract_all("\\d+") %>%
             map(as.integer) %>%
             map_lgl(~ any(.x >= 366L & .x <= 523L)),
           parE_result = if_else(gene != "parE", NA, parE_result),
           parE_result = as.integer(parE_result)) %>%
    mutate(result_gyr_par = case_when(gene == "gyrA" ~ gyrA_result,
                                      gene == "gyrB" ~ gyrB_result,
                                      gene == "parC" ~ parC_result,
                                      gene == "parE" ~ parE_result)) %>%
    mutate(result_total = ifelse(gene %in% c("gyrA","gyrB","parC","parE"),
                                 result_gyr_par, result)) %>%
    select(-c(gyrA_result,
              gyrB_result,
              parC_result,
              parE_result,
              result_gyr_par,
              result)) %>%
    rename("result" = result_total)
}
