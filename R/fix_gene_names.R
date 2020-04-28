#' Correct gene names
#'
#' Function that extracts the gene name from the column "cluster" in the ARIBA results.
#'
#' @param df The ARIBA result data
#' @param ending The file suffix of the input files from ARIBA
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@protonmail.com}
#'
#' @export
#' @import dplyr
#'
fix_gene_names <- function(df, ending, db) {
  genes <- unique(df$ref_name)

  # correct the gene names with regex patterns
  # specific for each database
  if (db == "res") {
    new_names <- gsub("^(.*?)\\..*", "\\1", genes)
    new_names <- gsub("_", "", new_names, fixed = T)
    new_names <- gsub("-", "", new_names, fixed = T)
  }
  if (db == "vfdb") {
    new_names <- sub("\\..+", "", genes)
  }
  if (db == "virfinder") {
    new_names <- gsub("^(.+_[0-9]+)_.+", "\\1", genes)
  }

  # match the database names to the new names
  gene_names <- c()
  for (i in new_names) {
    p <- paste(tolower(substring(i, 1, 3)),
              substring(i, 4),
              sep = "",
              collapse = " ")
    gene_names <- c(gene_names, p)
  }
  df2 <- data.frame(genes, gene_names) %>%
    mutate(genes = as.character(genes)) %>%
    rename(ref_name = genes)

  df %>%
    left_join(df2, by = "ref_name") %>%
    mutate(
      gene_names = as.character(gene_names),
      ref = gsub(paste0("(.*?)", ending), "\\1", ref)
    )
}
