#' Correct gene names
#'
#' Function that extracts the gene name from the column "cluster" in the ARIBA results.
#'
#' @param df The ARIBA result data
#' @param plasmid TRUE if the data is from plasmidfinder
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@protonmail.com}
#'
#' @export
#'
fix_gene_names <- function(df, plasmid = FALSE) {
  genes <- unique(df$ref_name)
  new_names <- gsub("^(.*?)\\..*", "\\1", genes)
  new_names <- gsub("_", "", new_names, fixed = T)
  new_names <- gsub("-", "", new_names, fixed = T)

  gene_names <- c()
  for (i in new_names) {
    p <-
      paste(tolower(substring(i, 1, 3)),
            substring(i, 4),
            sep = "",
            collapse = " ")
    gene_names <- c(gene_names, p)
  }
  df2 <- data.frame(genes, gene_names) %>%
    mutate(genes = as.character(genes)) %>%
    rename(ref_name = genes)

  if (plasmid == FALSE) {
    df %>%
      left_join(df2, by = "ref_name") %>%
      mutate(
        gene_names = as.character(gene_names),
        ref = gsub("(.*?)_amr_report.tsv", "\\1", ref)
      )
  } else {
    df %>%
      left_join(df2, by = "ref_name") %>%
      mutate(gene_names = as.character(gene_names),
             ref = gsub("(.*?)/report.tsv", "\\1", ref))
  }
}
