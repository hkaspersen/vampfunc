#' Filter table based on a list of genes
#'
#' Function that filters out all genes except the ones specified in list_genes supplied to the function
#'
#' @param df The data frame
#' @param list_genes A vector of genes of interest
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@protonmail.com}
#'
#' @export
#' @import dplyr
#'
filter_table <- function(df, list_genes) {
  report_genes <- unique(df$gene)
  grep_genes <- c()
  for (gene in list_genes) {
    for (g in report_genes) {
      if (grepl(gene, g, ignore.case = T) == TRUE) {
        grep_genes <- c(grep_genes, g)
      }
    }
  }
  df %>%
    filter(gene %in% grep_genes) %>%
    mutate(gene = gsub("_", "", gene),
           gene = as.character(gene))
}
