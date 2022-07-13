taxa2vector <- function(taxa_table) {

  ### taxa_table is a data frame with taxonomic levels as columns
  # Add the previous levels to each one of them
  taxa_table$k <- paste("k_", taxa_table$k, sep = "")
  taxa_table$p <- paste(taxa_table$k, taxa_table$p, sep = "__p_")
  taxa_table$c <- paste(taxa_table$p, taxa_table$c, sep = "__c_")
  taxa_table$o <- paste(taxa_table$c, taxa_table$o, sep = "__o_")
  taxa_table$f <- paste(taxa_table$o, taxa_table$f, sep = "__f_")
  taxa_table$g <- paste(taxa_table$f, taxa_table$g, sep = "__g_")
  taxa_table$s <- paste(taxa_table$g, taxa_table$s, sep = "__s_")

  ### Vectorize the taxa_table table
  tax_vector <- unique(unlist(setDT(taxa_table)[, .(k, p, c, o, f, g, s)]))

  ### Text formatting:
  # no punctuation or weird symbole, only '_'

  tax_vector <- str_replace_all(tax_vector, pattern = "[:punct:]", replacement = "_")
  tax_vector <- str_replace_all(tax_vector, pattern = " ", replacement = "_")
  tax_vector <- str_replace_all(tax_vector, pattern = "__", replacement = "_")

  # levels are separated by 2x'_' and the initial of the level
  tax_vector <- str_replace_all(tax_vector, pattern = "\\_(?=(p|c|o|f|g|s){1}_)", replacement = "__")

  return(tax_vector)
}


#' Transform character strings to be compatible with endoR functions.
#' @export
compatibleNames <- function(x) {
  x %>%
    str_replace_all(pattern = "[:punct:]", replacement = "_") %>%
    str_replace_all(pattern = " ", replacement = "_") %>%
    str_replace_all(pattern = "\\_+", replacement = "_") %>%
    str_replace_all(pattern = "\\_$", replacement = "")
}
