# install packages
install.packages(c('R.utils',"dplyr","tidyr","data.table"))

library(dplyr)
library(tidyr)
library(data.table)

# path to the gene_info file
gene_info_file_path <- "Homo_sapiens.gene_info.gz"

# Read the tab-delimited file with gzip compression and get required columns
gene_info_df <- fread(gene_info_file_path, sep='\t', header=TRUE)[, .(GeneID, Symbol, Synonyms)]

# Separate items in the Synonyms column into new rows

new_data1<-gene_info_df %>%
  separate_rows(Synonyms, sep="\\|")

# Mapping the Symbol to their Gene ID
map_Symbol <- with(gene_info_df, setNames(GeneID, Symbol))

# Mapping the Synonyms to their Gene ID
map_Synonyms <- with(new_data1, setNames(GeneID, Synonyms))

# combined two vectors with key as the gene name and value is the Gene ID
map_Symbol_Synonyms_geneid <- c(map_Symbol, map_Synonyms)


# Path to the gmt file
input_gmt_path <- "h.all.v2023.1.Hs.symbols.gmt"

out_gmt_file <- "output_file.gmt"

out_gmt_file <- file(out_gmt_file, "w")

# Read the input gmt file line by line

inp_file <- file(input_gmt_path, "r")

while (length(line <- readLines(inp_file, n = 1, warn = FALSE)) > 0) {
  pathway_info <- unlist(strsplit(line, "\t"))
  
  pathway_name <- pathway_info[1]
  pathway_desc <- pathway_info[2]
  gene_names <- pathway_info[-(1:2)]  # All subsequent values are gene names
  
  
  entrez_ids <- sapply(gene_names, function(gene) {
    if (gene %in% names(map_Symbol_Synonyms_geneid)) {
      return(map_Symbol_Synonyms_geneid[[gene]])
    } else {
      return(gene)  # If Entrez ID not found, keep the original gene name
    }
  })
  
  # Concatenate pathway information with entrez_ids
  pathway_line <- c(pathway_name, pathway_desc, entrez_ids)
  
  # Write updated pathway information to the output file
  cat(paste(pathway_line, collapse = "\t"), file = out_gmt_file, sep = "\n")
}

close(inp_file)
close(out_gmt_file)
