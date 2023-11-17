options(repos = c(CRAN = "https://cloud.r-project.org"))

# install packages 
install.packages(c('R.utils',"dplyr","tidyr","data.table", "gtools","ggplot2"))

library(dplyr)
library(tidyr)
library(data.table)
library(gtools)
library(ggplot2)

# Path to the gene_info (Homo_sapiens.gene_info) file
gene_info_file_path <- "Homo_sapiens.gene_info.gz"

# Read the tab-delimited file with gzip compression and get required columns
gene_info_df <- fread(gene_info_file_path, 
                      sep='\t', header=TRUE)[, .(Symbol, chromosome)]

# Show the first 8 rows 
#print(head(gene_info_df,8))

# filter the chromosome column. from ambiguous values like (-) , (10|19|3)
gene_data_filtered <- gene_info_df[!grepl("[|-]", gene_info_df$chromosome), ]

# Count the number of genes in each chromosome 
chromosome_counts <- table(gene_data_filtered$chromosome)

# convert the table of counts to dataframe
chromosome_counts_df <- as.data.frame(chromosome_counts)

# Rename the columns of table to chromosome and gene_count
names(chromosome_counts_df) <- c("Chromosome", "Gene_count")

# Define the custom ordering factor as in the provideed image in the tasks_pdf file
custom_order <- c(paste(1:22), "X", "Y", "MT", "Un")

# Convert the Chromosome column to a factor with custom order
chromosome_counts_df$Chromosome <- factor(chromosome_counts_df$Chromosome, levels = custom_order)

# Sort the data frame based on the custom ordered Chromosome column
chromosome_counts_df <- chromosome_counts_df[order(chromosome_counts_df$Chromosome), ]

plot_im= ggplot(chromosome_counts_df, aes(x = Chromosome, y = Gene_count)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of genes in each chromosome",
       x = "Chromosomes",
       y = "Gene count") +
  theme_classic()+
theme(
  #plot.title = element_text(size = 20, margin = margin(20, 0, 20, 0)),
  plot.title = element_text(size = 16, 
                            margin = margin(20, 0, 20, 0), hjust = 0.5),
  axis.title.x = element_text(size = 16, margin = margin(10, 0, 10, 0)), 
  axis.title.y = element_text(size = 16, margin = margin(0, 10, 0, 10)),
  
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_blank()
)
#plot_im
ggsave("plot_output.pdf", plot = plot_im)
