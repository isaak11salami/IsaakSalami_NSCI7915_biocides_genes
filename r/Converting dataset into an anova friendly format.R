#Qwen3-235B-A22B used to generate code to convert the dataset into an
#anova friendly format

# Load required libraries
library(dplyr)
library(tidyr)

# Step 1: Read the CSV without assuming headers
raw_data <- read.csv("transporters.csv", header = FALSE)

# Step 2: Set column names using row 3 (assumed to be actual headers)
col_names <- as.character(raw_data[3, ])  # Row 3 contains column names
test_df <- raw_data %>%
  slice(-c(1:3)) %>%            # Remove first 3 rows (metadata)
  setNames(col_names)           # Assign correct column names

# Step 3: Identify all logFC columns
logFC_columns <- names(test_df)[grepl("^logFC_", names(test_df))]

# Check if "gene_name" exists
if (!"gene_name" %in% names(test_df)) {
  stop("Column 'gene_name' not found. Available columns: ", paste(names(test_df), collapse = ", "))
}

# Select only gene_name and logFC columns
filtered_df <- test_df[c("gene_name", logFC_columns)]

# Step 4: Pivot longer to get one row per gene-biocide pair
long_df <- filtered_df %>%
  pivot_longer(
    cols = starts_with("logFC_"),
    names_to = "biocide",
    values_to = "logFC"
  )

# Step 5: Clean up biocide names by removing "logFC_" prefix
long_df$biocide <- gsub("logFC_", "", long_df$biocide)

# Step 6: Reorder columns
final_df <- long_df[, c("gene_name", "biocide", "logFC")]

# Optional: View and write output
print(head(final_df, n = 20))
write.csv(final_df, "all_genes_long_format.csv", row.names = FALSE)



