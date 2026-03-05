#' Match ASVs to MAGs across taxonomic levels
#'
#' @description Links amplicon sequence variants (ASVs) from a feature table to
#'   metagenome-assembled genomes (MAGs) by sequentially attempting matches at
#'   full taxonomy, genus, family, and order levels. Samples with fewer than 100
#'   total counts are removed prior to relative abundance conversion. Returns
#'   matched data, match level summaries, and relative abundance data for
#'   downstream plotting.
#'
#' @param mag_file A data frame of MAG metadata. Must contain columns
#'   \code{MAG_FULL_tax}, \code{MAG}, \code{compl}, \code{basin},
#'   \code{contam}, \code{sPROD}, \code{METHANO}, \code{rhodanase},
#'   \code{phsA}, and \code{dsrB}.
#' @param feat A data frame representing the ASV feature table. The first column
#'   is treated as ASV IDs, intermediate columns are sample counts, and the last
#'   column must be named \code{taxonomy} containing semicolon-delimited
#'   taxonomic strings.
#'
#' @return A named list with four elements:
#'   \describe{
#'     \item{merged_data_OUTPUT}{Data frame of ASVs with their matched MAG and
#'       functional annotations.}
#'     \item{match_level_counts}{Summary data frame of match counts and
#'       percentages per taxonomic level.}
#'     \item{feat_filt_relab_long}{Long-format relative abundance data frame
#'       with taxonomic and MAG annotations joined, for use in
#'       \code{generate_plots()}.}
#'     \item{removed_samples_numb}{Integer count of samples removed due to low
#'       read counts (<100).}
#'   }
#'
#' @examples
#' \dontrun{
#' result <- run_matching_tool(mag_file = shale_mags, feat = feature_table)
#' result$match_level_counts
#' plots <- generate_plots(result$match_level_counts, result$feat_filt_relab_long)
#' }
run_matching_tool <- function(mag_file, feat) {
  
  # Format MAG table
  mags <- mag_file %>%
    mutate(
      MAG_genus = sub("(;s__.*)$", "", MAG_FULL_tax),
      MAG_family = sub("(;g__.*)$", "", MAG_FULL_tax),
      MAG_order = sub("(;f__.*)$", "", MAG_FULL_tax),
      MAG_class = sub("(;o__.*)$", "", MAG_FULL_tax)
    )
  
  # Format feature table
  colnames(feat)[1] <- "ASV"
  taxa <- feat %>%
    select(ASV, "ASV_FULL_tax" = taxonomy) %>%
    separate(ASV_FULL_tax, into = c("d", "p", "c", "o", "f", "g", "s"), sep = ";", remove = FALSE) %>%
    mutate(
      ASV_genus = sub("(;s__.*)$", "", ASV_FULL_tax),
      ASV_family = sub("(;g__.*)$", "", ASV_FULL_tax),
      ASV_order = sub("(;f__.*)$", "", ASV_FULL_tax),
      ASV_class = sub("(;o__.*)$", "", ASV_FULL_tax)
    )
  
  # Count ASVs with insufficient classification
  insuf_ASV_UNASSIGNED <- sum(taxa$d == "Unassigned", na.rm = TRUE)
  insuf_ASV_justDOMAIN <- sum(is.na(taxa$p))
  insuf_ASV_justPHYLUM <- sum(is.na(taxa$c))
  insuf_ASV_justCLASS <- sum(is.na(taxa$o))
  
  # Mark insufficient classification
  merged_data <- taxa %>%
    mutate(match_level = ifelse(is.na(o), "insufficient ASV classification", NA))
  
  # Full taxonomic match
  mags_filtered_FULL <- mags %>%
    group_by(MAG_FULL_tax) %>%
    slice_max(order_by = compl, with_ties = FALSE) %>%
    ungroup()
  na_rows_full <- merged_data %>% filter(is.na(match_level))
  merged_data_full <- left_join(na_rows_full, select(mags_filtered_FULL, MAG_FULL_tax, MAG), by = c("ASV_FULL_tax" = "MAG_FULL_tax")) %>%
    mutate(match_level = ifelse(!is.na(MAG), "full tax", NA))
  merged_data <- bind_rows(merged_data %>% filter(!is.na(match_level)), merged_data_full)
  count_ASV_FULL <- sum(merged_data$match_level == "full tax", na.rm = TRUE)
  
  # Genus level match
  mags_filtered_GENUS <- mags %>%
    group_by(MAG_genus) %>%
    slice_max(order_by = compl, with_ties = FALSE) %>%
    ungroup()
  na_rows_genus <- merged_data %>% filter(is.na(match_level)) %>% select(-MAG)
  merged_data_genus <- left_join(na_rows_genus, select(mags_filtered_GENUS, MAG_genus, MAG), by = c("ASV_genus" = "MAG_genus")) %>%
    mutate(match_level = ifelse(!is.na(MAG), "genus", NA))
  merged_data <- bind_rows(merged_data %>% filter(!is.na(match_level)), merged_data_genus)
  count_ASV_GENUS <- sum(merged_data$match_level == "genus", na.rm = TRUE)
  
  # Family level match
  mags_filtered_FAMILY <- mags %>%
    group_by(MAG_family) %>%
    slice_max(order_by = compl, with_ties = FALSE) %>%
    ungroup()
  na_rows_family <- merged_data %>% filter(is.na(match_level)) %>% select(-MAG)
  merged_data_family <- left_join(na_rows_family, select(mags_filtered_FAMILY, MAG_family, MAG), by = c("ASV_family" = "MAG_family")) %>%
    mutate(match_level = ifelse(!is.na(MAG), "family", NA))
  merged_data <- bind_rows(merged_data %>% filter(!is.na(match_level)), merged_data_family)
  count_ASV_FAMILY <- sum(merged_data$match_level == "family", na.rm = TRUE)
  
  # Order level match
  mags_filtered_ORDER <- mags %>%
    group_by(MAG_order) %>%
    slice_max(order_by = compl, with_ties = FALSE) %>%
    ungroup()
  na_rows_order <- merged_data %>% filter(is.na(match_level)) %>% select(-MAG)
  merged_data_order <- left_join(na_rows_order, select(mags_filtered_ORDER, MAG_order, MAG), by = c("ASV_order" = "MAG_order")) %>%
    mutate(match_level = ifelse(!is.na(MAG), "order", NA))
  merged_data <- bind_rows(merged_data %>% filter(!is.na(match_level)), merged_data_order)
  count_ASV_ORDER <- sum(merged_data$match_level == "order", na.rm = TRUE)
  
  # Filter out samples with low counts
  feat_column_sums <- colSums(feat[2:(ncol(feat) - 1)])
  samples_to_remove <- names(feat_column_sums[feat_column_sums < 100])
  removed_samples <- feat %>%
    select(ASV, all_of(samples_to_remove), taxonomy)
  feat_filt <- feat %>%
    select(-all_of(samples_to_remove))
  
  # Convert feature table to relative abundance
  convert_to_relative_abundance <- function(column) {
    column_sum <- sum(column)
    relative_abundance <- (column / column_sum) * 100
    return(relative_abundance)
  }
  feat_filt_relab <- feat_filt %>%
    mutate(across(2:(ncol(.) - 1), convert_to_relative_abundance))
  
  # Pivot longer and bind with MAG data
  feat_filt_relab_long <- feat_filt_relab %>%
    pivot_longer(cols = 2:(ncol(feat_filt_relab) - 1), names_to = "Sample", values_to = "Relabund") %>%
    separate(taxonomy, into = c("d", "p", "c", "o", "f", "g", "s"), sep = ";", remove = FALSE) %>%
    left_join(merged_data %>% select(ASV, MAG, match_level), by = "ASV") %>%
    left_join(
      mags %>% select(MAG, basin, compl, contam, sPROD, METHANO, rhodanase, phsA, dsrB),
      by = "MAG")
  
  # Final merged data
  merged_data_FINAL <- merged_data %>%
    left_join(mags %>% select(MAG, basin, compl, contam, sPROD, METHANO, rhodanase, phsA, dsrB),
              by = "MAG") %>%
    arrange(match_level)
  
  merged_data_OUTPUT <- merged_data_FINAL %>%
    select(-c("d", "p", "c", "o", "f", "g", "s", "ASV_genus", "ASV_family", "ASV_order", "ASV_class"))
  
  # Summarize match levels
  match_level_counts <- merged_data_FINAL %>%
    group_by(match_level) %>%
    summarize(count = n()) %>%
    mutate(percentage = round(count / sum(count) * 100, 2)) %>%
    mutate(label = paste0(match_level, "\n(n=", count, ", ", percentage, "%)"))

  list(
    merged_data_OUTPUT = merged_data_OUTPUT,
    match_level_counts = match_level_counts,
    feat_filt_relab_long = feat_filt_relab_long,
    removed_samples_numb = ncol(removed_samples) - 2
  )
}