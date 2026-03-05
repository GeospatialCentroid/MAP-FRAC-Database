#' Generate diagnostic and summary plots for ASV-MAG matching results
#'
#' @description Produces a set of six plots summarizing the results of the
#'   ASV-MAG matching tool, including taxonomic match distributions, community
#'   composition, and functional potential (sulfide production, methanogenesis).
#'
#' @param match_level_counts A data frame containing ASV match level counts and
#'   percentages, as returned by \code{run_matching_tool()}.
#' @param feat_filt_relab_long A long-format data frame of filtered relative
#'   abundance values with taxonomic and functional annotations, as returned by
#'   \code{run_matching_tool()}.
#' @param interactive Logical. If \code{TRUE}, returns interactive
#'   \code{plotly} plots; if \code{FALSE} (default), returns static
#'   \code{ggplot2} plots.
#'
#' @return A named list of six plots:
#'   \describe{
#'     \item{p1}{Pie chart of ASV match level proportions.}
#'     \item{p2}{Bubble chart of ASV taxonomic classifications vs. phylum, colored by match level.}
#'     \item{p3}{Stacked bar chart of ASV community composition by phylum per sample.}
#'     \item{p4}{Stacked bar chart of proportion of community linked to MAGs per sample.}
#'     \item{p5}{Stacked bar chart of inferred sulfide producers by pathway per sample.}
#'     \item{p6}{Stacked bar chart of inferred methanogens per sample.}
#'   }
#'
#' @examples
#' \dontrun{
#' result <- run_matching_tool(mag_file, feat)
#' plots <- generate_plots(result$match_level_counts, result$feat_filt_relab_long)
#' plots <- generate_plots(result$match_level_counts, result$feat_filt_relab_long, interactive = TRUE)
#' }
generate_plots <- function(match_level_counts,
                           feat_filt_relab_long,
                           interactive = FALSE) {
  # Need to establish an order to the data so that it presents in a logical order downstream
  match_order <- c("full tax",
                   "genus",
                   "family",
                   "order",
                   "insufficient ASV classification",
                   "NA")
  
  # PIE CHART ######################################################
  # this plot has to be created in plotly as ggplotly doesn't support polar coordinates
  if (interactive) {
    p1 <- plot_ly(
      data = mutate(
        match_level_counts,
        match_level = factor(match_level, levels = match_order)
      ) %>% arrange(match_level),
      labels =  ~ match_level,
      values =  ~ count,
      type = "pie",
      sort = FALSE,
      marker = list(
        colors =  c(
          "#cc4778",
          "#f89540",
          "#f0f921",
          "#6a00a8",
          "#0d0887",
          "#808080"
        )
      )
    ) %>%
      layout(
        #autosize = F,
        title = list(text = 'Total proportion of ASVs that linked to a MAG'),
        xaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE
        ),
        yaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE
        ),
        legend = list(
          title = list(text = "Taxonomic level<br>where the ASV <br>linked to a MAG"),
          traceorder = 'normal'
        ),
        margin = list(
          l = 0,
          r = 0,
          b = 0,
          t = 50
        )
      )
    
  } else {
    p1 <- match_level_counts %>%
      arrange(desc(match_level)) %>%
      mutate(
        #match_level = factor(match_level, levels = match_order),
        # variable for pie chart
        text_y = cumsum(count) - count / 2
      ) %>%
      ggplot(aes(x = "", y = count, fill = match_level)) +
      geom_bar(stat = "identity", width = 1) +
      scale_fill_manual(values = c(
        "#F2671F",
        "#C91B26",
        "#9C0F5F",
        "#60047A",
        "#160A47",
        "#808080"
      )) +
      coord_polar(theta = "y") +
      theme_void() +
      labs(title = "Total proportion of ASVs that linked to a MAG", fill = "Match Level") +
      theme(legend.position = "none") +
      geom_label_repel(
        aes(label = label, y = text_y),
        size = 4,
        nudge_x = 1,
        nudge_y = 0.6,
        show.legend = FALSE,
        color = "white"
      )
    
  }
  
  
  # BUBBLE CHART ###################################################
  find_lowest_tax <- c("d", "p", "c", "o", "f", "g", "s")
  
  first_na_column <- function(row) {
    first_na <- which(is.na(row))
    if (length(first_na) == 0) {
      return("s")
    } else {
      return(names(row)[first_na[1]])
    }
  }
  
  feat_filt_relab_long$first_na <- apply(feat_filt_relab_long[find_lowest_tax], 1, first_na_column)
  
  feat_filt_relab_long_p2 <- feat_filt_relab_long %>%
    filter(Relabund > 0.05, !is.na(p)) %>%
    mutate(first_na = factor(first_na, levels = c("c", "o", "f", "g", "s"))) %>%
    mutate(match_level = factor(match_level, levels = match_order)) %>%
    mutate(p = str_remove(p, "p__")) %>%
    filter(Relabund > 1) %>%
    mutate(
      # jittered_x = jitter(as.numeric(as.factor(first_na)), amount = 0.25),
      # jittered_y = jitter(as.numeric(as.factor(p)), amount = 0.25),
      first_na = case_when(
        first_na == "d" ~ "Domain",
        first_na == "p" ~ "Phylum",
        first_na == "c" ~ "Class",
        first_na == "o" ~ "Order",
        first_na == "f" ~ "Family",
        first_na == "g" ~ "Genus",
        first_na == "s" ~ "Species"
      ),
      # factor taxa
      first_na = factor(
        first_na,
        levels = c(
          "Species",
          "Genus",
          "Family",
          "Order",
          "Class",
          "Phylum",
          "Domain"
        )
      ),
      jittered_x = jitter(as.numeric(as.factor(first_na)), amount = 0.25),
      jittered_y = jitter(as.numeric(as.factor(p)), amount = 0.25),
      
    )
  
  if (interactive) {
    p2 <- feat_filt_relab_long_p2 %>%
      plot_ly(
        x = ~ jittered_x,
        y = ~ jittered_y,
        # x = ~first_na,
        # y = ~p,
        type = 'scatter',
        mode = 'markers',
        color = ~ match_level,
        colors = c(
          "#9C0F5F",
          "#C91B26",
          "#F2671F",
          "#f0f921",
          "#990ac2",
          "#808080"
        ),
        marker = list(
          size = ~ Relabund,
          opacity = 0.8,
          symbol = 'circle'
        ),
        text = ~ paste(
          "First NA:",
          first_na,
          "<br>Phylum:",
          p,
          "<br>Relabund:",
          round(Relabund, 2),
          "<br>Match Level:",
          match_level
        ),
        # Custom hover text
        hoverinfo = 'text'  # Show only custom hover text
      ) %>% layout(
        title = "Taxonomic classifications of ASVs that linked to MAGs",
        xaxis = list(
          title = "Lowest level of ASV taxonomic classification",
          tickvals = 1:length(levels(feat_filt_relab_long_p2$first_na)),
          #tickvals = 1:length(unique(feat_filt_relab_long_p2$first_na)),
          ticktext = levels(feat_filt_relab_long_p2$first_na)
        ),
        yaxis = list(
          title = "Phylum",
          tickvals = 1:length(unique(feat_filt_relab_long_p2$p)),
          ticktext = levels(as.factor(feat_filt_relab_long_p2$p))
        ),
        legend = list(
          title = list(text = "Match level<br>Sized by<br>Relative Abundance"),
          itemsizing = 'constant'
        ),
        showlegend = TRUE
      )
    
    
  } else {
    p2 <- ggplot(
      feat_filt_relab_long_p2 %>% mutate(p = str_remove(p, "p__")),
      aes(x = first_na, y = p, color = match_level)
    ) +
      geom_jitter(
        aes(size = Relabund),
        shape = 19,
        alpha = 0.8,
        height = 0.25
      ) +
      theme_bw() +
      scale_color_manual(values = c(
        "#F2671F",
        "#C91B26",
        "#9C0F5F",
        "#60047A",
        "#160A47",
        "#808080"
      )) +
      labs(title = "Taxonomic classifications of ASVs that linked to MAGs",
           color = "Match level",
           size = "% Relative Abundance") +
      xlab("Lowest level of ASV taxonomic classification") +
      ylab("Phylum") +
      guides(fill = guide_legend(override.aes = list(size = 5)))
  }
  
  # STACKED BAR CHARTS #############################################
  plasma_palette <- viridis_pal(option = "plasma", direction = -1)(length(unique(feat_filt_relab_long$p)))
  
  p3 <- feat_filt_relab_long %>%
    # sum relabund for chart
    group_by(p, Sample) %>%
    summarize(Relabund = sum(Relabund, na.rm = TRUE)) %>%
    mutate(p = str_remove(p, "p__")) %>%
    ggplot(aes(x = Sample, y = Relabund, fill = p)) +
    geom_bar(stat = "identity") +
    theme_bw() +
    labs(title = "ASV Community Composition", fill = "Phylum") +
    scale_fill_manual(values = plasma_palette) +
    guides(fill = guide_legend(ncol = 2)) +
    xlab("Sample") +
    ylab("% Relative Abundance") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  p4 <- feat_filt_relab_long %>%
    mutate(match_level = factor(match_level, levels = rev(match_order))) %>%
    group_by(Sample, match_level) %>%
    summarize(Relabund = sum(Relabund, na.rm = TRUE)) %>%
    ggplot(aes(x = Sample, y = Relabund, fill = match_level)) +
    geom_bar(stat = "identity") +
    theme_bw() +
    labs(title = "Proportion of community with linkages to MAGs", fill = "Taxonomic level \nof ASV-MAG match") +
    scale_fill_manual(values = c(
      "#160A47",
      "#60047A",
      "#9C0F5F",
      "#C91B26",
      "#F2671F",
      "#808080"
    )) +
    xlab("Sample") +
    ylab("% Relative Abundance") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # FUNCTIONAL STACKED BAR CHARTS ##################################
  
  ## updated data cleaning for new data structure
  feat_filt_relab_long3 <- feat_filt_relab_long %>%
    rowwise() %>%
    mutate(S_pathway = case_when(
      sPROD == FALSE ~ "FALSE",
      is.na(sPROD) ~ NA_character_,
      sPROD == TRUE ~ {
        genes <- c()
        if (isTRUE(rhodanase))
          genes <- c(genes, "rhodanase")
        if (isTRUE(dsrB))
          genes <- c(genes, "dsrB")
        if (isTRUE(phsA))
          genes <- c(genes, "phsA")
        if (length(genes) == 0)
          "TRUE (no genes)"
        else
          paste(genes, collapse = ", ")
      }
    )) %>%
    ungroup() %>%
    # Arrange positions of categories
    mutate(S_pathway = factor(S_pathway, levels = c(NA, "FALSE", sort(
      unique(S_pathway[!is.na(S_pathway) & S_pathway != "FALSE"])
    ))))
  
  p5 <- feat_filt_relab_long3 %>%
    group_by(Sample, S_pathway) %>%
    summarise(Relabund = sum(Relabund, na.rm = TRUE)) %>%
    ggplot(aes(x = Sample, y = Relabund, fill = S_pathway)) +
    geom_bar(stat = "identity") +
    theme_bw() +
    labs(title = "Proportion of inferred sulfide producers", fill = "Sulfide production\npathway") +
    xlab("Sample") +
    ylab("% Relative Abundance") +
    scale_fill_manual(
      values = c(
        "#d3d3d3",
        "#008080",
        "#b4c7a8",
        "#70a494",
        "#ca5828",
        "#f6ebbc",
        "#dd8a58",
        "grey30"
      )
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  p6 <- feat_filt_relab_long3 %>%
    group_by(Sample, METHANO) %>%
    summarise(Relabund = sum(Relabund, na.rm = TRUE)) %>%
    ggplot(aes(x = Sample, y = Relabund, fill = METHANO)) +
    geom_bar(stat = "identity") +
    theme_bw() +
    labs(title = "Proportion of inferred methanogens", fill = "Potential\nmethanogen") +
    xlab("Sample") +
    ylab("% Relative Abundance") +
    scale_fill_manual(values = c("#d3d3d3", "#097969", "#434343")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  
  list(
    p1 = p1,
    p2 = p2,
    p3 = p3,
    p4 = p4,
    p5 = p5,
    p6 = p6
  )
}