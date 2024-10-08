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
  # match_level_counts_factored <- match_level_counts %>%
  #   mutate(
  #     match_level = factor(match_level, levels = match_order),
  #     # variable for pie chart
  #     text_y = cumsum(count) - count / 2
  #   )
  # 
  # this plot has to be created in plotly as ggplotly doesn't support polar coordinates
  if (interactive) {
    p1 <- plot_ly(
      data = mutate(match_level_counts, match_level = factor(match_level, levels = match_order)) %>% arrange(match_level),
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
        legend = list(title = list(text = "Taxonomic level<br>where the ASV <br>linked to a MAG"), traceorder = 'normal'),
        margin = list(l = 0, r = 0, b = 0, t = 50)
      )
    
  } else {
    p1 <- match_level_counts %>%
      arrange(desc(match_level)) %>%
      mutate(#match_level = factor(match_level, levels = match_order),
        # variable for pie chart
        text_y = cumsum(count) - count / 2) %>%
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
      first_na = case_when(first_na == "d" ~ "Domain",
                                  first_na == "p" ~ "Phylum",
                                  first_na == "c" ~ "Class",
                                  first_na == "o" ~ "Order",
                                  first_na == "f" ~ "Family",
                                  first_na == "g" ~ "Genus",
                                  first_na == "s" ~ "Species"),
      # factor taxa
      first_na = factor(first_na, levels = c("Species", "Genus", "Family", "Order", "Class", "Phylum", "Domain")),
      jittered_x = jitter(as.numeric(as.factor(first_na)), amount = 0.25),
      jittered_y = jitter(as.numeric(as.factor(p)), amount = 0.25),
      
    )
  
  if(interactive) {

    p2 <- feat_filt_relab_long_p2 %>%
      plot_ly(
        x = ~jittered_x,
        y = ~jittered_y,
        # x = ~first_na,
        # y = ~p,
        type = 'scatter',
        mode = 'markers',
        color = ~match_level,
        colors = c("#9C0F5F", "#C91B26", "#F2671F","#f0f921", "#990ac2", "#808080"),
        marker = list(
          size = ~Relabund,
          opacity = 0.8,
          symbol = 'circle'
        ),
        text = ~paste("First NA:", first_na, "<br>Phylum:", p, "<br>Relabund:", round(Relabund, 2), "<br>Match Level:", match_level),  # Custom hover text
        hoverinfo = 'text'  # Show only custom hover text
      ) %>% layout(
        title = "Taxonomic classifications of ASVs that linked to MAGs",
        xaxis = list(
          title = "Lowest level of ASV taxonomic classification",
          tickvals = 1:length(levels(feat_filt_relab_long_p2$first_na)),
         #tickvals = 1:length(unique(feat_filt_relab_long_p2$first_na)),
          ticktext = levels(feat_filt_relab_long_p2$first_na)),
        yaxis = list(
          title = "Phylum",
          tickvals = 1:length(unique(feat_filt_relab_long_p2$p)),
          ticktext = levels(as.factor(feat_filt_relab_long_p2$p))
        ),
        legend = list(title = list(text = "Match level<br>Sized by<br>Relative Abundance"),
                      itemsizing='constant'),
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
    scale_fill_manual(
      values = c(
        "#160A47",
        "#60047A",
        "#9C0F5F",
        "#C91B26",
        "#F2671F",
        "#808080"
      )
    ) +
    xlab("Sample") +
    ylab("% Relative Abundance") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # FUNCTIONAL STACKED BAR CHARTS ##################################
  p5 <- feat_filt_relab_long %>%
    group_by(Sample, sPROD) %>%
    summarise(Relabund = sum(Relabund, na.rm = TRUE)) %>%
    ggplot(aes(x = Sample, y = Relabund, fill = sPROD)) +
    geom_bar(stat = "identity") +
    theme_bw() +
    labs(title = "Proportion of inferred sulfide producers", fill = "Potential for\nsulfide production") +
    xlab("Sample") +
    ylab("% Relative Abundance") +
    scale_fill_manual(values = c("#d3d3d3", "#0F52BA", "#434343")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  p6 <- feat_filt_relab_long %>%
    group_by(Sample, acetatePROD) %>%
    summarise(Relabund = sum(Relabund, na.rm = TRUE)) %>%
    ggplot(aes(x = Sample, y = Relabund, fill = acetatePROD)) +
    geom_bar(stat = "identity") +
    theme_bw() +
    labs(title = "Proportion of inferred acetate producers", fill = "Potential for\nacetate production") +
    xlab("Sample") +
    ylab("% Relative Abundance") +
    scale_fill_manual(values = c("#d3d3d3", "#DE3163", "#434343")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  p7 <- feat_filt_relab_long %>%
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
  
  # Save plots if needed
  # ggsave("p1_PieChart.pdf", plot = p1)
  # ggsave("p2_ASVs_that_match_1perc.pdf", plot = p2)
  # ggsave("p3_ASV_comcomp_Phylum.pdf", plot = p3)
  # ggsave("p5_ASV_comcomp_Matches.pdf", plot = p4)
  # ggsave("p6_ASV_function_sulfide.pdf", plot = p5)
  # ggsave("p7_ASV_function_acetate.pdf", plot = p6)
  # ggsave("p8_ASV_function_methanogens.pdf", plot = p7)
  
  list(
    p1 = p1,
    p2 = p2,
    p3 = p3,
    p4 = p4,
    p5 = p5,
    p6 = p6,
    p7 = p7
  )
}


# Test
#plots <- generate_plots(match_level_counts, feat_filt_relab_long, interactive = TRUE)
# plots