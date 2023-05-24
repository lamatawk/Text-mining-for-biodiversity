# cleaning around
# ==============================================================================
rm(list=ls()) # removing variables
graphics.off() # clearing plotting devices
cat("\014") # clearing console
# ==============================================================================



# load the extraction functions
# ==============================================================================
source("Aux_pdfsExtractor.R")
# ==============================================================================

zipfLaw <- function() {
  
  # Get input files
  pdfsFolderPath <- file.path(getwd(), "Inputs/PDFs")
  pdfsPaths <- list.files(pdfsFolderPath, pattern="*.pdf", full.names=TRUE)
  
  # Delete old folders
  unlink('2. Zipf law/1. Plots', recursive = TRUE, force = TRUE)
  unlink('2. Zipf law/2. Plots vs LReg', recursive = TRUE, force = TRUE)
  
  # Create new folders
  dir.create('2. Zipf law/1. Plots')
  dir.create('2. Zipf law/2. Plots vs LReg')
  
  # Initialize vectors and lists
  outFullList <- list()
  pdfNames <- c()
  
  # Use the function called fullCleanWords_stopWKept from Aux_pdfsExtractor
  # For the analysis, the data is extracted from the PDFs removing symbols and 
  # numbers but keeping stop words (required for this procedure) 
  for (i in pdfsPaths) {
    preparedData <- fullCleanWords_stopWKept(i)
    outFullList <- append(outFullList, list(preparedData))
    pdfNames <- c(pdfNames, tools::file_path_sans_ext(basename(i)))
  }
  
  # Turn extracted data into tibble
  outFullVect <- unlist(outFullList)
  tibble_pdf_text <- tibble(pdfName = pdfNames,  text = outFullVect)
  
  # compute term frequency per pdf
  tibble_pdf_word_freq <- tibble_pdf_text %>%
    unnest_tokens(word, text) %>%
    count(pdfName, word, sort = TRUE)
  
  # compute total words per pdf
  tibble_pdf_total <- tibble_pdf_word_freq %>% 
    group_by(pdfName) %>% 
    summarize(total = sum(n))
  
  # join term freq and total words per pdf
  tibble_pdf_word_freq_total <- left_join(tibble_pdf_word_freq, tibble_pdf_total)
  
  
  # Zipf’s law -> freq that a word appears is inversely proportional to its rank.
  
  # add rank and term freq(%) columns to the tibble
  tibble_pdf_word_freq_total_rank_tf <- tibble_pdf_word_freq_total %>% 
    group_by(pdfName) %>% 
    mutate(rank = row_number(), 
           `term frequency` = n/total) %>%
    ungroup()
  
  # check if Sum_tf gives 1 in each pdf
  # test_SumTf <- tibble_pdf_word_freq_total_rank_tf %>% 
  #  group_by(pdfName) %>% 
  #  summarize(`Sum_tf` = sum(`term frequency`))
  
  
  # Plot rank (x) and term freq (y), on log scales 
  # -> expected: (-) const slope (inv prop on log)
  #-----------------------------------------------------------------------------
  # All-in-one plot - Zipf's  law
  #-----------------------------------------------------------------------------
  png(file = paste("2. Zipf law/1. Plots/All-in-one.png"), width = 4.5, 
      height = 3, units = "in", res = 1100, pointsize = 4)
  groupPlot <- tibble_pdf_word_freq_total_rank_tf %>% 
    ggplot(aes(rank, `term frequency`, color = pdfName)) + 
    geom_line(alpha = 1, show.legend = TRUE) + 
    scale_x_log10() +
    scale_y_log10() +
    labs(
      title = "All PDFs",
      subtitle = "Zipf's law") +
    theme(
      plot.title = element_text(color = "#0099f9", size = 15, face ="bold", 
                                hjust = 0.5),
      plot.subtitle = element_text(size = 10, face ="bold", hjust = 0.5),
      axis.text.x=element_text(size=8),
      axis.text.y=element_text(size=8),
      axis.title=element_text(size=9,face="bold"),
      legend.text=element_text(size=6)
    ) + scale_color_viridis(discrete = TRUE, option = "D")+
    scale_fill_viridis(discrete = TRUE) 
  # Save the file
  print(groupPlot)
  dev.off()
  
  # Print the graph in the plot window
  print(groupPlot)
  
  # Compare (All-in-one plot) -> linear regression
  linearReg <- lm(log10(`term frequency`) ~ log10(rank), 
                  data = tibble_pdf_word_freq_total_rank_tf)
  intercp <- coefficients(linearReg)[1]
  slop <- coefficients(linearReg)[2]
  
  # Add comparison line to the All-in-one plot
  groupPlotR <- groupPlot + 
    geom_abline(intercept = intercp, slope = slop, color = "gray50", 
                linetype = 2) 
  
  # save image
  png(file = paste("2. Zipf law/2. Plots vs LReg/All-in-one+R.png"), width = 4.5, 
      height = 3, units = "in", res = 1100, pointsize = 4)
  print(groupPlotR)
  dev.off()
  
  # Print the graph in the plot window
  print(groupPlotR)
  
  #----------------------------------------------------------------------------------------------
  # Individual plots (one per pdf) - Zipf's  law
  #----------------------------------------------------------------------------------------------
  
  # split tibble per pdf 
  tibble_split <- split(tibble_pdf_word_freq_total_rank_tf, 
                        tibble_pdf_word_freq_total_rank_tf$pdfName)
  namesPdf <- names(tibble_split)
  
  # Generate and save a plot per pdf
  indivPlots <- lapply(namesPdf, function(i) {
    ggplot(tibble_split[[i]], aes(rank, `term frequency`)) +
      geom_line(color = "#0099f9") + 
      geom_point(size = 0.5) +
      scale_x_log10() +
      scale_y_log10() +
      ggtitle(names(i)) +
      labs(
        title = i,
        subtitle = "Zipf's law") +
      theme(
        plot.title = element_text(color = "#0099f9", size = 15, face ="bold", 
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 10, face ="bold", hjust = 0.5),
        axis.text.x=element_text(size=8),
        axis.text.y=element_text(size=8),
        axis.title=element_text(size=9,face="bold")
      ) 
    # Save the file
    ggsave(paste0("2. Zipf law/1. Plots/", i, ".png"), device = "png", width=3,
           height=3)
  })
  
  # Print them in the plot window
  indivPlots <- lapply(namesPdf, function(i) {
    ggplot(tibble_split[[i]], aes(rank, `term frequency`)) +
      geom_line(color = "#0099f9") + 
      geom_point(size = 0.5) +
      scale_x_log10() +
      scale_y_log10() +
      ggtitle(names(i)) +
      labs(
        title = i,
        subtitle = "Zipf's law") +
      theme(
        plot.title = element_text(color = "#0099f9", size = 15, face ="bold", 
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 10, face ="bold", hjust = 0.5),
        axis.text.x=element_text(size=8),
        axis.text.y=element_text(size=8),
        axis.title=element_text(size=9,face="bold")
      ) 
    # Save the file
    #ggsave(paste0("2. Zipf law/1. Plots/", i, ".png"), device = "png", width=3,
    #height=3)
  })
  print(indivPlots)
  
  # Compare (individual plots) with its own linear regression
  linearRegs <- list()
  
  for(k in 1:length(namesPdf)){
    reg <- lm(log10(`term frequency`) ~ log10(rank), 
              data = tibble_split[[names(tibble_split)[k]]])
    coeff <- list(coefficients(reg))
    linearRegs <- append(linearRegs, coeff)
  }
  
  # Add regression line to each individual plot
  indivPlotsReg <- lapply(namesPdf, function(i) {
    #png(file = paste("Point 2/Linear Reg", i, "- Reg.png"), width = 3.5, 
    #height = 2.5, units = "in", res = 1100, pointsize = 4)
    ggplot(tibble_split[[i]], aes(rank, `term frequency`)) +
      geom_line(color = "#0099f9") + 
      geom_point(size = 0.5) +
      geom_abline(intercept = linearRegs[[which(namesPdf == i)]][1], 
                  slope = linearRegs[[which(namesPdf == i)]][2], 
                  color = "gray50", linetype = 2) +
      scale_x_log10() +
      scale_y_log10() +
      ggtitle(names(i)) +
      labs(
        title = i,
        subtitle = "Zipf's law vs Linear Regression",
        caption = paste0("Intercept: ", 
                         format(round(linearRegs[[which(namesPdf == i)]][1], 2), 
                                nsmall = 2), ", Slope: ", 
                         format(round(linearRegs[[which(namesPdf == i)]][2], 2), 
                                nsmall = 2))) +
      
      theme(
        plot.title = element_text(color = "#0099f9", size = 15, face ="bold", 
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 10, face ="bold", hjust = 0.5),
        axis.text.x=element_text(size=8),
        axis.text.y=element_text(size=8),
        axis.title=element_text(size=9,face="bold"),
        plot.caption = element_text(color = "#009900", size = 7, face ="bold", 
                                    hjust = 0.5),
      ) 
    
    # Save the file
    ggsave(paste0("2. Zipf law/2. Plots vs LReg/", i, ".png"), device = "png", 
           width=3,height=3)
    #dev.off()
  })
  
  cat(green("Done!\n"))
}



