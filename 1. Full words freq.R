# cleaning around
# ==============================================================================
rm(list=ls()) # removing variables
graphics.off() # clearing plotting devices
cat("\014") # clearing console
# ==============================================================================



# load the extraction function
# ==============================================================================
source("Aux_pdfsExtractor.R")
# ==============================================================================

fullWordsFreqHist <- function() {
  # Get input files
  pdfsFolderPath <- file.path(getwd(), "Inputs/PDFs")
  pdfsPaths <- list.files(pdfsFolderPath, pattern="*.pdf", full.names=TRUE)
  
  # Delete old output files
  unlink("1. Full words freq/*")
  
  # Indicate output path for the file
  outputPath <- "1. Full words freq/AllWords.xlsx"
  
  # Indicate sheet name for the excel file
  sheetName <- "AllWords"
  
  # Create workbook and sheet for the excel file
  wBook <- createWorkbook()
  addWorksheet(wBook, sheetName)
  showGridLines(wBook, 1, showGridLines = FALSE)
  saveWorkbook(wBook, file = outputPath, overwrite = TRUE)
  outFullList <- list()
  
  # Write results in the excel file and add format to it
  colCounter <- 1
  secondCount <- 0
  for (i in pdfsPaths) {
    # Use the function called freqFullWords from Aux_pdfsExtractor
    outFull <- freqFullWords(i, "english")
    outFullList <- append(outFullList, list(outFull))
    myHeaderStyle <- openxlsx::createStyle(fontSize = 12, fontColour= "#FFFFFF", 
                                           halign = "center",
                                           fgFill = "#0000FF", 
                                           border = "TopBottomLeftRight",  
                                           borderStyle ="medium",
                                           valign = "center", 
                                           textDecoration = "bold",
                                           wrapText = FALSE)
    myPdfNamesStyle <- openxlsx::createStyle(fontSize = 12, 
                                             fontColour = "#FFFFFF", 
                                             halign = "center",
                                             fgFill = "#990033", 
                                             border = "TopBottomLeftRight",  
                                             borderStyle ="medium",
                                             valign = "center", 
                                             textDecoration = "bold",
                                             wrapText = FALSE)
    writeData(wBook, sheetName, outFull, startRow = 2, startCol = colCounter, 
              headerStyle = myHeaderStyle, borders = "surrounding",
              borderStyle = "medium")
    writeData(wBook, sheetName, tools::file_path_sans_ext(basename(i)), 
              startRow = 1, startCol = colCounter, borders = "surrounding",
              borderStyle = "medium")
    
    setColWidths(wBook, sheetName, cols = colCounter + 2, widths = "1")
    secondCount <- colCounter + 1
    mergeCells(wBook, sheetName, cols = colCounter:secondCount, rows=1)
    addStyle(wBook, sheet = sheetName, style = myPdfNamesStyle, rows = 1, 
             cols = colCounter)
    
    
    colCounter <-  colCounter + 3
  }
  saveWorkbook(wBook, file = outputPath, overwrite = TRUE)
  
  
  # Build a histogram per pdf (with 10 most common terms)
  index <- 1
  for (i in pdfsPaths) {
    # Create the data for the chart
    topFreq <- head(unlist(outFullList[[index]][2]),10)
    topWord <- head(unlist(outFullList[[index]][1]),10)
    
    # Create the png for each plot
    png(file = paste("1. Full words freq/hist_", 
                     tools::file_path_sans_ext(basename(i)), ".png"), 
        width = 3.5, height = 2.5, units = "in", res = 1100, pointsize = 4)
    
    # Plot the bar chart 
    par(mar=c(12, 10, 6, 4), mgp=c(8, 0.5, 0), las=1)
    barplot(topFreq,names.arg = topWord, xlab = "Top words", ylab = "Frequency",
            col = "#990033",
            main = tools::file_path_sans_ext(basename(i)), las=2,
            cex.axis=1.5, cex.names=1.5, cex.main = 2, cex.lab = 2)
    
    # Save the file
    dev.off()
    
    index <- index + 1
  }
  cat(green("Done!\n"))
}



