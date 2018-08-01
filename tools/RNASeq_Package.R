if (!require("XML")) {
  install.packages("XML", dependencies = TRUE)
  libarary(XML)
}

if (!require("DESeq2")) {
  source("https://bioconductor.org/biocLite.R")
  biocLite("DESeq2")
  library(DESeq2)
}

if (!require("tximport")) {
  source("https://bioconductor.org/biocLite.R")
  biocLite("tximport")
  library(tximport)
}

if (!require("readr")) {
  install.packages("readr", dependencies = TRUE)
  library(readr)
}

if (!require("tximportData")) {
  source("https://bioconductor.org/biocLite.R")
  biocLite("tximportData")
  library(tximportData)
}

if (!require("stringr")) {
  install.packages("stringr", dependencies = TRUE)
  library(stringr)
}

if (!require("stringi")) {
  install.packages("stringi", dependencies = TRUE)
  library(stringi)
}

if (!require("tidyr")) {
  install.packages("tidyr", dependencies = TRUE)
  library(tidyr)
}

if (!require("RColorBrewer")) {
  install.packages("RColorBrewer", dependencies = TRUE)
  library(RColorBrewer)
}

if (!require("gplots")) {
  install.packages("gplots", dependencies = TRUE)
  library(gplots)
}

if (!require("gtools")) {
  install.packages("gtools", dependencies = TRUE)
  library(gtools)
}


print_heatmap_rna <- function(input_file, output_name = "heatmap.jpg", xaxis_title = "X_Axis", yaxis_title = "Y_Axis") {
  color_range = 500
  jpeg(filename=output_name,res=600,height = 12,width = 12,units = "in")
  
  Synergy <- colorRampPalette(c("red","white","Blue"))(n = color_range)
  heatmap.2(as.matrix(input_file),
            dendrogram = "both",
            xlab = NA, 
            ylab = NA,
            col = Synergy,
            trace='none',
            symkey = TRUE,
            cexRow = 0.4,
            cexCol = 0.3,
            margin = c(6,6))
  
  dev.off()
  
}

normalize_to_column <- function(Full_Data, columns_to_select = "Mia", column_to_normalize_by = "MiaP_V4") {
  Data_Subset <- Full_Data[,grep(columns_to_select,colnames(Full_Data))]
  Data_Subset <- sweep(Data_Subset, MARGIN = 1, FUN = "/", STATS = Data_Subset[,7])[,-grep(column_to_normalize_by, colnames(Data_Subset))]
  Data_Subset <- log(Data_Subset, 2)
  Data_Subset[is.nan(Data_Subset)] <- 0
  Data_Subset[is.infinite(Data_Subset)] <- 0
  Full_Data <- Full_Data[,-grep(column_to_normalize_by, colnames(Full_Data))]
  Full_Data_Overlap <- colnames(Data_Subset)
  
  for(i in 1:length(Full_Data_Overlap)){
    current_item = Full_Data_Overlap[i]
    Full_Data[,current_item] <- Data_Subset[,current_item]
  }
  return(Full_Data)
}

normalize_multiple_column <- function(Data, columns_list = c("Mia"), columns_to_normalize_by_list = c("MiaP_V4")) {
  for (item in 1:length(columns_list)) {
    Data <- normalize_to_column(Data, columns_to_select = columns_list[item], column_to_normalize_by = columns_to_normalize_by_list[item])
  }
  return(Data)
}

pipelined_build_count_file <- function(InputFilePath, OutputFileName, total_file_name = "Default.csv") {
  current_table = read.table(InputFilePath, sep = "\t", row.names = 1)
  colnames(current_table) <- c(OutputFileName)
  if(file.exists(total_file_name)){
    current_file = read.csv(total_file_name, header = TRUE, na.strings = c("","<NA>","NA"), row.names = 1)
    final_table <- merge(current_file, current_table, by = 0)
    rownames(final_table) <- final_table[,1]
    final_table <- final_table[,-1]
    write.csv(final_table, file = total_file_name)
    return(final_table)
  }
  current_file = current_table
  write.csv(current_file, file = total_file_name)
  return(current_file)
  
}