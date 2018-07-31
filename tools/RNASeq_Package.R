if (!require("stringr")) {
  install.packages("stringr", dependencies = TRUE)
  library(stringr)
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