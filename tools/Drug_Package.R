if (!require("drc")) {
  install.packages("drc", dependencies = TRUE)
  library(drc)
}

if (!require("RColorBrewer")) {
  install.packages("RColorBrewer", dependencies = TRUE)
  library(RColorBrewer)
}

if (!require("gplots")) {
  install.packages("gplots", dependencies = TRUE)
  library(gplots)
}

if (!require("readxl")) {
  install.packages("tidyverse", dependencies = TRUE)
  library(readxl)
}

library(tools)

read_xlsx <- readxl::read_xlsx
heatmap.2 <- gplots::heatmap.2
file_ext <- tools::file_ext

un_craig_plate <- function(Plate) {
  rownames(Plate) = rev(as.vector(rownames(Plate)))
  colnames(Plate) = rev(as.vector(colnames(Plate)))
  
  fixed_plate <- matrix(rep(0, times = ncol(Plate) * nrow(Plate)),ncol = ncol(Plate), nrow = nrow(Plate))
  row_index <- 1
  column_index <- 1
  for ( i in nrow(Plate):1 ) {
    for (j in ncol(Plate):1) {
      fixed_plate[row_index,column_index] <- Plate[i,j]
      column_index = column_index + 1
    }
    column_index = 1
    row_index = row_index + 1
  }
  
  rownames(fixed_plate) <- as.vector(rownames(Plate))
  colnames(fixed_plate) <- as.vector(colnames(Plate))
  return(fixed_plate)
}

growth_effect_calculator <- function(raw_growth, column_to_normalize_by = 1, row_to_normalize_by = 1){
  growth_plate <- raw_growth/raw_growth[row_to_normalize_by,column_to_normalize_by]
  return(growth_plate)
}

kill_effect_matrix <- function(PlateOne,PlateTwo,Blank,CraigFactor){
  fold_over = growth_effect_matrix(PlateOne,PlateTwo,Blank,CraigFactor)
  Kill_effect = 1-fold_over
  return(Kill_effect)
}

kill_effect_matrix <- function(growth_effect_matrix, CraigFactor = FALSE){
  fold_over = growth_effect_matrix
  if (CraigFactor == TRUE) {
    fold_over = un_craig_plate(growth_effect_matrix)
  }
  Kill_effect = 1-fold_over
  return(Kill_effect)
}

growth_effect_matrix <- function(PlateOne,PlateTwo,Blank,CraigFactor) {
  if (CraigFactor == TRUE) {
    PlateOne = un_craig_plate(PlateOne)
    PlateTwo = un_craig_plate(PlateTwo)
    Blank = un_craig_plate(Blank)
  }  
  data_plate <- (PlateOne+PlateTwo)/2
  fold_over = data_plate/Blank
  fold_over = (fold_over/fold_over[1,1])
  return(fold_over)
}

bliss_calculator <- function(PlateOne,PlateTwo,Blank,CraigFactor) {
  bliss <- matrix(ncol = ncol(PlateOne) - 1,nrow = nrow(PlateOne) - 1)
  Kill_effect <- kill_effect_matrix(PlateOne,PlateTwo,Blank,CraigFactor)
  
  row_index = 1
  column_index = 1
  for(i in 2:nrow(PlateOne)) {
    for(j in 2:ncol(PlateOne)) {
      fr <- Kill_effect[i,1]
      fb <- Kill_effect[1,j]
      union = fr * fb
      bliss[row_index,column_index] <- ((fr + fb) - union)/Kill_effect[i,j]
      column_index = column_index + 1
    }
    column_index = 1
    row_index = row_index + 1
    
  }
  
  row_names = as.vector(rownames(Kill_effect))
  col_names = as.vector(colnames(Kill_effect))
  
  row_names <- row_names[-1]
  col_names <- col_names[-1]
  
  rownames(bliss) <- row_names
  colnames(bliss) <- col_names
  
  return(bliss)
}

bliss_calculator <- function(growth_effect_matrix,CraigFactor = FALSE) {
  bliss <- matrix(ncol = ncol(growth_effect_matrix) - 1,nrow = nrow(growth_effect_matrix) - 1)
  Kill_effect <- kill_effect_matrix(growth_effect_matrix,CraigFactor)
  
  row_index = 1
  column_index = 1
  for(i in 2:nrow(Kill_effect)) {
    for(j in 2:ncol(Kill_effect)) {
      fr <- Kill_effect[i,1]
      fb <- Kill_effect[1,j]
      union = fr * fb
      bliss[row_index,column_index] <- ((fr + fb) - union)/Kill_effect[i,j]
      column_index = column_index + 1
    }
    column_index = 1
    row_index = row_index + 1
    
  }
  
  row_names = as.vector(rownames(Kill_effect))
  col_names = as.vector(colnames(Kill_effect))
  
  row_names <- row_names[-1]
  col_names <- col_names[-1]
  
  rownames(bliss) <- row_names
  colnames(bliss) <- col_names
  
  return(bliss)
}

bliss_calculator_celltox <- function(growth_effect_matrix,CraigFactor = FALSE) {
  bliss <- matrix(ncol = ncol(growth_effect_matrix) - 1,nrow = nrow(growth_effect_matrix) - 1)
  Kill_effect <- growth_effect_matrix - 1
  
  row_index = 1
  column_index = 1
  for(i in 2:nrow(Kill_effect)) {
    for(j in 2:ncol(Kill_effect)) {
      fr <- Kill_effect[i,1]
      fb <- Kill_effect[1,j]
      bliss[row_index,column_index] <- (fr + fb)/Kill_effect[i,j]
      column_index = column_index + 1
    }
    column_index = 1
    row_index = row_index + 1
    
  }
  
  row_names = as.vector(rownames(Kill_effect))
  col_names = as.vector(colnames(Kill_effect))
  
  row_names <- row_names[-1]
  col_names <- col_names[-1]
  
  rownames(bliss) <- row_names
  colnames(bliss) <- col_names
  
  return(bliss)
}



plot_growth_curves <- function(PlateOne,PlateTwo,Blank,CraigFactor = FALSE,jpeg_name = "default.jpg") {
  growth_effect = growth_effect_matrix(PlateOne,PlateTwo,Blank,CraigFactor)
  df1 <- data.frame(as.numeric(as.vector(colnames(growth_effect))))
  
  colors <- brewer.pal(nrow(PlateOne),"Set1")
  
  
  for(i in 1:nrow(PlateOne)) {
    df1 <- cbind(df1,as.vector(growth_effect[i,]))
  }
  
  jpeg(filename=jpeg_name,res=600,height = 10,width = 10,units = "in")
  index = 2;
  for(i in 1:nrow(p1)) {
    mL <- drm(df1[,index] ~ df1[,1],data = df1 ,fct = L.4(), type="continuous")
    if (i == 1) {
      plot(mL,type="all",col = colors[i],pch = i,lwd = 5,xlab = NA,ylab = NA,main = "Combination Points",ylim= c(0,1))
    }else {
      plot(mL,type="all",col = colors[i],add=TRUE,pch = i,lwd = 3,xlab = NA,ylab = NA)
    }
    index = index + 1
  }
  
  legend("topright",lwd = 2,col = colors,pch = c(1:nrow(PlateOne)),legend=as.character(rownames(growth_effect)))
  
  dev.off()
}

print_growth_curves <- function(growth_effect_matrix,jpeg_name = "default.jpg", numCurves = 2,CraigFactor = FALSE) {
  df1 <- data.frame(as.numeric(as.vector(colnames(growth_effect_matrix))))
  
  colors <- brewer.pal(nrow(growth_effect_matrix),"Set1")
  
  if(numCurves > nrow(growth_effect_matrix) || numCurves < 0) {
    return()
  }
  
  for(i in 1:numCurves) {
    df1 <- cbind(df1,as.vector(growth_effect_matrix[i,]))
  }
  
  jpeg(filename=jpeg_name,res=600,height = 10,width = 10,units = "in")
  index = 2;
  for(i in 1:numCurves) {
    mL <- drm(df1[,index] ~ df1[,1],data = df1 ,fct = LL.4(fixed = c(NA,NA,NA,NA)), type="continuous")
    if (i == 1) {
      plot(mL,type="all",col = colors[i],pch = i,lwd = 5,xlab = NA,ylab = NA,main = "Combination Points",ylim= c(0,2))
    }else {
      plot(mL,type="all",col = colors[i],add=TRUE,pch = i,lwd = 3,xlab = NA,ylab = NA)
    }
    index = index + 1
  }
  
  legend("topright",lwd = 2,col = colors,pch = c(1:numCurves),legend=as.character(rownames(growth_effect_matrix)[1:numCurves]))
  
  dev.off()
}


build_parsed_bliss_map <- function(growth_effect_matrix, max_gi50 = 0.95, min_gi50 = 0.05){  
  data_bliss <- bliss_calculator(growth_effect_matrix)
  cols_to_keep = c()
  row_to_keep = c()
  
  for (item in 1:ncol(growth_effect_matrix)){
    curr_item = growth_effect_matrix[1,item]
    if( curr_item < max_gi50 && curr_item > min_gi50) {
      cols_to_keep = c(cols_to_keep,item)
    }
  }
  
  for (item in 1:nrow(growth_effect_matrix)){
    curr_item = growth_effect_matrix[item, 1]
    if( curr_item < max_gi50 && curr_item > min_gi50) {
      row_to_keep = c(row_to_keep,item)
    }
  }
  row_to_keep = row_to_keep - 1
  cols_to_keep = cols_to_keep - 1
  
  keep <- data_bliss[row_to_keep, cols_to_keep]
  return(keep)
}

build_celltox_parsed_bliss_map <- function(growth_effect_matrix, max_gi50 = 0.95, min_gi50 = 0.05){  
  data_bliss <- bliss_calculator_celltox(growth_effect_matrix)
  cols_to_keep = c()
  row_to_keep = c()
  
  for (item in 1:ncol(growth_effect_matrix)){
    curr_item = growth_effect_matrix[1,item]
    if( curr_item < max_gi50 && curr_item > min_gi50) {
      cols_to_keep = c(cols_to_keep,item)
    }
  }
  
  for (item in 1:nrow(growth_effect_matrix)){
    curr_item = growth_effect_matrix[item, 1]
    if( curr_item < max_gi50 && curr_item > min_gi50) {
      row_to_keep = c(row_to_keep,item)
    }
  }
  row_to_keep = row_to_keep - 1
  cols_to_keep = cols_to_keep - 1
  
  keep <- data_bliss[row_to_keep, cols_to_keep]
  return(keep)
}


print_growth_curves_error <- function(growth_effect_matrix, error_matrix,jpeg_name = "default.jpg", numCurves = 2,CraigFactor = FALSE) {
  df1 <- data.frame(as.numeric(as.vector(colnames(growth_effect_matrix))))
  df2 <- data.frame(as.numeric(as.vector(colnames(error_matrix))))
  df2[1,1] = .001
  
  colors <- brewer.pal(nrow(growth_effect_matrix),"Set1")
  
  if(numCurves > nrow(growth_effect_matrix) || numCurves < 0) {
    return()
  }
  
  for(i in 1:numCurves) {
    df1 <- cbind(df1,as.vector(growth_effect_matrix[i,]))
    df2 <- cbind(df2,as.vector(error_matrix[i,]))
  }
  
  
  jpeg(filename=jpeg_name,res=600,height = 10,width = 10,units = "in")
  index = 2;
  for(i in 1:numCurves) {
    mL <- drm(df1[,index] ~ df1[,1],data = df1 ,fct = LL.4(fixed = c(NA,NA,NA,NA)), type="continuous")
    if (i == 1) {
      plot(mL,type="all",col = colors[i],pch = i,lwd = 5,xlab = NA,ylab = NA,main = "Combination Points",ylim= c(0,2))
      arrows(df2[,1], df1[,index] - df2[,index], df2[,1], df1[,index] + df2[,index], length=0.05, angle=90, code=3, col = colors[i])
    }else {
      plot(mL,type="all",col = colors[i],add=TRUE,pch = i,lwd = 3,xlab = NA,ylab = NA)
      arrows(df2[,1], df1[,index] - df2[,index], df2[,1], df1[,index] + df2[,index], length=0.05, angle=90, code=3, col = colors[i])
    }
    index = index + 1
  }
  dev.off()
}

import_plate <- function(FileName) {
  ext = file_ext(FileName)
  if (ext == "xlsx"){
    df <- as.data.frame(read_xlsx(FileName, col_names = TRUE))
    rows <- as.vector(df[,1])
    df[,1] <- NULL
    rownames(df) <- rows
    return(as.matrix(df))
  } else if (ext == "csv"){
    #range only supported in xlsx files
    df <- read.csv2(FileName, sep = ',', check.names = FALSE)
    rows <- as.vector(df[,1])
    cols <- colnames(df[,2:ncol(df)])
    df[,1] <- NULL
    rownames(df) <- rows
    matrix <- as.matrix(df)
    matrix <- mapply(matrix, FUN=as.numeric)
    matrix <- matrix(matrix, ncol = ncol(df), nrow = nrow(df))
    colnames(matrix) = cols
    rownames(matrix) = rows
    return(matrix)
  } 
  return(NULL)
}

import_plate_range <- function(FileName, ImportRange) {
  ext = file_ext(FileName)
  if (ext == "xlsx"){
    df <- as.data.frame(read_xlsx(FileName, col_names = TRUE, range = ImportRange))
    rows <- as.vector(df[,1])
    df[,1] <- NULL
    rownames(df) <- rows
    return(as.matrix(df))
  } else if (ext == "csv"){
    #range only supported in xlsx files
    df <- read.csv2(FileName, sep = ',', check.names = FALSE)
    rows <- as.vector(df[,1])
    cols <- colnames(df[,2:ncol(df)])
    df[,1] <- NULL
    rownames(df) <- rows
    matrix <- as.matrix(df)
    matrix <- mapply(matrix, FUN=as.numeric)
    matrix <- matrix(matrix, ncol = ncol(df), nrow = nrow(df))
    colnames(matrix) = cols
    rownames(matrix) = rows
    return(matrix)
  } 
  return(NULL)
}




three_d_bliss <- function (plate_list,concentration_list,craig_factor) {
  #fix craigs brain
  if(craig_factor) {
    for(index in 1:length(plate_list)) {
      current = plate_list[[index]]
      plate_list[[index]] = un_craig_plate(current)
      
    }
  }  
  
  normalization_factor = plate_list[[1]] [1,1]
  
  #normalize to 0,0,0 IE no drug
  for (index in 1:length(plate_list)) {
    for( i in 1: nrow(plate_list[[index]])){
      for (j in 1:ncol(plate_list[[index]])) {
        plate_list[[index]][i,j] = plate_list[[index]][i,j]/normalization_factor
      }
      
    }
  }
  
  #Make kill effects
  for (index in 1:length(plate_list)) {
    for( i in 1: nrow(plate_list[[index]])){
      for (j in 1:ncol(plate_list[[index]])) {
        plate_list[[index]][i,j] = 1 - plate_list[[index]][i,j]
      }
      
    }
  }
  
  gem_alone_kill_effects <- c(rep(0,length(plate_list)))
  
  for (dex in 1:length(plate_list)) {
    gem_alone_kill_effects[dex] = plate_list[[dex]][1,1]
  }
  
  print(gem_alone_kill_effects)
  
  empty_matrix <- matrix(rep(0,(ncol(plate_list[[1]]) - 1) * (nrow(plate_list[[1]]) -1)),ncol = (ncol(plate_list[[1]]) - 1), nrow = (nrow(plate_list[[1]]) -1))
  synergy_list <- list()
  
  for (kelly in 1:length(plate_list)) {
    synergy_list[[kelly]] <- empty_matrix
    print(kelly)
  }
  
  print(plate_list[[1]])
  print(gem_alone_kill_effects)
  
  for (index in 1:length(plate_list)) {
    row_index = 1;
    for( i in 2: nrow(plate_list[[index]])){
      column_index = 1;
      for (j in 2:ncol(plate_list[[index]])) {
        #drug one alone
        drug_one_alone <- plate_list[[index]][1,j]
        
        #drug two alone
        drug_two_alone <- plate_list[[index]][i,1]
        
        #drug three alone
        drug_three_alone <- gem_alone_kill_effects[index]
        
        #actual kill effect
        actual_kill_effect <- plate_list[[index]][i,j]
        
        overlap <- (drug_one_alone * drug_three_alone * drug_two_alone) + (drug_one_alone * drug_two_alone) + (drug_two_alone * drug_three_alone) + (drug_one_alone * drug_three_alone)
        bliss_score <- ((drug_one_alone + drug_two_alone + drug_three_alone) - overlap)/actual_kill_effect
        
        synergy_list[[index]][row_index,column_index] <- bliss_score
        empty_matrix[row_index,column_index] <- bliss_score
        
        column_index = column_index + 1
      }
      row_index = row_index + 1
      column_index = 1
    }
  }
  
  row_title <- as.vector(rownames(plate_list[[1]]))
  column_title <- as.vector(colnames(plate_list[[1]]))
  
  row_title <- row_title[-1]
  column_title <- column_title[-1]
  
  for(index in 1:length(synergy_list)) {
    rownames(synergy_list[[index]]) <- row_title
    colnames(synergy_list[[index]]) <- column_title
    
  }
  
  return(synergy_list)
  
}

print_heatmap_bliss_fine <- function(data_plate,export_name) {
  color_range = 1000
  
  jpeg(filename=export_name,res=600,height = 12,width = 12,units = "in")
  
  Synergy <- colorRampPalette(c("red","white"))(n = color_range)
  antagonism <- colorRampPalette(c("white","blue"))(n = color_range)
  
  color_matrix <- matrix(rep(0,nrow(data_plate) * ncol(data_plate)), nrow = nrow(data_plate), ncol = ncol(data_plate))
  
  for (i in 1:nrow(data_plate)) {
    for(j in 1:ncol(data_plate)) {
      current_value <- data_plate[i,j]
      if (current_value <= 0) {
        color_matrix[i,j] = Synergy[1]
      } else if (current_value > 2) {
        color_matrix[i,j] = antagonism[color_range]
      } else if (current_value == 1){
        color_matrix[i,j] = "#FFFFFF"
      }else if (current_value < 1) {
        color_index <- current_value * color_range
        color_index = trunc(color_index)
        if(color_index == 0){color_index = 1}
        color_matrix[i,j] = Synergy[color_index]
      } else if (current_value > 1) {
        color_index = (current_value - 1) * color_range
        color_index = trunc(color_index)
        if(color_index == 0) {color_index =1}
        color_matrix[i,j] = antagonism[color_index]
      } 
    }
  }
  
  row_title <- as.vector(rownames(data_plate))
  column_title <- as.vector(colnames(data_plate))
  
  #rownames(color_matrix) <- row_title
  colnames(color_matrix) <- column_title
  
  heatmap.2(data_plate, dendrogram = "none",
            Rowv = NA, 
            Colv = NA, 
            xlab = "LY3009120 [uM]", 
            ylab = "SCH772984 [uM]",
            col = color_matrix,
            trace='none')
  
  dev.off()
  
}

print_heatmap_bliss <- function(data_plate,export_name="default_heatmap.jpg",xlabel="x-Axis",ylabel="y-Axis") {
  
  
  color_range = 399
  
  jpeg(filename=export_name,res=600,height = 12,width = 12,units = "in")
  
  Synergy <- colorRampPalette(c("red","white","Blue"))(n = color_range)
  colors_pal = sort(c(seq(-1,-0.001, length=200), seq(0,1,length=200)))

  for (i in 1:nrow(data_plate)) {
    for(j in 1:ncol(data_plate)) {
      current_value <- data_plate[i,j]
      if (current_value <= 0) {
        data_plate[i,j] = -1
      } else if (current_value > 2) {
        data_plate[i,j] = 1
      } else {
        data_plate[i,j] = current_value - 1
      } 
    }
  }
  
  heatmap.2(data_plate, dendrogram = "none",
            Rowv = NA, 
            Colv = NA, 
            xlab = xlabel, 
            ylab = ylabel,
            col = Synergy,
            trace='none',
            density.info = c("none"),
            keysize = 1,
            symkey = FALSE,
            breaks = colors_pal)

  dev.off()
  
}

calculate_bliss_sum <- function(data_plate,export_name="default_heatmap.csv",xlabel="x-Axis",ylabel="y-Axis") {
  for (i in 1:nrow(data_plate)) {
    for(j in 1:ncol(data_plate)) {
      current_value <- data_plate[i,j]
      if (current_value <= 0) {
        data_plate[i,j] = -1
      } else if (current_value > 2) {
        data_plate[i,j] = 1
      } else {
        data_plate[i,j] = current_value - 1
      } 
    }
  }
  
  bliss_sum = 0
  for (i in 1:nrow(data_plate)) {
    for(j in 1:ncol(data_plate)) {
      current_value <- data_plate[i,j]
      bliss_sum = current_value + bliss_sum
    }
  }
  return(bliss_sum)
}

calculate_bliss_sum_v2 <- function(data_plate,export_name="default_heatmap.csv",xlabel="x-Axis",ylabel="y-Axis") {
  for (i in 1:nrow(data_plate)) {
    for(j in 1:ncol(data_plate)) {
      current_value <- data_plate[i,j]
      if (current_value < 0) {
        data_plate[i,j] = 0
      } else if (current_value > 2) {
        data_plate[i,j] = 1
      } else {
        data_plate[i,j] = current_value - 1
      } 
    }
  }
  
  bliss_sum = 0
  for (i in 1:nrow(data_plate)) {
    for(j in 1:ncol(data_plate)) {
      current_value <- data_plate[i,j]
      bliss_sum = current_value + bliss_sum
    }
  }
  return(bliss_sum)
}

print_heatmap_crispr <- function(data_plate,export_name="default_heatmap.jpg",xlabel="x-Axis",ylabel="y-Axis") {
  
  color_range = 500
  
  jpeg(filename=export_name,res=600,height = 12,width = 12,units = "in")
  
  Synergy <- colorRampPalette(c("red","white","Blue"))(n = color_range)
  
  
  color_matrix <- matrix(rep(0,nrow(data_plate) * ncol(data_plate)), nrow = nrow(data_plate), ncol = ncol(data_plate))
  i = 2
  j = 2
  for (i in 1:nrow(data_plate)) {
    for(j in 1:ncol(data_plate)) {
      current_value <- data_plate[i,j]
      data_plate[i,j] = current_value - 1
      if(data_plate[i,j] > 1) {
        data_plate[i,j] = 1
      }
    }
  }
  
  heatmap.2(data_plate, dendrogram = "none",
            xlab = xlabel, 
            ylab = ylabel,
            col = Synergy,
            trace='none',
            symkey = TRUE)
  
  dev.off()
  
}

parse_bliss_plate <- function(data_plate) {
  for (i in 1:nrow(data_plate)) {
    for(j in 1:ncol(data_plate)) {
      current_value <- data_plate[i,j]
      if (current_value < 0) {
        data_plate[i,j] = 0
      } else if (current_value > 2) {
        data_plate[i,j] = 1
      } else {
        data_plate[i,j] = current_value - 1
      } 
    }
  }
  return(data_plate)
}

print_heatmap_bliss_v2 <- function(data_plate,export_name="default_heatmap.jpg",xlabel="x-Axis",ylabel="y-Axis") {
  
  color_range = 399
  
  jpeg(filename=export_name,res=600,height = 12,width = 12,units = "in")
  
  Synergy <- colorRampPalette(c("red","white","Blue"))(n = color_range)
  colors_pal = sort(c(seq(-1,-0.001, length=200), seq(0,1,length=200)))
  
  for (i in 1:nrow(data_plate)) {
    for(j in 1:ncol(data_plate)) {
      current_value <- data_plate[i,j]
      if (current_value < 0) {
        data_plate[i,j] = 0
      } else if (current_value > 2) {
        data_plate[i,j] = 1
      } else {
        data_plate[i,j] = current_value - 1
      } 
    }
  }
  
  heatmap.2(data_plate, dendrogram = "none",
            Rowv = NA, 
            Colv = NA, 
            xlab = xlabel, 
            ylab = ylabel,
            col = Synergy,
            trace='none',
            density.info = c("none"),
            keysize = 1,
            symkey = FALSE,
            breaks = colors_pal)
  
  dev.off()
  
}

print_heatmap_bliss_parsed_v1 <- function(growth_plate,export_name="default_heatmap.jpg", GI_Max = 0.95, GI_Min = 0.05, xlabel="x-Axis",ylabel="y-Axis") {
  data_plate <- build_parsed_bliss_map(growth_plate, max_gi50 = GI_Max, min_gi50 = GI_Min)
  color_range = 399
  
  jpeg(filename=export_name,res=600,height = 12,width = 12,units = "in")
  
  Synergy <- colorRampPalette(c("red","white","Blue"))(n = color_range)
  colors_pal = sort(c(seq(-1,-0.001, length=200), seq(0,1,length=200)))
  
  for (i in 1:nrow(data_plate)) {
    for(j in 1:ncol(data_plate)) {
      current_value <- data_plate[i,j]
      if (current_value < 0) {
        data_plate[i,j] = 0
      } else if (current_value > 2) {
        data_plate[i,j] = 1
      } else {
        data_plate[i,j] = current_value - 1
      } 
    }
  }
  
  heatmap.2(data_plate, dendrogram = "none",
            Rowv = NA, 
            Colv = NA, 
            xlab = xlabel, 
            ylab = ylabel,
            col = Synergy,
            trace='none',
            density.info = c("none"),
            keysize = 1,
            symkey = FALSE,
            breaks = colors_pal)
  
  dev.off()
  
}

print_heatmap_bliss_celltox_v1 <- function(growth_plate,export_name="default_heatmap.jpg", xlabel="x-Axis",ylabel="y-Axis") {
  data_plate <- bliss_calculator_celltox(growth_plate)
  color_range = 399
  
  jpeg(filename=export_name,res=600,height = 12,width = 12,units = "in")
  
  Synergy <- colorRampPalette(c("red","white","Blue"))(n = color_range)
  colors_pal = sort(c(seq(-1,-0.001, length=200), seq(0,1,length=200)))
  
  for (i in 1:nrow(data_plate)) {
    for(j in 1:ncol(data_plate)) {
      current_value <- data_plate[i,j]
      if (current_value < 0) {
        data_plate[i,j] = 0
      } else if (current_value > 2) {
        data_plate[i,j] = 1
      } else {
        data_plate[i,j] = current_value - 1
      } 
    }
  }
  
  heatmap.2(data_plate, dendrogram = "none",
            Rowv = NA, 
            Colv = NA, 
            xlab = xlabel, 
            ylab = ylabel,
            col = Synergy,
            trace='none',
            density.info = c("none"),
            keysize = 1,
            symkey = FALSE,
            breaks = colors_pal)
  
  dev.off()
  
}


pipelined_print_heatmap <- function(InputFilePath, OutputFileName) {
  OutputName = paste(OutputFileName, ".jpg",sep = "")
  print_heatmap_bliss(import_plate(InputFilePath), OutputName)
}

pipelined_print_heatmap <- function(InputFilePath, OutputFileName, data_location) {
  OutputName = paste(OutputFileName, ".jpg",sep = "")
  print_heatmap_bliss(import_plate_range(InputFilePath, data_location), OutputName)
}

pipelined_print_heatmap_v2 <- function(InputFilePath, OutputFileName, data_location) {
  OutputName = paste(OutputFileName, ".jpg",sep = "")
  print_heatmap_bliss_v2(import_plate_range(InputFilePath, data_location), OutputName)
}

pipelined_print_bliss_growth_v1 <- function(InputFilePath, OutputFileName, data_location) {
  OutputName = paste(OutputFileName, ".jpg",sep = "")
  plate <- import_plate_range(InputFilePath, data_location)
  plate <- bliss_calculator(plate)
  print_heatmap_bliss_v2(plate, OutputName)
}

pipelined_bliss_heatmap_parsed_v1 <- function(InputFilePath, OutputFileName, data_location, GI_Max = 0.95, GI_Min = 0.05) {
  OutputName = paste(OutputFileName, ".jpg",sep = "")
  print_heatmap_bliss_parsed_v1(import_plate_range(InputFilePath, data_location), OutputName, GI_Max, GI_Min)
}

pipelined_bliss_heatmap_celltox_v1 <- function(InputFilePath, OutputFileName, data_location) {
  OutputName = paste(OutputFileName, ".jpg",sep = "")
  print_heatmap_bliss_celltox_v1(import_plate_range(InputFilePath, data_location), OutputName)
}

pipelined_print_bliss_sum <- function(InputFilePath, OutputFileName, data_location, combined_file="default_file.csv") {
  current_file <- NULL
  bliss_scores <- import_plate_range(InputFilePath, data_location)
  bliss_sum <- calculate_bliss_sum(bliss_scores)
  if(file.exists(combined_file)){ ## requires that we overwrite our "final_file_name" for each addition
    current_file = read.csv(combined_file, header = TRUE, na.strings = c("",0,"<NA>","NA"), row.names = 1)
  } else { ## this is for processing the first data frame/heatmap column
    current_file = data.frame(row.names = c("Bliss_Score_Sum"))
  }
  current_file$default <- bliss_sum
  names(current_file)[ncol(current_file)]<-OutputFileName
  write.csv(current_file, file = combined_file)
}

pipelined_print_bliss_sum_v2 <- function(InputFilePath, OutputFileName, data_location, combined_file="default_file.csv") {
  current_file <- NULL
  bliss_scores <- import_plate_range(InputFilePath, data_location)
  bliss_sum <- calculate_bliss_sum_v2(bliss_scores)
  if(file.exists(combined_file)){ ## requires that we overwrite our "final_file_name" for each addition
    current_file = read.csv(combined_file, header = TRUE, na.strings = c("",0,"<NA>","NA"), row.names = 1)
  } else { ## this is for processing the first data frame/heatmap column
    current_file = data.frame(row.names = c("Bliss_Score_Sum"))
  }
  current_file$default <- bliss_sum
  names(current_file)[ncol(current_file)]<-OutputFileName
  write.csv(current_file, file = combined_file)
}

pipelined_print_heatmap_fine <- function(InputFilePath, OutputFileName) {
  OutputName = paste(OutputFileName, ".jpg",sep = "")
  print_heatmap_bliss_fine(import_plate(InputFilePath), OutputName)
}

pipelined_print_general <- function(InputFilePath, OutputFileName, xaxis = "X-Axis", yaxis = "Y-Axis") {
  BlissPlate <- import_plate_range(InputFilePath, "O26:X31")
  GrowthPlate <- import_plate_range(InputFilePath, "N10:X16")
  
  BlissName <- paste(OutputFileName, "-Bliss.jpg", sep = "")
  toPrint <- paste("Printing Bliss For: ", BlissName,sep = "")
  print(toPrint)
  print_heatmap_bliss(BlissPlate,BlissName,xaxis,yaxis)
  
  
  
  GrowthName <- paste(OutputFileName, "-Raw.jpg", sep = "")
  toPrint <- paste("Printing Graph For: ", GrowthName,sep = "")
  print(toPrint)
  print_growth_curves(GrowthPlate,GrowthName)
  
}

pipelined_print_general_Error <- function(InputFilePath, OutputFileName, xaxis = "X-Axis", yaxis = "Y-Axis") {
  BlissPlate <- import_plate_range(InputFilePath, "O26:X31")
  BlissError <- import_plate_range(InputFilePath, "AB26:AK31")
  GrowthPlate <- import_plate_range(InputFilePath, "N10:X16")
  GrowthError <- import_plate_range(InputFilePath, "AA10:AK16")
  
  BlissName <- paste(OutputFileName, "-Bliss.jpg", sep = "")
  toPrint <- paste("Printing Bliss For: ", BlissName,sep = "")
  print(toPrint)
  print_heatmap_bliss(BlissPlate,BlissName,xaxis,yaxis)
  
  
  
  GrowthName <- paste(OutputFileName, "-Raw.jpg", sep = "")
  toPrint <- paste("Printing Graph For: ", GrowthName,sep = "")
  print(toPrint)
  print_growth_curves_error(GrowthPlate,GrowthError,GrowthName)
  
}

pipelined_print_bliss_scores_from_raw_growth <- function(Input_file_name = "default.csv",output_name = "Mean_Raw", data_location){
  output_name <- paste(output_name,".csv", sep = "")
  raw_data = import_plate_range(Input_file_name, data_location)
  raw_data = (raw_data/raw_data[1,1])
  bliss = bliss_calculator(raw_data)
  write.csv(bliss, file = output_name)
}

pipelined_uncraig_batch_normalize <- function(InputFilePath, OutputFileName, data_location) {
  OutputName = paste(OutputFileName, ".jpg",sep = "")
  plate <- un_craig_plate(import_plate_range(InputFilePath, data_location))
  plate <- growth_effect_calculator(plate)
  write.csv(plate, file = paste(OutputFileName,".csv", sep = ""))
}

Print_reproducibility_DM_Mean <- function(ReplicateA,ReplicateB, xlabel = "ReplicateB", ylabel = "ReplicateA", xymax  = 3, xymin = 0, export_name = "default.jpg", label_point = FALSE) {
  
  ReplicateA <- as.data.frame(read.csv(ReplicateA))
  ReplicateB <- as.data.frame(read.csv(ReplicateB))
  
  
  ReplicateA_ordered <- ReplicateA[order(ReplicateA$Gene),]
  ReplicateB_ordered <- ReplicateB[order(ReplicateB$Gene),]
  
  jpeg(filename=export_name,res=600,height = 12,width = 12,units = "in")
  
  plot(ReplicateA_ordered$DM_mean, ReplicateB_ordered$DM_mean, xlab = xlabel, ylab = ylabel, xlim = c(xymin,xymax), ylim = c(xymin,xymax))
  if (label_point) {
    if(xymax - xymin < 3) {
      YLabelValue = ReplicateB_ordered$DM_mean + .006
      text(ReplicateA_ordered$DM_mean, YLabelValue, labels = ReplicateB_ordered$Gene, cex = .7)  
    } else {
      YLabelValue = ReplicateB_ordered$DM_mean + .1
      text(ReplicateA_ordered$DM_mean, YLabelValue, labels = ReplicateB_ordered$Gene, cex = .7)
    }
    
  }
  abline(0,1)
  
  dev.off()
}
