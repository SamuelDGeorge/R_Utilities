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


## This function combines most of the upfront data management and analysis. It pulls a csv from input folder,
## and creates a data frame of the two columns specified.
## The end result is a data frame with a new column, "mean", "stdev", "bin_placement" and "cum_rank"
build_mean_chart <- function(filename = "default.csv", column_one_name = "DMSO.Week.2", column_two_name = "LY3.Week.2") {

  raw_data_frame <- extract_data_frame(filename, column_one_name, column_two_name)
  raw_data_frame <- remove_zero(input_dat = raw_data_frame, column_name = column_one_name)
  uniq_genes <- uniq_gene(raw_data_frame)
  raw_data_frame <- impute_mean(input_dat = raw_data_frame, unique_gene_vector = uniq_genes)
  raw_data_frame <- percentage_calc(input_dat = raw_data_frame, column_one_name, column_two_name)
  raw_data_frame <- treat_enrich(input_dat = raw_data_frame, column_one_name, column_two_name)
  raw_data_frame <- construct_mean(input_dat = raw_data_frame, unique_gene_vector = uniq_genes,
                                        enrich.col = "enrich_treated_untreated")
  raw_data_frame <- outlier_remove(input_dat = raw_data_frame, col_name = "stdev", threshold = 3)
  raw_data_frame <- rank_values(input_data = raw_data_frame, column_to_rank = "mean")
  raw_data_frame <- bin_placement(input_dat = raw_data_frame, rank_col = "cum_rank")
}

print_mean_chart <- function(filename = "default.csv", output_name = "mean.csv", column_one_name = "DMSO.Week.2", column_two_name = "LY3.Week.2") {
  to_print <- build_mean_chart(filename,column_one_name,column_two_name)
  write.csv(to_print,file = output_name)

  return(as.data.frame(to_print))
}

pipelined_print_mean_chart <- function(Input_file_name = "default.csv",output_file_name = "Output.csv",column_one_name = "DMSO.Week.2", column_two_name = "LY3.Week.2") {
  output_name <- paste(output_file_name,column_one_name,sep = "_")
  output_name <- paste(output_name,".csv", sep = "")
  print_mean_chart(Input_file_name,output_name,column_one_name,column_two_name)
}

pipelined_print_combined_chart <- function(Input_file_name = "default.csv",output_file_name = "Mean_Raw.csv",column_one_name = "DMSO.Week.2", column_two_name = "LY3.Week.2", final_file_name = "Final_CSV.csv", column_to_carry = "mean") {
  
  if(file.exists(final_file_name)){ ## requires that we overwrite our "final_file_name" for each addition
    already_present <- read.csv(final_file_name, header = TRUE, na.strings = c("",0,"<NA>","NA"), row.names = 1)
    to_add <- build_mean_chart(Input_file_name,column_one_name,column_two_name)
    to_write <- isolate_column(to_add,column_to_carry,Input_file_name)
    final_frame <- merge_frames_by_rowname(already_present,to_write)
    write.csv(final_frame, file = final_file_name)
    
  } else { ## this is for processing the first data frame/heatmap column
    to_add <- build_mean_chart(Input_file_name,column_one_name,column_two_name)
    final_frame <- isolate_column(to_add,column_to_carry,Input_file_name)
    write.csv(final_frame, file = final_file_name)
    
    
  }

}

## The master function that compiles multiple data.frames from an input folder, and outputs/updates existing 
## data frames in the output folder
pipelined_print_combined_heatmap <- function(Input_file_name = "default.csv", 
                                             output_file_name = "Heatmap_Raw.csv",
                                             column_one_name = "DMSO.Week.2", 
                                             column_two_name = "LY3.Week.2", 
                                             final_file_name = "Final_Heatmap.csv", 
                                             column_to_carry = "assignment_vect", 
                                             threshold = 5, 
                                             min_columns_to_parse = 4) {

  if(file.exists(final_file_name)){ ## requires that we overwrite our "final_file_name" for each addition
    already_present <- read.csv(final_file_name, header = TRUE, na.strings = c("",0,"<NA>","NA"), row.names = 1)
    to_add <- build_mean_chart(Input_file_name,column_one_name,column_two_name)
    to_write <- isolate_column(to_add,column_to_carry,Input_file_name)
    final_frame <- merge_frames_by_rowname(already_present,to_write)
    write.csv(final_frame, file = final_file_name)
    print_heatmap(final_frame, output_name = paste(parse_name(final_file_name),".jpg", sep = ""))
    if(ncol(final_frame) >= min_columns_to_parse) {print_heatmap(parse_heatmap(final_file_name, as.numeric(threshold)),output_name = paste(parse_name(final_file_name),"_Parsed.jpg", sep = ""))
      write.csv(parse_heatmap(final_file_name, as.numeric(threshold)), file = paste(parse_name(final_file_name),"_Parsed.csv", sep = ""))
      } 
    
  } else { ## this is for processing the first data frame/heatmap column
    to_add <- build_mean_chart(Input_file_name,column_one_name,column_two_name)
    final_frame <- isolate_column(to_add,column_to_carry,Input_file_name)
    write.csv(final_frame, file = final_file_name)
    
    
  }
  
}


build_combined_file_from_folder <- function(column_to_isolate = "mean", output_name = "combined") {
  files <- list.files()
  if(length(files) < 2) {return()}
  starting_file <- read.csv(files[1], header = TRUE, na.strings = c("","<NA>","NA"), row.names = 1)
  final_file <- isolate_column(starting_file, column_to_isolate, column_to_isolate_name = files[1])  
  for(i in 2:length(files)) {
    next_file <- read.csv(files[i], header = TRUE, na.strings = c("","<NA>","NA"), row.names = 1)
    next_file <- isolate_column(next_file, column_to_isolate, column_to_isolate_name = files[i])
    final_file <- merge_frames_by_rowname(final_file, next_file)
  }
  write.csv(final_file, file = paste(output_name,".csv", sep = ""))
  return(final_file)
}

build_combined_and_print_heatmap_from_folder <- function(column_to_isolate = "mean", output_name = "combined", threshold = 5, min_columns_to_parse = 4, xaxis_title = "X-axis", yaxis_title = "Y-axis" ) {
  #column_to_isolate =  "assignment_vect"
  #output_name = "combined"
  df <- build_combined_file_from_folder(column_to_isolate, output_name)
  print_heatmap(df, output_name = paste(output_name,".jpg",sep = ""), xaxis_title, yaxis_title)
  if(ncol(df) >= min_columns_to_parse) {
    print_heatmap(parse_heatmap_frame(df, as.numeric(threshold)),output_name = paste(parse_name(output_name),"_Parsed.jpg", sep = ""),xaxis_title,yaxis_title)
    write.csv(parse_heatmap_frame(df, as.numeric(threshold)), file = paste(parse_name(output_name),"_Parsed.csv", sep = ""))
  } 
  return(df)
}

merge_frames_by_rowname <- function(frame_one, frame_two) {
  present_genes <- rownames(frame_one)
  new_genes <- rownames(frame_two)
  both_genes <- intersect(present_genes,new_genes)
  common_frame <- data.frame(both_genes) ## data frame that will the basis for merging
  rownames(common_frame) <- common_frame$both_genes
  frame_one$both_genes <- rownames(frame_one) ## need to add a column to merge by
  frame_two$both_genes <- rownames(frame_two) ## need to add a column to merge by
  
  intermediate_df <- merge(common_frame, frame_one, by = "both_genes", all.x = TRUE) 
  final_frame <- merge(intermediate_df, frame_two, by = "both_genes", all.x = TRUE)
  
  rownames(final_frame) <- final_frame$both_genes
  final_frame <- final_frame[,!(names(final_frame) %in% "both_genes")] ## removes the merge column after assigning rownames
  final_frame <- na.replace(final_frame,0) ## if there was an inconsistency in merging, NA is produced. make 0 here
  return(final_frame)
}


isolate_column <- function(input_frame, column_to_isolate = "mean", column_to_isolate_name = "default_column_name") {
  column <- grep(colnames(input_frame), pattern = column_to_isolate)
  final_frame <- data.frame(input_frame[,column])
  rownames(final_frame) <- rownames(input_frame)
  colnames(final_frame) <- c(column_to_isolate_name)
  return(final_frame)
}

print_heatmap <- function(input_file, output_name = "heatmap.jpg", xaxis_title = "X_Axis", yaxis_title = "Y_Axis") {
  color_range = 500
  jpeg(filename=output_name,res=600,height = 12,width = 12,units = "in")
  
  Synergy <- colorRampPalette(c("red","white","Blue"))(n = color_range)
  heatmap.2(as.matrix(input_file),
            dendrogram = "column",
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

print_parsed_heatmap_from_raw_csv <- function(input_file, sum_to_parse = 5, output_name_start = "heatmap_parsed", xaxis_title = "X_Axis", yaxis_title = "Y_Axis") {
  raw_map_present <- parse_heatmap(input_file, sum_to_parse)
  print_heatmap(raw_map_present,output_name = paste(output_name_start,".jpg", sep = ""),xaxis_title,yaxis_title)
  write.csv(raw_map_present, file = paste(output_name_start,".csv", sep = ""))
  
}

## Finds row sums that are greater or less than +/- "threshold" value set in initial call
parse_heatmap <- function(input_file, sum_to_parse = 5) {
  df <- read.csv(input_file, header = TRUE, na.strings = c("","<NA>","NA"), row.names = 1) 
  df$sum_column <- rowSums(df)
  vector <- c()
  for(i in 1: nrow(df)) {
    if (df$sum_column[i] > (-1 * sum_to_parse) && df$sum_column[i] < sum_to_parse) {
      vector <- c(vector,i)
    }
  }
  df <- df[-(vector),]
  df <- df[,!(names(df) %in% "sum_column")]
  return(df)
}
parse_heatmap_frame <- function(df, sum_to_parse = 5) {
  df$sum_column <- rowSums(df)
  vector <- c()
  for(i in 1: nrow(df)) {
    if (df$sum_column[i] > (-1 * sum_to_parse) && df$sum_column[i] < sum_to_parse) {
      vector <- c(vector,i)
    }
  }
  df <- df[-(vector),]
  df <- df[,!(names(df) %in% "sum_column")]
  return(df)
}

parse_name <- function(String_input = "default.csv") {
  name <- strsplit(as.character(String_input),"[.]")
  return(name[[1]][1])
}

## When the pipeline loops through this function, it takes the first three arguments and creates a new data_frame
## with the two desire columns specified in the original call. It can withstand the columns being out of order,
## but the column names need to specified exactly.
extract_data_frame <- function(filename = "default.csv", column_one_name = "DMSO.Week.2", column_two_name = "LY3.Week.2") {
  data_frame <- read.csv(file = filename, header = T, na.strings = c("",0,"<NA>"), row.names = 1)
  one_int <- grep(pattern = column_one_name, colnames(data_frame))
  two_int <- grep(pattern = column_two_name, colnames(data_frame))
  desired_vector <- c(one_int,two_int)
  data_frame <- data_frame[,desired_vector]
  return(data_frame)
}

## pulls the rownames from the data frame, gets rid of the construct number, and retains any unique gene names
uniq_gene <- function(input_dat){
  gene_df <- data.frame(rownames(input_dat))
  gene_list <- strsplit(as.character(gene_df$rownames.input_dat.), "_")
  temp_vect <- c()
  for(i in 1:length(gene_list)){
    temp_vect[i] <- gene_list[[i]][1]
  }
  final_vect <- unique(temp_vect)
  return(final_vect)
}


## for values in the treated column that were lost, we imputed them with the mean of all treated values
## this can be edited in the future if necessary. We thought it would not introduce additional bias, but
## may produce additional false positives (which is preferable to losing True positives)
impute_mean <- function(input_dat, unique_gene_vector){
  for(i in 1:length(unique_gene_vector)){ 
    index <- grep(pattern = unique_gene_vector[i], x = rownames(input_dat))
    treat_vect <- input_dat[index, grep("LY3", colnames(input_dat))]
    treat_vect[which(is.na(treat_vect))] <- mean(treat_vect, na.rm = TRUE)
    input_dat[index,grep("LY3", colnames(input_dat))] <- treat_vect
  }
  return(input_dat)
}

## This function searches for NA values in the DMSO treated cell lines, and removes the entire row.
## If a construct didn't work in DMSO treated, it means the construct likely didnt transfect well.
## rather than imputing these values, we just tossed them. Frequently, the matched treated sample
## was also NA or 0, supporting our "bad construct" hypothesis.
remove_zero <- function(input_dat, column_name = "DMSO") {
  return_dat = input_dat
  column_full_position <- grep(pattern = column_name, colnames(input_dat))
  column_vector <- input_dat[,column_full_position]
  remove_vector <- c()
  index = 1
  for(i in 1:length(column_vector)) {
    if(is.na(column_vector[i])) {
      remove_vector[index] <- i
      index = index + 1
    } else {
      
    }
  }
  return_dat = return_dat[-remove_vector,]
  return(return_dat)
}


## Each read value is converted to a percentage of the total sample reads for the treatment condition.
percentage_calc <- function(input_dat, column_one_name = "DMSO", column_two_name = "LY3"){
  dmso_sum <- sum(input_dat[,grep(column_one_name, colnames(input_dat))], na.rm = TRUE)
  treat_sum <- sum(input_dat[,grep(column_two_name, colnames(input_dat))], na.rm = TRUE)
  input_dat[,grep(column_one_name, colnames(input_dat))] <- (input_dat[,grep(column_one_name, colnames(input_dat))]/dmso_sum)
  input_dat[,grep(column_two_name, colnames(input_dat))] <- (input_dat[,grep(column_two_name, colnames(input_dat))]/treat_sum)
  return(input_dat)
}

## Find the ratio of treated:untreated or LY3/DMSO, values < 1 are sensitizers to LY3
## changed column numbers to column names, utilized Grep. Kept old code below.
## returns a three column data frame, third column is now a ratio
treat_enrich <- function(input_dat, column_one_name = "DMSO", column_two_name = "LY3"){
  enrich_treated_untreated <- c()
  for(i in 1:nrow(input_dat)){
    enrich_treated_untreated[i] <- input_dat[i,grep(column_two_name, colnames(input_dat))]/input_dat[i,grep(column_one_name, colnames(input_dat))]
  }
  return(data.frame(input_dat,enrich_treated_untreated))
}



## This assumes that we have already saved a variable that we can assign to unique_gene_vector
## first creates a new "temp_df" that will accept our data
## avoids missing gene names with a catch "if" statement
## produces new data frame with unique genes as rownames, means and stdevs of the FOLD CHANGE LY3/DMSO
construct_mean <- function(input_dat, unique_gene_vector, enrich.col){
  temp_df <- data.frame(mean = c(rep(0,length(unique_gene_vector))), 
                        stdev = c(rep(0,length(unique_gene_vector))))
  index <- 1
  for(i in 1:length(unique_gene_vector)){
    row_position <- grep(pattern = unique_gene_vector[i], rownames(input_dat))
    if(length(row_position) == 0){
      next
    }
    temp_vector <- input_dat[row_position, enrich.col]
    mean_vector <- mean(temp_vector)
    stdev_vector <- sd(temp_vector)
    temp_df[index,] <- c(mean_vector,stdev_vector)
    rownames(temp_df)[index] <- unique_gene_vector[i]
    index <- index + 1
  }
  return(temp_df)
}

## removes items with stdev greater than 3 (which is pretty high)
## threshold is set in the actual function call later
outlier_remove <- function(input_dat, col_name = "stdev", threshold){
  temp_df <- input_dat[1,] ## initiated a very similar temporary data frame to add values to, can start with any row number
  index = 1
  for(i in 1:nrow(input_dat)){
    value <- input_dat[i,col_name]
    if(is.na(value)){
      next
    }
    else if(value > threshold){
      next
    }else{
      temp_df[index,] <- input_dat[i,]
      index = index + 1
    }
  }
  return(temp_df)
}



## initiates a new column in input_dat "cum_rank" which will take the ranks of each value
rank_values <- function(input_data, column_to_rank = "Mean") {
  column_rank <- grep(pattern = column_to_rank, colnames(input_data))
  input_data$cum_rank <- 0
  rank_column <- grep(pattern = "cum_rank", colnames(input_data)) ## finds the index of "cum_rank" column
  input_data <- input_data[order(input_data[,column_rank]),]
  for (i in 1:length(input_data$cum_rank)) {
    input_data[i,rank_column] = i ## assigns values 1:nrow(input_dat), in order now that we've ordered
  }                               ## lowest rank values are the BEST sensitizers (ie lowest ratios LY3/DMSO)
  
  input_data <- input_data[order(rownames(input_data)), ] ## reorders the data frame by alphabetical gene name
  return(input_data)
}

## creates a new data frame with an additional "assignment_vect" column
bin_placement <- function(input_dat, bin_number = 30, rank_col){
  temp_dat <- input_dat
  length_dat <- nrow(temp_dat)
  bin_size = floor(length_dat / bin_number)
  half_size = bin_number/2
  quarter_size = half_size/2
  assignment_vect <- c()
  for(i in 1:nrow(temp_dat)){
      cum_rank = temp_dat[i,rank_col]
      bin = floor(cum_rank/bin_size)
      bin = bin - floor(half_size)
      bin = bin/quarter_size
      assignment_vect[i] <- bin

    
  }
  temp_dat <- cbind(temp_dat,assignment_vect)
  return(temp_dat)
}


## searches a file path to a working directory and returns a vector of file names
collect_file_vector <- function(FolderPath) {
  files <- list.files(FolderPath)    
  return(files)
}


Print_reproducibility_DM_Mean <- function(ReplicateA,ReplicateB, xlabel = "ReplicateA", ylabel = "ReplicateB", xymax  = 3, xymin = 0, export_name = "default.jpg", label_point = FALSE) {
  ReplicateA <- as.data.frame(read.csv(ReplicateA, header = T, row.names = 1))
  ReplicateB <- as.data.frame(read.csv(ReplicateB, header = T, row.names = 1))
  
  ReplicateA_mean <- data.frame(rownames(ReplicateA),ReplicateA$mean)
  colnames(ReplicateA_mean) <- c("Gene","mean")
  ReplicateB_mean <- data.frame(rownames(ReplicateB),ReplicateB$mean)
  colnames(ReplicateB_mean) <- c("Gene","mean")
  
  Gene = union(rownames(ReplicateA), rownames(ReplicateB))
  
  common_frame = data.frame(Gene)
  
  intermediate_df <- merge(common_frame, ReplicateA_mean, by = "Gene", all.x = TRUE) 
  
  final_frame <- merge(intermediate_df, ReplicateB_mean, by = "Gene", all.x = TRUE)
  
  jpeg(filename=export_name,res=600,height = 12,width = 12,units = "in")
  
  plot(final_frame[,2], final_frame[,3], xlab = xlabel, ylab = ylabel, xlim = c(xymin,xymax), ylim = c(xymin,xymax))
  if (label_point) {
    if(xymax - xymin < 3) {
      YLabelValue = final_frame[,3] + .006
      text(final_frame[,2], YLabelValue, labels = final_frame$Gene, cex = .7)  
    } else {
      YLabelValue = final_frame[,3] + .1
      text(final_frame[,2], YLabelValue, labels = final_frame$Gene , cex = .7)
    }
    
  }
  abline(0,1)
  
  dev.off()
}