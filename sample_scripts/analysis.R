source("C:/Users/sdgeo/Dropbox/Own/Programming/R/R_Utilities/tools/Drug_Package.R")
source("C:/Users/sdgeo/Dropbox/Own/Programming/R/R_Utilities/tools/Utility_package.R")
source("C:/Users/sdgeo/Dropbox/Own/Programming/R/R_Utilities/tools/CrisprFunctionScripts.R")


data <- import_plate_range("C:/Users/sdgeo/Dropbox/Own/Programming/R/hM1A_LYE.xlsx", "A8:K14")
parsed = build_parsed_bliss_map(data)

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
