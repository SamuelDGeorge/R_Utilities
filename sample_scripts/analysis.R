source("C:/Users/sdgeo/Dropbox/Own/Programming/R/R_Utilities/tools/Drug_Package.R")
source("C:/Users/sdgeo/Dropbox/Own/Programming/R/R_Utilities/tools/Utility_package.R")
source("C:/Users/sdgeo/Dropbox/Own/Programming/R/R_Utilities/tools/CrisprFunctionScripts.R")


data <- import_plate_range("C:/Users/sdgeo/Dropbox/Own/Programming/R/hM1A_LYE.xlsx", "A8:K14")
parsed = build_parsed_bliss_map(data)


