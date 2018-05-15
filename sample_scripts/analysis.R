source("C:/Users/sdgeo/Dropbox/Own/Programming/R/R_Utilities/tools/Drug_Package.R")
source("C:/Users/sdgeo/Dropbox/Own/Programming/R/R_Utilities/tools/Utility_package.R")


build_map_3d("012718_lye_cq_day3_blissanalysis_Pa14C.xlsx")

build_curves("Luminespib_Viability_2D.xlsx")

build_curves_3d("Quisinostat_Viability_2D.xlsx")

plate_string = "Pa02c Docetaxel.xlsx"

build_map <- function(plate_string, export_name) {
  doce <- import_plate(plate_string)
  doce_bliss <- bliss_calculator(doce)
  print_heatmap_bliss(doce_bliss, export_name = "map.jpg")
}

build_map_3d <- function(plate_string, export_name) {
  doce <- import_plate_range(plate_string, "N10:X16")
  doce_bliss <- bliss_calculator(doce)
  print_heatmap_bliss(doce_bliss, export_name = paste(plate_string, "_heatmap.jpg", sep = ""))
}

build_curves <- function(plate_string, export_name) {
  doce <- import_plate(plate_string)
  print_growth_curves(doce,jpeg_name = paste(plate_string, "_curves.jpg", sep = ""), numCurves = 2,CraigFactor = FALSE)
}

build_curves_3d <- function(plate_string, export_name) {
  doce <- import_plate_range(plate_string, "N10:X16")
  print_growth_curves(doce,jpeg_name = paste(plate_string, "_curves.jpg", sep = ""), numCurves = 2,CraigFactor = FALSE)
}
