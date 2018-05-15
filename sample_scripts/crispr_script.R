source("C:/Users/sdgeo/Dropbox/Own/Programming/R/R_Utilities/tools/packages.R")
source("C:/Users/sdgeo/Dropbox/Own/Programming/R/R_Utilities/tools/CrisprFunctionScripts.R")
source("C:/Users/sdgeo/Dropbox/Own/Programming/R/R_Utilities/tools/Utility_package.R")


#Use this to translate the raw data in one folder into proper output for each condition
pipeline(pipelined_print_mean_chart, input_location = "C:/Users/sdgeo/Dropbox/Der Lab/Data/NamCRISPRAnalysis/RawData/all_but_pancs_final/all_input_files", output_location = "C:/Users/sdgeo/Dropbox/Der Lab/Data/NamCRISPRAnalysis/RawData/all_but_pancs_final/all_output_files", c("DMSO", "LY3"), parallel_run = TRUE, environment = c(lsf.str()))

#Use this to combine all the data from one chart to make a heatmap of a single feature
#call in wd of output data above
#Only call once, use below for further
build_combined_and_print_heatmap_from_folder(column_to_isolate = "assignment_vect", output_name = "Full_Map_vect", threshold = 1, min_columns_to_parse = 2, xaxis_title = "X-axis", yaxis_title = "Y-axis" )

#Use this to further print parsed heatmaps for a sum of a certain amount using a Full Map as a guide 
print_parsed_heatmap_from_raw_csv("Full_Map_vect.csv", sum_to_parse = 4, output_name_start = "Threshold_4_Heatmap")


#Can use this alternatively to print the combined heatmap with thresholds
pipeline(pipelined_print_combined_heatmap, input_location = "C:/Users/sdgeo/Dropbox/Der Lab/Data/NamCRISPRAnalysis/RawData/test_csv_wk4", output_location = "C:/Users/sdgeo/Dropbox/Der Lab/Data/NamCRISPRAnalysis/RawData/output_csv_files/Sam", c("DMSO.Week.4", "LY3.Week.4","Final_Heatmap.csv","assignment_vect",6,4))


#Does the first two above in one step.   
pipeline_with_finish_function(pipelined_print_mean_chart, build_combined_and_print_heatmap_from_folder,input_location = "C:/Users/sdgeo/Dropbox/Der Lab/Data/NamCRISPRAnalysis/RawData/irem_analysis_raw_files-3-30-2018", output_location = "C:/Users/sdgeo/Dropbox/Der Lab/Data/NamCRISPRAnalysis/RawData/irem_analysis_out_files-3-30-2018", func_one_additional_args =  c("DMSO", "LY3"), func_two_args = c("assignment_vect","Final_Heatmap",6,4), parallel_run = TRUE, environment = c(lsf.str()))

#Print the DM Mean
Print_reproducibility_DM_Mean("C:/Users/sdgeo/Dropbox/Der Lab/Data/NamCRISPRAnalysis/RawData/output_raw_files/Pa16c_LY3_repA_wk2_DMSO.Week.2.csv","C:/Users/sdgeo/Dropbox/Der Lab/Data/NamCRISPRAnalysis/RawData/output_raw_files/Pa16c_LY3_repA_wk4_DMSO.Week.4.csv", xymax = .7, xymin = 0, label_point = TRUE, xlabel = "Pa16C-Week2", ylabel = "Pa16C-week4", export_name = "Pa16C-Crispr-Cut.jpg")

