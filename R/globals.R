utils::globalVariables(c("Worm_Length", "Metadata_Plate", "Metadata_Well",
                         "Parent_WormObjects", "num_worms", "model_select",
                         "model", "worm_count", "AreaShape_Center_X", "AreaShape_Center_Y",
                         "remove_outliers", "worm_length_um", "well_outlier_flag",
                         "flag_removed", "sd_wormlength_um", "mean_wormlength_um"))

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  {utils::globalVariables(c("."))}
