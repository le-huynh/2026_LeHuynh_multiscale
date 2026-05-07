# Rule
# target : prerequisite1 prerequisite2 prerequisite3
#	(tab)recipe

.PHONY: all clean

################################################################################
#
# Part 1: Data for data analysis
#
# Run scripts to generate dataset for data analysis
#
################################################################################

data/raw/data_station_raw.csv: \
code/get_data_station_raw.R
	Rscript code/get_data_station_raw.R

data/process/data_station_missing.csv: \
code/get_data_station_missing.R\
data/raw/data_station_raw.csv
	Rscript code/get_data_station_missing.R

data/process/data_station_interpolated.csv: \
code/get_data_station_interpolated.R\
data/process/data_station_missing.csv
	Rscript code/get_data_station_interpolated.R

data/process/data_health_station.csv: \
code/get_data_health_station.R\
data/process/data_station_interpolated.csv\
data/raw/edvisit_full_richmond_msa.csv\
data/raw/us_holiday_1900_2100.csv
	Rscript code/get_data_health_station.R

data/process/data_health_era5_ric_msa.csv: \
code/get_data_health_era5_ric_msa.R\
data/raw/era5_cleaned_ric_msa.csv\
data/raw/edvisit_full_richmond_msa.csv\
data/raw/us_holiday_1900_2100.csv
	Rscript code/get_data_health_era5_ric_msa.R

data/process/data_health_era5_zcta.rds: \
code/get_data_health_era5_zcta.R\
data/raw/era5_cleaned_ric_zipcode.rds\
data/raw/edvisit_zcta_richmond_msa.rds\
data/raw/us_holiday_1900_2100.csv
	Rscript code/get_data_health_era5_zcta.R

data/process/data_health_era5_county.rds: \
code/get_data_health_era5_county.R\
data/raw/era5_cleaned_ric_county.rds\
data/raw/edvisit_county_richmond_msa.rds\
data/raw/us_holiday_1900_2100.csv
	Rscript code/get_data_health_era5_county.R

data/process/sf_svi_cluster_geometry.rda data/for_manuscript/supp_svi_cluster.csv: \
code/get_data_svi_cluster_geometry.R\
data/raw/SVI_2022_virginia_county.csv
	Rscript code/get_data_svi_cluster_geometry.R

data/process/data_EDvisit_cluster.rds: \
code/get_data_EDvisit_cluster.R\
data/process/data_health_era5_county.rds\
data/process/sf_svi_cluster_geometry.rda
	Rscript code/get_data_EDvisit_cluster.R

data/process/data_health_era5_cluster.rds: \
code/get_data_health_era5_cluster.R\
data/raw/era5_cleaned_ric_cluster.rds\
data/process/data_EDvisit_cluster.rds\
data/raw/us_holiday_1900_2100.csv
	Rscript code/get_data_health_era5_cluster.R

data/process/data_health_station_zcta.rds: \
code/get_data_health_station_zcta.R\
data/process/data_health_era5_zcta.rds\
data/process/data_health_station.csv
	Rscript code/get_data_health_station_zcta.R

data/process/data_health_station_county.rds: \
code/get_data_health_station_county.R\
data/process/data_health_era5_county.rds\
data/process/data_health_station.csv
	Rscript code/get_data_health_station_county.R

data/process/data_health_station_cluster.rds: \
code/get_data_health_station_cluster.R\
data/process/data_health_era5_cluster.rds\
data/process/data_health_station.csv
	Rscript code/get_data_health_station_cluster.R

################################################################################
#
# Part 2: Formal data analysis
#
# Run scripts to fit models, etc.
#
################################################################################

data/RDSmodel/gam_health_station.rds data/RDSmodel/pgam_health_station.rds: \
code/model_gam_health_station.R\
data/process/data_health_station.csv
	Rscript code/model_gam_health_station.R

data/RDSmodel/gam_health_era5.rds data/RDSmodel/pgam_health_era5.rds: \
code/model_gam_health_era5.R\
data/process/data_health_era5_ric_msa.csv
	Rscript code/model_gam_health_era5.R

data/RDSmodel/gam_health_station_zcta.rds: \
code/model_gam_health_station_zcta.R\
data/process/data_health_station_zcta.rds
	Rscript code/model_gam_health_station_zcta.R

data/RDSmodel/gam_health_era5_zcta.rds: \
code/model_gam_health_era5_zcta.R\
data/process/data_health_era5_zcta.rds
	Rscript code/model_gam_health_era5_zcta.R

data/RDSmodel/gam_health_station_county.rds: \
code/model_gam_health_station_county.R\
data/process/data_health_station_county.rds
	Rscript code/model_gam_health_station_county.R

data/RDSmodel/gam_health_era5_county.rds: \
code/model_gam_health_era5_county.R\
data/process/data_health_era5_county.rds
	Rscript code/model_gam_health_era5_county.R

data/RDSmodel/gam_health_station_cluster.rds: \
code/model_gam_health_station_cluster.R\
data/process/data_health_station_cluster.rds
	Rscript code/model_gam_health_station_cluster.R

data/RDSmodel/gam_health_era5_cluster.rds: \
code/model_gam_health_era5_cluster.R\
data/process/data_health_era5_cluster.rds
	Rscript code/model_gam_health_era5_cluster.R

data/RDSmodel/mixmeta_station_zcta_all.rds data/RDSmodel/pgam_meta_station_zcta_all.rds: \
code/model_mixmeta_station_zcta_all.R\
code/fn_crosspred.R\
data/RDSmodel/gam_health_station_zcta.rds
	Rscript code/model_mixmeta_station_zcta_all.R

data/RDSmodel/mixmeta_era5_zcta_all.rds data/RDSmodel/pgam_meta_era5_zcta_all.rds: \
code/model_mixmeta_era5_zcta_all.R\
code/fn_crosspred.R\
data/RDSmodel/gam_health_era5_zcta.rds
	Rscript code/model_mixmeta_era5_zcta_all.R

data/RDSmodel/gam_health_station_zcta_mean5.rds data/RDSmodel/mixmeta_station_zcta_mean5.rds data/RDSmodel/pgam_meta_station_zcta_mean5.rds: \
code/model_mixmeta_station_zcta_mean5.R\
code/fn_crosspred.R\
data/RDSmodel/gam_health_station_zcta.rds\
data/process/data_health_station_zcta.rds
	Rscript code/model_mixmeta_station_zcta_mean5.R

data/RDSmodel/gam_health_era5_zcta_mean5.rds data/RDSmodel/mixmeta_era5_zcta_mean5.rds data/RDSmodel/pgam_meta_era5_zcta_mean5.rds: \
code/model_mixmeta_era5_zcta_mean5.R\
code/fn_crosspred.R\
data/RDSmodel/gam_health_era5_zcta.rds\
data/process/data_health_era5_zcta.rds
	Rscript code/model_mixmeta_era5_zcta_mean5.R

data/RDSmodel/mixmeta_station_county.rds data/RDSmodel/pgam_meta_station_county.rds: \
code/model_mixmeta_station_county.R\
code/fn_crosspred.R\
data/RDSmodel/gam_health_station_county.rds
	Rscript code/model_mixmeta_station_county.R

data/RDSmodel/mixmeta_era5_county.rds data/RDSmodel/pgam_meta_era5_county.rds: \
code/model_mixmeta_era5_county.R\
code/fn_crosspred.R\
data/RDSmodel/gam_health_era5_county.rds
	Rscript code/model_mixmeta_era5_county.R

data/RDSmodel/mixmeta_station_cluster.rds data/RDSmodel/pgam_meta_station_cluster.rds: \
code/model_mixmeta_station_cluster.R\
code/fn_crosspred.R\
data/RDSmodel/gam_health_station_cluster.rds
	Rscript code/model_mixmeta_station_cluster.R

data/RDSmodel/mixmeta_era5_cluster.rds data/RDSmodel/pgam_meta_era5_cluster.rds: \
code/model_mixmeta_era5_cluster.R\
code/fn_crosspred.R\
data/RDSmodel/gam_health_era5_cluster.rds
	Rscript code/model_mixmeta_era5_cluster.R

data/for_manuscript/supp_summary.csv data/for_manuscript/df_temperature_diff.csv: \
code/table_supp_summary.R\
data/process/data_health_station.csv\
data/process/data_health_era5_ric_msa.csv\
data/process/data_health_era5_cluster.rds\
data/process/data_health_era5_county.rds\
data/process/data_health_era5_zcta.rds
	Rscript code/table_supp_summary.R

data/for_manuscript/supp_dlnm.RDS: \
code/model_supp_dlnm.R\
data/process/data_health_station.csv
	Rscript code/model_supp_dlnm.R

data/for_manuscript/supp_dlnm_comparison.csv: \
code/table_supp_dlnm.R\
data/for_manuscript/supp_dlnm.RDS
	Rscript code/table_supp_dlnm.R

################################################################################
#
# Part 3: Figure and table generation
#
# Run scripts to generate figures
#
################################################################################

results/figures/fig_study_location.pdf: \
code/plot_study_location.R\
data/process/sf_svi_cluster_geometry.rda
	Rscript code/plot_study_location.R

results/figures/fig_correlation.pdf results/figures/fig_correlation_county.pdf data/for_manuscript/df_correlation.csv: \
code/plot_correlation.R\
data/process/data_health_station.csv\
data/process/data_health_era5_ric_msa.csv\
data/process/data_health_era5_cluster.rds\
data/process/data_health_era5_county.rds\
data/process/data_health_era5_zcta.rds
	Rscript code/plot_correlation.R

results/figures/fig_relative_risk.pdf results/figures/fig_relative_risk_supp.pdf: \
code/plot_relative_risk.R\
code/fn_cal_relative_risk.R\
data/RDSmodel/gam_health_station.rds\
data/RDSmodel/gam_health_station_cluster.rds\
data/RDSmodel/gam_health_station_county.rds\
data/RDSmodel/gam_health_station_zcta.rds\
data/RDSmodel/gam_health_station_zcta_mean5.rds\
data/RDSmodel/gam_health_era5.rds\
data/RDSmodel/gam_health_era5_cluster.rds\
data/RDSmodel/gam_health_era5_county.rds\
data/RDSmodel/gam_health_era5_zcta.rds\
data/RDSmodel/gam_health_era5_zcta_mean5.rds\
data/RDSmodel/pgam_meta_station_county.rds\
data/RDSmodel/pgam_meta_station_cluster.rds\
data/RDSmodel/pgam_meta_station_zcta_all.rds\
data/RDSmodel/pgam_meta_station_zcta_mean5.rds\
data/RDSmodel/pgam_meta_era5_county.rds\
data/RDSmodel/pgam_meta_era5_cluster.rds\
data/RDSmodel/pgam_meta_era5_zcta_all.rds\
data/RDSmodel/pgam_meta_era5_zcta_mean5.rds
	Rscript code/plot_relative_risk.R

results/figures/fig_heat_cold.pdf data/for_manuscript/RR_heat_cold.RDS: \
code/plot_heat_cold.R\
code/fn_cal_relative_risk.R\
code/fn_crosspred.R\
data/RDSmodel/gam_health_station.rds\
data/RDSmodel/gam_health_station_cluster.rds\
data/RDSmodel/gam_health_station_county.rds\
data/RDSmodel/gam_health_station_zcta.rds\
data/RDSmodel/gam_health_station_zcta_mean5.rds\
data/RDSmodel/gam_health_era5.rds\
data/RDSmodel/gam_health_era5_cluster.rds\
data/RDSmodel/gam_health_era5_county.rds\
data/RDSmodel/gam_health_era5_zcta.rds\
data/RDSmodel/gam_health_era5_zcta_mean5.rds\
data/RDSmodel/mixmeta_station_county.rds\
data/RDSmodel/mixmeta_station_cluster.rds\
data/RDSmodel/mixmeta_station_zcta_all.rds\
data/RDSmodel/mixmeta_station_zcta_mean5.rds\
data/RDSmodel/mixmeta_era5_county.rds\
data/RDSmodel/mixmeta_era5_cluster.rds\
data/RDSmodel/mixmeta_era5_zcta_all.rds\
data/RDSmodel/mixmeta_era5_zcta_mean5.rds
	Rscript code/plot_heat_cold.R

results/figures/fig_rr_diff.pdf: \
code/plot_rr_diff.R\
code/fn_cal_relative_risk.R\
data/process/data_health_station_zcta.rds\
data/RDSmodel/gam_health_station.rds\
data/RDSmodel/gam_health_station_zcta.rds\
data/RDSmodel/gam_health_era5_zcta.rds
	Rscript code/plot_rr_diff.R; rm Rplots.pdf

results/figures/dlnm_overall_lag3_bs_4_ns_6.png results/figures/dlnm_contour_lag3_bs_4_ns_6.png: \
code/plot_dlnm_supp.R\
data/for_manuscript/supp_dlnm.RDS
	Rscript code/plot_dlnm_supp.R

################################################################################
#
# Part 4: Pull it all together
#
# Render the manuscript
#
################################################################################
manuscript/supporting_information.pdf: \
manuscript/supporting_information.Rmd\
data/for_manuscript/supp_svi_cluster.csv\
data/for_manuscript/supp_gam_model_comparison.csv\
data/for_manuscript/supp_summary.csv\
data/for_manuscript/RR_heat_cold.RDS\
data/for_manuscript/supp_dlnm_comparison.csv\
results/figures/fig_correlation_county.pdf\
results/figures/fig_correlation_zcta1.pdf\
results/figures/fig_correlation_zcta2.pdf\
results/figures/fig_correlation_zcta3.pdf\
results/figures/fig_correlation_zcta4.pdf\
results/figures/fig_relative_risk_supp.pdf\
results/pictures/fig_dlnm_supp.pdf
	R -e 'rmarkdown::render("manuscript/supporting_information.Rmd", output_format="all")'

manuscript/manuscript.pdf: \
manuscript/manuscript.Rmd\
manuscript/data_availability.Rmd\
manuscript/conflict_of_interest_disclosure.Rmd\
results/pictures/fig_study_setting_01.pdf\
results/figures/fig_study_location.pdf\
results/figures/fig_correlation.pdf\
results/figures/fig_relative_risk.pdf\
results/figures/fig_heat_cold.pdf\
results/figures/fig_rr_diff.pdf\
data/for_manuscript/supp_summary.csv\
data/for_manuscript/df_correlation.csv\
data/for_manuscript/df_temperature_diff.csv\
data/for_manuscript/RR_heat_cold.RDS
	R -e 'rmarkdown::render("manuscript/manuscript.Rmd", output_format="all")'

manuscript/response_to_reviewers.pdf: \
manuscript/response_to_reviewers.Rmd
	R -e 'rmarkdown::render("manuscript/response_to_reviewers.Rmd", output_format="all")'

manuscript/figures.pdf: \
manuscript/figures.Rmd\
results/pictures/fig_study_setting_01.pdf\
results/figures/fig_study_location.pdf\
results/figures/fig_correlation.pdf\
results/figures/fig_relative_risk.pdf\
results/figures/fig_heat_cold.pdf\
results/figures/fig_rr_diff.pdf
	R -e 'rmarkdown::render("manuscript/figures.Rmd", output_format="all")'

