#Install required packages
required.packages <- c("data.table", "rstudioapi", "openxlsx", "reshape2")
install.packages(required.packages[!(required.packages %in% installed.packages())])
lapply(required.packages, require, character.only=T)

#Set working directory as local folder
setwd(dirname(dirname(getActiveDocumentContext()$path)))

# Load subnational data
subnat = read.xlsx("project_data/global-subnational-poverty-gsap-2019-data.xlsx")
names(subnat)[6] = "welfaretype_code" # Account for duplicate col name

# Fix corrupted IDs
subnat$geo_code2_new[
  which(subnat$geo_code2_new=="CPV_2022_GADM1_20" & subnat$sample=="9-Brava")
] = "CPV_2022_GADM1_20_BRAVA"
subnat$geo_code2_new[
  which(subnat$geo_code2_new=="CPV_2022_GADM1_20" & subnat$sample!="9-Brava")
] = "CPV_2022_GADM1_20_VICENTE"
subnat$geo_code2_new[
  which(subnat$geo_code2_new=="MLI_2015_" & subnat$sample=="11 - Menaka")
] = "MLI_2015_ADM1_MENAKA"
subnat$geo_code2_new[
  which(subnat$geo_code2_new=="MLI_2015_" & subnat$sample!="11 - Menaka")
] = "MLI_2015_ADM1_TAO"

# Load imputed extreme poverty
extreme_poverty_cc = fread("output/projected_2.15_poverty_country.csv")
extreme_poverty_cc = unique(extreme_poverty_cc) # Remove PHL

# Calculate percent change in national poverty headcount from 2019 to 2023
extreme_poverty_cc = extreme_poverty_cc[,c("country_code", "year", "effective_headcount")]
extreme_poverty_cc = subset(extreme_poverty_cc, year %in% c(2019, 2023))
extreme_poverty_cc_melt = melt(extreme_poverty_cc, id.vars=c("country_code", "year"))
extreme_poverty_cc_wide = dcast(extreme_poverty_cc_melt, country_code~year)
extreme_poverty_cc_wide$pct_change_ratio = (
  extreme_poverty_cc_wide$`2023` / extreme_poverty_cc_wide$`2019`
)
extreme_poverty_cc_wide = extreme_poverty_cc_wide[,c("country_code", "pct_change_ratio")]

# Account for zero on either side of fraction
extreme_poverty_cc_wide$pct_change_ratio[which(is.nan(extreme_poverty_cc_wide$pct_change_ratio))] = 1
extreme_poverty_cc_wide$pct_change_ratio[which(!is.finite(extreme_poverty_cc_wide$pct_change_ratio))] = 0

# Merge
setnames(extreme_poverty_cc_wide, "country_code", "code")
subnat = merge(subnat, extreme_poverty_cc_wide, by="code")

# Calc and truncate
subnat$poor215_ln = subnat$poor215_ln * subnat$pct_change_ratio
subnat$poor215_ln[which(subnat$poor215_ln > 1)] = 1 # Only 1 subnational region
subnat$poor215_ln[which(subnat$poor215_ln < 0)] = 0 # No subnational regions
subnat = subnat[,c("code", "baseyear", "geo_code2_new", "poor215_ln")]
fwrite(subnat, file="output/projected_2.15_poverty_subnational.csv")
