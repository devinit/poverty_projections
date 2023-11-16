#Install required packages
required.packages <- c("data.table","jsonlite", "rstudioapi", "httr")
install.packages(required.packages[!(required.packages %in% installed.packages())])
lapply(required.packages, require, character.only=T)

#Set working directory as local folder
setwd(dirname(dirname(getActiveDocumentContext()$path)))

##
##### Start of API querying ######\

#Set up API base address
pip <- "https://api.worldbank.org/pip/v1/pip?"

#Read in effective extreme poverty lines
ipls <- fread("project_data/effective_ipls.csv")

#Melt to 'long' format
ipls <- melt(ipls, id.vars = "countrycode", variable.factor = F)

#Extract the reporting year (for API use), and effective year
ipls[, `:=` (reporting_year = substr(variable, 5, 8), effective_year = substr(variable, nchar(variable) - 3, nchar(variable)))]

#Either read or create the output file
if(length(list.files("project_data", pattern = "^projected_expoverty[.]csv$")) == 1){
  ipl_out <- fread("project_data/projected_expoverty.csv")
} else {
  ipl_out <- data.table(cc = character(), effective_year = integer())
}

#List the ipls which are outstanding to do based on the read output file
ipls_todo <- ipls[!(paste0(countrycode, effective_year) %in% ipl_out[, paste0(cc, effective_year)])]

#Iterate through the outstanding ipls
for(i in 1:nrow(ipls_todo)){
  if(exists("ipl_response")) rm(ipl_response)
  
  #Read parameters for the API call from the list of ipls
  ipl_r <- ipls_todo[i]
  cc <- ipl_r$countrycode
  rep_lvl <- "all"
  reporting_year <- ipl_r$reporting_year
  pov_line <- round(ipl_r$value, 3)
  
  #Message country code and year to console to indicate progress
  message(cc, ipl_r$effective_year)
  
  #Select subnational data when required
  if(nchar(cc) > 3){
    
    rep_lvl <- ifelse(substr(cc, 5, 5) == "R", "rural", "urban")
    cc <- substr(cc, 0, 3)
  }
  
  #Create the API call address based on parameters
  pip_call <- paste0(pip, "country=", cc, "&year=", reporting_year, "&povline=", pov_line, "&reporting_level=", rep_lvl, "&fill_gaps=true")
  
  #Check whether the call is valid
  if(status_code(GET(pip_call)) != 404){
    
    #If valid, read the data from it
    ipl_response <- rbindlist(content(GET(pip_call)))
  } else {
    
    ipl_response <- data.table()
  }
  
  #Fill in the effective year and original country code to the outputted data
  ipl_response$effective_year <- ipl_r$effective_year
  ipl_response$cc <- ipl_r$countrycode
  
  #Append the outputted data to the output table and write it to the local folder
  ipl_out <- rbind(ipl_out, ipl_response, fill = T)
  fwrite(ipl_out, "project_data/projected_expoverty.csv")
}

##### End of API querying #####
##

##
##### Start of analysis ######

#Total calculations
extreme_pov <- ipl_out

extreme_pov <- extreme_pov[country_name != ""]
extreme_pov[, effective_year := as.integer(effective_year)]
extreme_pov <- extreme_pov[order(cc, effective_year)]

#Fill SSD with SDN pre-2008
extreme_pov[cc == "SSD" & is.na(headcount), headcount := extreme_pov[cc == "SDN" & extreme_pov[cc == "SSD", is.na(headcount)], headcount]]

#Fill other blanks with nearest value
extreme_pov[, headcount := nafill(headcount, "nocb"), by = .(cc)]
extreme_pov[, headcount := nafill(headcount, "locf"), by = .(cc)]

#Read WUP population function from remote repo
source("https://raw.githubusercontent.com/devinit/gha_automation/main/general/wupData.R")

#Calculate number of poor
wupPop <- wup_get()
wupPop[, cc := paste0(ISO3, ifelse(area == "total", "", ifelse(area == "rural", "-R", "-U")))]
extreme_pov <- merge(extreme_pov, wupPop[, .(cc, effective_year = as.integer(year), population)], all.x = T)
extreme_pov[, poor := headcount * population]

#Fill gaps with regional estimates
pip_call <- paste0(pip, "country=all&year=2019&fill_gaps=true")
pip_response <- data.table(fromJSON(pip_call))

missing_pip_countries <- pip_response[!(country_code %in% extreme_pov$country_code), .(country_name, country_code, region_name, region_code, estimation_type = "regional")]
regional_headcounts <- extreme_pov[, .(headcount = sum(poor)/sum(population)), by = .(effective_year, region_name)]

missing_pip_countries <- merge(missing_pip_countries, regional_headcounts, by = c("region_name"), all.x = T, allow.cartesian = T)
missing_pip_countries <- merge(missing_pip_countries, wupPop[, .(cc, country_code = cc, effective_year = as.integer(year), population)], all.x = T, by = c("country_code", "effective_year"))

missing_pip_countries[, poor := headcount * population]

extreme_pov <- rbind(extreme_pov, missing_pip_countries, fill = T)[order(effective_year, country_code)]

wb_call <- "https://api.worldbank.org/v2/country?per_page=500&format=json"
wb_response <- data.table(fromJSON(wb_call)[[2]])

missing_wb_countries <- wb_response[region.id != "NA" & !(id %in% extreme_pov$country_code), .(country_name = name, country_code = id, region_name = trimws(region.value), region_code = region.id, income = incomeLevel.value, estimation_type = "regional")]
missing_wb_countries[income == "High income", region_name := "Other High Income Countries"][, income := NULL]

missing_wb_countries <- merge(missing_wb_countries, regional_headcounts, by = c("region_name"), all.x = T, allow.cartesian = T)
missing_wb_countries <- merge(missing_wb_countries, wupPop[, .(cc, country_code = cc, effective_year = as.integer(year), population)], all.x = T, by = c("country_code", "effective_year"))

missing_wb_countries[, poor := headcount * population]

extreme_pov <- rbind(extreme_pov, missing_wb_countries, fill = T)[order(effective_year, country_code)]

#Total by year
extreme_poor_total <- extreme_pov[!is.na(poor), .(poor = sum(poor), population = sum(population)), by = effective_year][, effective_headcount := poor/population][]
fwrite(extreme_poor_total, "output/projected_expoverty_global.csv")

#Total by country
extreme_poor_cc <- extreme_pov[!is.na(poor),  .(poor = sum(poor), population = sum(population)), by = .(cc, effective_year)][, effective_headcount := poor/population][]
extreme_poor_cc <- dcast(extreme_poor_cc, cc ~ effective_year, value.var = "poor")
fwrite(extreme_poor_cc, "output/projected_expoor_country.csv")

##### End of analysis #####
##