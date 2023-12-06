#Install required packages
required.packages <- c("data.table","jsonlite", "rstudioapi", "httr")
install.packages(required.packages[!(required.packages %in% installed.packages())])
lapply(required.packages, require, character.only=T)

#Set working directory as local folder
setwd(dirname(dirname(getActiveDocumentContext()$path)))

#Choose a poverty line and starting year of analysis (pre-1981 data is incomplete)
chosen_poverty_line <- 2.15
first_year <- 1981

##
##### Start of API querying ######

#Set up API base address
pip <- "https://api.worldbank.org/pip/v1/pip?"

#Read in effective extreme poverty lines
ipls <- fread("project_data/effective_ipls.csv")
ipls <- ipls[is.na(poverty_line), poverty_line := 1]

#Select only national IPLs where they exist for the year-country combination
ipls <- ipls[, .SD[reporting_level == "national" | !any(reporting_level == "national")], by = .(country_code, year)]

#Set IPLs to chosen poverty line
ipls[, poverty_line := poverty_line * chosen_poverty_line]

#Get current PIP data at the IPL
pip_call <- paste0(pip, "country=all&fill_gaps=true&povline=", chosen_poverty_line)
pip_current <- suppressWarnings(rbindlist(content(GET(pip_call))))
pip_current <- pip_current[reporting_year >= first_year]
pip_current[, year := reporting_year]
pip_current <- pip_current[, .SD[reporting_level == "national" | !any(reporting_level == "national")], by = .(country_code, year)]

#Either read or create the output file
if(length(list.files("project_data", pattern = paste0("^projected_", chosen_poverty_line, "_poverty[.]csv$"))) == 1){
  ipl_out <- fread(paste0("project_data/projected_", chosen_poverty_line, "_poverty.csv"))
} else {
  ipl_out <- data.table(country_code = character(), year = integer())
}

#List the IPLs which are outstanding to do based on the read output file and current PIP data
ipls_todo <- ipls[!(paste0(country_code, reporting_level, year) %in% ipl_out[, paste0(country_code, reporting_level, year)]) & year >= first_year]
ipls_todo <- unique(ipls_todo[!(paste0(country_code, reporting_level, year)) %in% pip_current[, paste0(country_code, reporting_level, year)]])

#Iterate through the outstanding ipls
if(nrow(ipls_todo) > 0){
  for(i in 1:nrow(ipls_todo)){
    if(exists("ipl_response")) rm(ipl_response)
    
    #Read parameters for the API call from the list of ipls
    ipl_r <- ipls_todo[i]
    cc <- ipl_r$country_code
    rep_lvl <- ipl_r$reporting_level
    reporting_year <- ipl_r$reporting_year
    pov_line <- round(ipl_r$poverty_line, 3)
    fill <- ipl_r$fill
    
    #Message country code and year to console to indicate progress
    message(cc, ipl_r$year)
    
    #Create the API call address based on parameters
    pip_call <- paste0(pip, "country=", cc, "&year=", reporting_year, "&povline=", pov_line, "&reporting_level=", rep_lvl, "&fill_gaps=", fill)
    
    pip_response <- RETRY("GET", pip_call)
    
    #Check whether the call is valid
    if(status_code(pip_response) != 404){
      
      #If valid, read the data from it
      ipl_response <- suppressWarnings(rbindlist(content(pip_response)))
    } else {
      
      ipl_response <- data.table()
    }
    
    #Fill in the effective year and original country code to the outputted data
    ipl_response$year <- ipl_r$year
  
    #Append the outputted data to the output table and write it to the local folder
    ipl_out <- rbind(ipl_out, ipl_response, fill = T)
    fwrite(ipl_out, paste0("project_data/projected_", chosen_poverty_line, "_poverty.csv"))
  }
}

##### End of API querying #####
##

##
##### Start of analysis ######

#Total calculations
extreme_pov <- unique(rbind(pip_current, ipl_out))

extreme_pov[, year := as.integer(year)]
extreme_pov <- extreme_pov[order(country_code, reporting_level, year)]

#Read WUP population function from remote repo
source("https://raw.githubusercontent.com/devinit/gha_automation/main/general/wupData.R")

#Calculate number of poor
wupPop <- wup_get()
wupPop[area == "total", area := "national"]
extreme_pov <- merge(extreme_pov, wupPop[, .(country_code = ISO3, reporting_level = area, year = as.integer(year), population)], all.x = T)
extreme_pov[, poor := headcount * population]

#Fill gaps with regional estimates
pip_call <- paste0(pip, "country=all&year=2019&fill_gaps=true")
pip_response <- data.table(fromJSON(pip_call))

missing_pip_countries <- pip_response[!(country_code %in% extreme_pov$country_code), .(country_name, country_code, region_name, region_code, estimation_type = "regional")]
regional_headcounts <- extreme_pov[!is.na(poor), .(headcount = sum(poor)/sum(population)), by = .(year, region_name)]

wb_call <- "https://api.worldbank.org/v2/country?per_page=500&format=json"
wb_response <- data.table(fromJSON(wb_call)[[2]])

missing_wb_countries <- wb_response[region.id != "NA" & !(id %in% extreme_pov$country_code), .(country_name = name, country_code = id, region_name = trimws(region.value), region_code = region.id, income = incomeLevel.value, estimation_type = "regional")]
missing_wb_countries[income == "High income", region_name := "Other High Income Countries"][, income := NULL]

missing_wb_countries <- merge(missing_wb_countries, regional_headcounts, by = c("region_name"), all.x = T, allow.cartesian = T)
missing_wb_countries <- merge(missing_wb_countries, wupPop[area == "national", .(country_code = ISO3, year = as.integer(year), population)], all.x = T, by = c("country_code", "year"))

missing_wb_countries[, poor := headcount * population]

extreme_pov <- rbind(extreme_pov, missing_wb_countries, fill = T)[order(year, country_code)]

#Total by year
extreme_poverty_global <- extreme_pov[!is.na(poor), .(poor = sum(poor), population = sum(population)), by = year][, effective_headcount := poor/population][]
fwrite(extreme_poverty_global, paste0("output/projected_", chosen_poverty_line, "_poverty_global.csv"))

#Total by country
extreme_poverty_cc <- extreme_pov[!is.na(poor),  .(poor = sum(poor), population = sum(population)), by = .(country_code, year)][, effective_headcount := poor/population][]
fwrite(extreme_poverty_cc, paste0("output/projected_", chosen_poverty_line, "_poverty_country.csv"))

##### End of analysis #####
##