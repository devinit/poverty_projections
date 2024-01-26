#Install required packages
required.packages <- c("data.table","jsonlite", "rstudioapi", "httr")
install.packages(required.packages[!(required.packages %in% installed.packages())])
lapply(required.packages, require, character.only=T)

#Set working directory as local folder
setwd(dirname(dirname(getActiveDocumentContext()$path)))

#Set up API base address
pip <- "https://api.worldbank.org/pip/v1/pip?"

#Return year of most recent available poverty data from PIP 
pip_call <- paste0(pip, "fill_gaps=true")
pip_response_fill <- suppressWarnings(rbindlist(content(GET(pip_call)))) #dataset of years when surveys were not conducted

pip_call <- paste0(pip, "fill_gaps=false")
pip_response_nofill <- suppressWarnings(rbindlist(content(GET(pip_call)))) #dataset of years for when survey was conducted
pip_response_nofill <- pip_response_nofill[!(paste0(country_code, reporting_level, reporting_year) %in% pip_response_fill[, paste0(country_code, reporting_level, reporting_year)])] #removes duplicates in fillgaps true and fillgaps false

pip_response <- rbind(pip_response_fill[, fill := "true"], pip_response_nofill[, fill := "false"]) #joins both fillgaps true and fillgaps false and add columns for whether true or false

pip_most_recent <- pip_response[, .SD[which.max(reporting_year), .(reporting_year, fill)], by = .(country_code, reporting_level)] #filtered table for latest report year

#Get HHFCE growth per capita rates from World Bank (household consumption)
hhfce_call <- "https://api.worldbank.org/v2/country/all/indicator/NE.CON.PRVT.PC.KD.ZG?per_page=30000&format=json"
hhfce_response <- data.table(fromJSON(hhfce_call)[[2]])

hhfce_growth <- hhfce_response[countryiso3code != "", .(country_code = countryiso3code, year = date, hhfce_growth = 1+(value/100))][order(country_code, year)] #formating & converts values into %s

#Get GDP growth per capita projections from most recent IMF WEO
tyear <- year(Sys.Date())
tmonth <- month(Sys.Date())
weo_month <- ifelse(tmonth <= 10 & tmonth >= 4, 4, 10)
weo_year <- ifelse(tmonth < 4, tyear-1, tyear)
weo_ver <- format(as.Date(paste("1", weo_month, weo_year, sep = "-"), "%d-%m-%Y"), "%b%Y")

while(T){
  weo_call <- paste0("https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/", weo_year, "/WEO", weo_ver ,"all.ashx")
  weo_response <- GET(weo_call)
  if(weo_response$headers$`content-type` == "application/vnd.ms-excel") break
  
  if(weo_month <= 10 & weo_month > 4){
    weo_month <- 4
  } else {
    if(weo_month <= 4){
      weo_year <- weo_year - 1
    }
    weo_month <- 10
  }
  weo_ver <- format(as.Date(paste("1", weo_month, weo_year, sep = "-"), "%d-%m-%Y"), "%b%Y")
}
message("Using IMF WEO version ", weo_ver, ".")

weo <- content(weo_response)

#-----Changed from Dan's script---
temp_file <- tempfile(fileext = ".xls") # Save the content to a temporary file
writeBin(weo, temp_file)

weo <- fread(temp_file, na.strings = c("n/a", "--")) # Read the data using fread

unlink(temp_file) # Remove the temporary file
#--------------------------------

data_cols <- c("ISO", "WEO Subject Code", grep("^\\d{4}$", names(weo), value = T))
weo <- melt(weo[, ..data_cols], id.vars = c("ISO", "WEO Subject Code"), variable.factor = F) #filters for columns specified in previous line
weo = data.table(weo)
weo[, value := as.numeric(gsub(",", "", value))]
weo[ISO == "WBG", ISO := "PSE"]

weo_gdppc <- weo[`WEO Subject Code` == "NGDPRPC"][order(ISO, variable)] #new table for just gdp per capita

weo_gdppc_growth <- weo_gdppc[, .(year = variable, gdppc_growth = value/shift(value)), by = .(country_code = ISO)] #format column names & convert gdppc to %

#Apply GDP growth modifiers
weo_gdppc_growth[!(country_code %in% c("CHN", "IND")), gdppc_growth := (gdppc_growth-1)*0.87+1]
weo_gdppc_growth[country_code == "CHN", gdppc_growth := (gdppc_growth-1)*0.72+1]
weo_gdppc_growth[country_code == "IND", gdppc_growth := (gdppc_growth-1)*0.54+1]

#Merge growth rates (consumption and gdppc)
growth_rates <- merge(hhfce_growth, weo_gdppc_growth, all = T, by = c("country_code", "year"))
growth_rates <- growth_rates[, .(growth_rate = ifelse(is.na(hhfce_growth), gdppc_growth, hhfce_growth)), by = .(country_code, year)] #keeps consumption growth except when it is not available and gdppc growth is

growth_rates[is.na(growth_rates)] <- 1 #puts all missing growth rates to 1 (no change)

growth_rates[, growth_index := cumprod(growth_rate), by = country_code] #creates new column with cumulative growth for all previous years

#Establish poverty line years to project
poverty_lines <- pip_most_recent[rep(seq_len(.N), max(as.numeric(as.character(growth_rates$year)))-reporting_year+1)][, year := as.character(reporting_year + seq_len(.N)-1), by = .(country_code, reporting_level, fill)]

#Calculate effective poverty lines
poverty_lines <- merge(poverty_lines, growth_rates, by = c("country_code", "year"), all.x = T)
poverty_lines[, poverty_line := (growth_index[which.min(year)]/growth_index), by = .(country_code, reporting_level)]

#Join effective lines with existing PIP line data (historic + projected)
poverty_lines <- unique(rbind(pip_response[, .(country_code, reporting_level, year = reporting_year, reporting_year, poverty_line = 1, fill)], poverty_lines[, .(country_code, reporting_level, year, reporting_year, poverty_line, fill)])[order(country_code, reporting_level, year)])

fwrite(poverty_lines, "project_data/effective_ipls.csv")
