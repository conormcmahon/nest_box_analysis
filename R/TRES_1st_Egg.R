
# ***********************************
#     Day-of-Year Lookup Function
# ***********************************

# Function to return Day-of-Year given input string in date format
#   Accepts:  M/D/YYYY, MM/D/YYYY, M/DD/YYYY
#   If input is not in one of these formats, returns -1
#   Otherwise, returns the Day-of-Year (e.g. January 1st ->1, December 31 -> 365 (if not Leap year))
DOYFromString <- function(input)
{
  # Discard input strings which don't contain the character '/'
  if (!grepl('/', input))
    return -1
  # Should add some extra safety checks in here but not yet

  # Extract Month (characters BEFORE first '/')
  month <- as.numeric(gsub("/.*$", "", input))
  # Extract Year (characters AFTER second '/')
  year <- as.numeric(gsub(".*/", "", input))
  # Extract Date (characters BETWEEN two '/')
  day <- sub(paste(month,"/",sep=""), "", input)
  day <- as.numeric(gsub(paste("/",year,sep=""), "", day))
  
  # Sum days from previous months
  if(month > 1)
  {
    month_lengths <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    day_of_year <- sum(month_lengths[1:month-1])
  }
  else 
    day_of_year <- 0
  
  # Add this month's date
  day_of_year <- day_of_year + day
  
  # Account for leap years
  if(year %% 4 == 0)
    day_of_year <- day_of_year + 1
  
  return(day_of_year)
}


# ***********************************
#            Process Data
# ***********************************

# Year List
years <- c('2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019')

# Prepare Dataframe
first_egg_data <- data.frame(matrix(ncol=length(years)+1,nrow=0),stringsAsFactors=FALSE)
colnames(first_egg_data) <- c("site",paste('y',years,sep=""))
for (year in years)
  first_egg_data[[paste('y',year,sep="")]] <- as.integer(first_egg_data[[paste('y',year,sep="")]])
first_egg_data[["site"]] <- as.character(first_egg_data[["site"]])

# ***********************************
# Iterate over Years
for (year_ind in 1:length(years))
{
  # Load Input CSV Data
  TRES <- read.csv(paste(paste("C:/Users/conor/Documents/Tree_Swallows/Excel_Data/TRES_",years[year_ind],sep=""),".csv",sep=""))
  # Delete all rows which do NOT have a numeric value in Eggs column:
  TRES_eggs <- transform(TRES[grep("^\\d+$", TRES$Eggs),,drop=F], Eggs= as.numeric(as.character(Eggs)))
  
  # Nest Box List
  nests <- as.vector(unique(TRES_eggs$BOX))
  #   remove empty strings (from blank lines)
  nests <- nests[nests != '']
  
  # ***********************************
  # Iterate over nests to find 1st Eggs
  for (nest_ind in 1:length(nests))
  {
    
    nest_data <- TRES_eggs[TRES_eggs$BOX == nests[nest_ind],]
    dates <- as.vector(TRES_eggs[TRES_eggs$BOX == nests[nest_ind],]$DATE)
    
    # ***********************************
    # Iterate over dates surveyed at this nest
    for(date_ind in 1:(length(dates)))
    {
      if (nest_data[date_ind,]$Eggs > 0)
      {
        # Only save information from TRES eggs for now
        if (nest_data[date_ind,]$Bird.Taxon == 'TRES' || nest_data[date_ind,]$Bird.Taxon == "tres" || nest_data[date_ind,]$Bird.Taxon == "Tres")
        {
          # Find Date -> DOY
          first_egg_doy <- DOYFromString(dates[date_ind])
          
          # Populate Dataframe
          if(nrow(first_egg_data[first_egg_data$site==nests[nest_ind],]) == 0)
            first_egg_data[nrow(first_egg_data)+1,] <- c(nests[nest_ind],0,0,0,0,0,0,0,0)
          first_egg_data[first_egg_data$site==nests[nest_ind],year_ind+1] <- first_egg_doy
        }
        # Continue to next nest
        break
      }
    }
  }

}


# ***********************************
# Fill boxes not used, or not used by TRES, with NA
first_egg_data[first_egg_data==0] <- NA

# Ensure type is numeric to prevent boxplot() from being smangry
for (year in years)
  first_egg_data[[paste('y',year,sep="")]] <- as.integer(first_egg_data[[paste('y',year,sep="")]])



# ***********************************
#              Reporting
# ***********************************

# Generate Boxplot
boxplot(first_egg_data[,2:ncol(first_egg_data)], na.rm=TRUE, ylab="Date of First TRES Egg in Box")
title("First Egg Across Sites, By Year")


# ***********************************
# Print out some basic statistics: 

data_statistics <- data.frame(matrix(ncol=4,nrow=0),stringsAsFactors=FALSE)
colnames(data_statistics) <- c("Mean","Min","Max","Count")

# Mean First Egg Dates, Across Years
for (year_ind in 1:length(years))
  data_statistics[years[year_ind],1] <- mean(first_egg_data[[paste('y',years[year_ind],sep="")]], na.rm=TRUE)

# Min First Egg Dates, Across Years
for (year_ind in 1:length(years))
  data_statistics[years[year_ind],2] <- min(first_egg_data[[paste('y',years[year_ind],sep="")]], na.rm=TRUE)

# Max First Egg Dates, Across Years
for (year_ind in 1:length(years))
  data_statistics[years[year_ind],3] <- max(first_egg_data[[paste('y',years[year_ind],sep="")]], na.rm=TRUE)

# Number of First Eggs, Across Years
for (year_ind in 1:length(years))
  data_statistics[years[year_ind],4] <- sum(!is.na(first_egg_data[[paste('y',years[year_ind],sep="")]]))

print("")
print(data_statistics)