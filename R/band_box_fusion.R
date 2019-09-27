
# Load banding data
banding <- read.csv("C:/Users/conor/Documents/Tree_Swallows/Excel_Data/banding.csv")
banding$Full.Code <- paste(banding$Alum_prefix,banding$Alum_suffix,sep="-")

adult_bands <- banding[banding$Age != "L",]

# Load nest data
nests <- read.csv("C:/Users/conor/Documents/Tree_Swallows/Excel_Data/nests.csv")

# adult_band_list <- unique(adult_bands$Full.Code)
# num_adults <- length(adult_band_list)
# adult_records <- data.frame(adult_band_list, rep(0,num_adults), rep(0,num_adults), rep(0,num_adults), rep(0,num_adults), rep(0,num_adults), rep(0,num_adults), rep(0,num_adults), rep(0,num_adults), rep(0,num_adults))
# names(adult_records) <- c("band","num_records", "2017a", "2017b", "2018a", "2018b", "2019a", "2019b")
# ind = 1
# for (band in adult_band_list)
# {
#   adult_records[ind,2] = nrow(adult_bands[adult_bands$Full.Code==band,])
#   adult_records[ind,"2017a"] = paste(as.character(adult_bands[adult_bands$Full.Code==band & adult_bands$Year=="2017",]$Nest.Box)[1],"",sep="")
#   adult_records[ind,"2017b"] = paste(as.character(adult_bands[adult_bands$Full.Code==band & adult_bands$Year=="2017",]$Nest.Box)[2],"",sep="")
#   adult_records[ind,"2018a"] = paste(as.character(adult_bands[adult_bands$Full.Code==band & adult_bands$Year=="2018",]$Nest.Box)[1],"",sep="")
#   adult_records[ind,"2018b"] = paste(as.character(adult_bands[adult_bands$Full.Code==band & adult_bands$Year=="2018",]$Nest.Box)[2],"",sep="")
#   adult_records[ind,"2019a"] = paste(as.character(adult_bands[adult_bands$Full.Code==band & adult_bands$Year=="2019",]$Nest.Box)[1],"",sep="")
#   adult_records[ind,"2019b"] = paste(as.character(adult_bands[adult_bands$Full.Code==band & adult_bands$Year=="2019",]$Nest.Box)[2],"",sep="")
#   
#   ind = ind+1
# }

adult_band_list <- unique(adult_bands$Full.Code)
num_adults <- length(adult_band_list)
ind = 1
adult_records_dataframe <- list()
adult_records <- data.frame(matrix(nrow=num_adults, ncol=5))
colnames(adult_records) <- c("Full.Code", "Species", "Sex", "Weight_Avg", "Num_Years")
for(band in adult_band_list)
{
  adult_records[ind,"Full.Code"] <- band
  adult_records[ind,"Species"] <- as.character((adult_bands[adult_bands$Full.Code==band,]$Species)[1])
  adult_records[ind,"Sex"] <- as.character((adult_bands[adult_bands$Full.Code==band,]$Sex)[1])
  num_observations <- nrow(adult_bands[adult_bands$Full.Code==band,])
  adult_records_dataframe[[ind]] <- data.frame(matrix(ncol=3, nrow=num_observations))
  colnames(adult_records_dataframe[[ind]]) <- c("Nest.Box","Date","Weight")
  adult_records_dataframe[[ind]]$Nest.Box <- as.character(adult_bands[adult_bands$Full.Code==band,]$Nest.Box)
  date <- paste(paste(as.character(adult_bands[adult_bands$Full.Code==band,]$Year), as.character(adult_bands[adult_bands$Full.Code==band,]$Month), sep="-"), as.character(adult_bands[adult_bands$Full.Code==band,]$Day), sep="-")
  adult_records_dataframe[[ind]]$Date <- date
  new_weights_vector <- as.character(adult_bands[adult_bands$Full.Code==band,]$Weight..g.)
  new_weights_vector[new_weights_vector==" -"] <- NA
  adult_records_dataframe[[ind]]$Weight <- new_weights_vector
  adult_records[ind,"Weight_Avg"] <- mean(as.numeric(adult_records_dataframe[[ind]]$Weight), na.rm=TRUE)
  print(adult_records_dataframe[[ind]]$Weight)
  adult_records[ind,"Num_Years"] <- length(unique(as.character(adult_bands[adult_bands$Full.Code==band,]$Year)))
  ind = ind+1
}

adult_records_tres <- adult_records[adult_records$Species=="TRES",]
adult_records_tres <- adult_records_tres[adult_records_tres$Sex == "F" | adult_records_tres$Sex == "M",]
sex_weight <- lm(Weight_Avg ~ Sex, adult_records_tres)
print(summary(sex_weight))
boxplot(Weight_Avg ~ Sex, adult_records_tres)



# Extract Weight vs. Fecundity 



nest_outcomes = data.frame(Nest=character(), 
                           Year=numeric(),
                           Fledged=numeric(),
                           Bad_Egg=numeric(),
                           Dead=numeric(),
                           Unknown=numeric())
new_nest = TRUE
for(row in 1:nrow(nests))
{
  if(new_nest)
  {
    current_nest <- as.character(nests$BOX..[[row]])
    new_nest = FALSE
    print(paste("new nest: ",current_nest,sep=""))
  }
    
  if(!is.na(nests$Fledged[[row]]) && nests$Fledged[[row]] != "")
  {
    temp_data <- data.frame(current_nest, nests[row,2], nests[row,13:16])
    names(temp_data) <- names(nest_outcomes)
    nest_outcomes <- rbind(nest_outcomes, temp_data)
    #nest_outcomes[nrow(nest_outcomes)+1,1] <- current_nest
    #nest_outcomes[nrow(nest_outcomes),2:5] <- nests[row,14:17]
    new_nest = TRUE
    print(paste("new obsv: ",nests[row,14:17],sep=""))
  }
}

# Number Fledged
print(paste("Number of total fledged young: ",sum(as.numeric(nest_outcomes[!is.na(nest_outcomes$Fledged),]$Fledged)),sep=""))
# Number Dead
print(paste("Number of total dead young: ",sum(as.numeric(nest_outcomes[!is.na(nest_outcomes$Dead),]$Dead)),sep=""))
# Fledge Efficiency
print(paste("Fledged divided by dead: ",sum(as.numeric(nest_outcomes[!is.na(nest_outcomes$Fledged),]$Fledged))/sum(as.numeric(nest_outcomes[!is.na(nest_outcomes$Dead),]$Dead)),sep=""))





