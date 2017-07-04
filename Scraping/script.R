# Most surprising result
library(gtools)

url <- "http://www.tennis-data.co.uk/year/ausopen.csv"
years <- sapply(2015:2017, function(x) sub("year", x, url))
data <- do.call("rbind", lapply(years, read.csv))

# Check classes
table(apply(data, 2, class))

for(name in names(data)){
	print(name)
    print(sort(unique(data[,name])))
    ask()
}

library(lubridate)

# Create date
data <- data %>%
	dplyr::mutate(
		Date  = dmy(Date),
		Year = year(Date)
	)
	
# Check missing values
data %>%
	select(B365W:AvgL) %>%
	summarise_all(
		funs(sum(is.na(.)))
	)
	
	
# Create prob
data <- data %>% 
	dplyr::mutate(
		WinScore = 1 - 1 / as.numeric( B365W)
	)	
	
	
# Fix player names
players <- sort(unique(as.character(data$Winner))) 
approx <- lapply(players, agrep, fixed = T, x = players) 
# Compare each player against all others
players[sapply(approx, length) > 1]	

data$Winner[data$Winner == "Bautista Agut R."] <- "Bautista R."

summarise_wins <- data %>%
	filter(Comment != "Retired") %>%
	group_by(Year, Winner) %>%
	dplyr::summarise(
		TotalWinScore = sum(WinScore)
	)

summarise_wins[order(summarise_wins$TotalWinScore, decreasing = T),][1:10,]