
# Exploratory Data Analysis Project 2
# 2014-06-20


# The overall goal of this assignment is to explore the National Emissions Inventory 
# database and see what it say about fine particulate matter pollution in the United 
# states over the 10-year period 1999–2008. You may use any R package you want to 
# support your analysis. 


# Load given data objects
NEI <- readRDS("Raw Data/summarySCC_PM25.rds")
SCC <- readRDS("Raw Data/Source_Classification_Code.rds")



# ---------------------------------------------------------------------
# QUESTION 1
# ---------------------------------------------------------------------

# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission from all 
# sources for each of the years 1999, 2002, 2005, and 2008.


# How many pollutants are there?
table(NEI$Pollutant)
# only 1

# Are there missing values?
which(is.na(NEI))
# None

library(reshape)
NEI_melt1 <- melt(NEI, id.vars = "year", measure.vars = "Emissions")
NEI_cast1 <- cast(NEI_melt1, year ~ variable, sum)

png("plot1.png")
barplot(NEI_cast1$Emissions, names.arg = NEI_cast1$year, main = "Total PM2.5 Emissions From \nAll Sources (1999 - 2008) in USA", xlab = "Year", ylab = "PM2.5 Emissions (tons)")
dev.off()


# Yes, emissions are being reduced over time.


# ---------------------------------------------------------------------
# QUESTION 2
# ---------------------------------------------------------------------


# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
# (fips == "24510") from 1999 to 2008? Use the base plotting system to make a 
# plot answering this question.


# Subset only Baltimore data
NEI_balt <- NEI[which(NEI$fips == "24510"), ]


library(reshape)
NEI_melt2 <- melt(NEI_balt, id.vars = "year", measure.vars = "Emissions")
NEI_cast2 <- cast(NEI_melt2, year ~ variable, sum)

png("plot2.png")
barplot(NEI_cast2$Emissions, names.arg = NEI_cast2$year, main = "Total PM2.5 Emissions From All Sources\n (1999 - 2008) in Baltimore, MD", xlab = "Year", ylab = "PM2.5 Emissions (tons)")
dev.off()


# Yes, the overall emissions have been decreased from 199 to 2008 (however, there was an 
# increase in 2005).





# ---------------------------------------------------------------------
# QUESTION 3
# ---------------------------------------------------------------------


# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) 
# variable, which of these four sources have seen decreases in emissions from 1999–2008 
# for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the 
# ggplot2 plotting system to make a plot answer this question.



NEI_melt3 <- melt(NEI_balt, id.vars = c("year", "type"), measure.vars = "Emissions")
NEI_cast3 <- cast(NEI_melt3, year + type ~ variable, sum)


library(ggplot2)
png("plot3.png")
p <- ggplot(NEI_cast3, aes(x = year, y = Emissions, group = type))
p + geom_line(aes(color = factor(type))) + scale_color_discrete(name = "PM2.5 Type") + labs(x = "Year", y = "PM2.5 Emissions (tons)", title = "Total PM2.5 Emissions From Four Sources\n (1999 - 2008) in Baltimore, MD") 
dev.off()


# Only "non-road", "on-road" and "non-point" emissions have seen decreases. Point has not decreased.






# ---------------------------------------------------------------------
# QUESTION 4
# ---------------------------------------------------------------------

# Across the United States, how have emissions from coal combustion-related 
# sources changed from 1999–2008?



# Find all SCC source codes that contain the word "coal" and "comb" in in 
# the EI.Sector column.
comb_coal_SCC <- SCC[grepl("Coal", SCC$EI.Sector) & grepl("Comb", SCC$EI.Sector), ]

# Use those indicies to subset the NEI data frame, keeping only rows 
# that contain Comb Coal related SCC values. 
comb_coal_SCC_df <- NEI[NEI$SCC %in% comb_coal_SCC$SCC, ]

# Aggregate the data
NEI_melt4 <- melt(comb_coal_SCC_df, id.vars = c("year", "type"), measure.vars = "Emissions")
NEI_cast4 <- cast(NEI_melt4, year + type ~ variable, sum)


library(ggplot2)
png("plot4.png")
p <- ggplot(NEI_cast4, aes(x = year, y = Emissions, group = type))
p + geom_line(aes(colour = factor(type))) + scale_color_discrete(name = "PM2.5 Type") + labs(x = "Year", y = "PM2.5 Emissions (tons)", title = "Total PM2.5 Emissions From Coal Combustion\n-related sources (1999 - 2008) in the USA") 
dev.off()

# The amount of "Point" related PM2.5 emissions have dropped, but 
# the amount of ""Non-road" and "road""Non-point" emissions have not significantly dropped. 




# ---------------------------------------------------------------------
# QUESTION 5
# ---------------------------------------------------------------------


# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

# Subset only Baltimore data
NEI_balt <- NEI[NEI$fips == "24510", ]


# Find all SCC source codes that contain the words below in 
# the EI.Sector column.
motorveh_SCC <- SCC[SCC$EI.Sector == "Mobile - On-Road Diesel Heavy Duty Vehicles" |
                        SCC$EI.Sector == "Mobile - On-Road Gasoline Heavy Duty Vehicles" |
                        SCC$EI.Sector == "Mobile - On-Road Diesel Light Duty Vehicles" |
                        SCC$EI.Sector == "Mobile - On-Road Gasoline Light Duty Vehicles", ]




# Use those SSC numbers to subset the NEI data frame, keeping only rows 
# that contain the related SCC values. Subset from the Baltimore, MD data frame (not
# the whole data frame)
motorveh_SCC_balt_df <- NEI_balt[NEI_balt$SCC %in% motorveh_SCC$SCC, ]

# Aggregate the data
NEI_melt5 <- melt(motorveh_SCC_df, id.vars = c("year", "type"), measure.vars = "Emissions")
NEI_cast5 <- cast(NEI_melt5, year + type ~ variable, sum)



library(ggplot2)
png("plot5.png")
p <- ggplot(NEI_cast5, aes(x = year, y = Emissions, group = type))
p + geom_line(aes(colour = factor(type))) + scale_color_discrete(name = "PM2.5 Type") + labs(x = "Year", y = "PM2.5 Emissions (tons)", title = "Total PM2.5 Emissions From Motor Vehicle\n Sources (1999 - 2008) in Baltimore, MD") 
dev.off()



# Yes, the amount of PM2.5 emissions from motor vehicles have dropped over time in Baltimore.



# ---------------------------------------------------------------------
# QUESTION 6
# ---------------------------------------------------------------------



# Compare emissions from motor vehicle sources in Baltimore City with emissions from 
# motor vehicle sources in Los Angeles County, California (fips == "06037"). Which 
# city has seen greater changes over time in motor vehicle emissions?



# Subset only Baltimore (24510) or Los Angles County (06037) data
NEI_balt_la <- NEI[NEI$fips == "24510" | NEI$fips == "06037", ]




# Find all SCC source codes that contain the words below in 
# the EI.Sector column.
motorveh_SCC <- SCC[SCC$EI.Sector == "Mobile - On-Road Diesel Heavy Duty Vehicles" |
                        SCC$EI.Sector == "Mobile - On-Road Gasoline Heavy Duty Vehicles" |
                        SCC$EI.Sector == "Mobile - On-Road Diesel Light Duty Vehicles" |
                        SCC$EI.Sector == "Mobile - On-Road Gasoline Light Duty Vehicles", ]




# Use those SSC numbers to subset the NEI data frames, keeping only rows 
# that contain the related SCC values. Subset from the Baltimore and Los Angles data frames
# (not the whole data frame)
motorveh_SCC_balt_la_df <- NEI_balt_la[NEI_balt_la$SCC %in% motorveh_SCC$SCC, ]


# Aggregate the data
NEI_melt6 <- melt(motorveh_SCC_balt_la_df, id.vars = c("year", "type", "fips"), measure.vars = "Emissions")
NEI_cast6 <- cast(NEI_melt6, year + fips ~ variable, sum)




library(ggplot2)
png("plot6.png")
p <- ggplot(NEI_cast6, aes(x = year, y = Emissions, group = fips))
p + geom_line(aes(colour = factor(fips))) + scale_color_discrete(name = "PM2.5 Type", labels = c("Los Angeles", "Baltimore")) + labs(x = "Year", y = "PM2.5 Emissions (tons)", title = "Total PM2.5 Emissions From Motor Vehicle\n Sources (1999 - 2008) in Baltimore and Los Angeles") 
dev.off()


# The emissions in Los Angeles County have slightly rised from 1999-2008, but 
# are decreasing from their highest value in 2005. The PM2.5 emissions in Los
# Angeles County are about 10 times higher than in Baltimore. The Baltimore 
# emissions are slowly decreasing over time.






