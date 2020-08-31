################################################################################
## Project: EDA course project 2
## Script: plot3.R
## Date: 30/08/2020
## Author: 
################################################################################
.Rfliburl <- "https://raw.githubusercontent.com/robertwcw/Rflib/master"
source(file.path(.Rfliburl,"getRflib.R"), local = TRUE)
source(getRflib("is.defined.R"), local = TRUE)
source(getRflib("myplclust.R"), local = TRUE)
################################################################################
## National Emission Inventory data plot for year 1999, 2002, 2005 and 2008.
## Plot a graph to show the trend for the 4-type of pollutant source (point, 
## non-point, road, non-road) of PM2.5 emissions in the Baltimore City, Maryland
## (fips 24510) between 1999 ~ 2008.
################################################################################
# Libraries
# library(maps)
library(dplyr)
library(ggplot2)
# Preparing data for EDA
# data(county.fips)
# data(state.fips)
# county.code <- ifelse(nchar(county.fips[,1]) == 4,
#                       paste0("0",as.character(county.fips[,1])),
#                       county.fips[,1])
datadir <- paste(".", "data", sep = "/")
if (!dir.exists(datadir)) 
{
    dir.create(datadir)
}
if (!is.defined(fileSCC) | !is.defined(filePM25)) 
{
    if (is.na(list.files(datadir)[1]) | is.na(list.files(datadir)[2])) 
    {
        fileurl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
        filetmp <- tempfile() 
        download.file(url = fileurl, destfile = filetmp) 
        unzip(filetmp, overwrite = TRUE, exdir = datadir) 
        unlink(filetmp) 
        rm(filetmp, fileurl) 
    }
    fileout1 <- paste(datadir, list.files(datadir)[1], sep = "/") 
    fileout2 <- paste(datadir, list.files(datadir)[2], sep = "/") 
    fileSCC <- readRDS(fileout1) 
    filePM25 <- readRDS(fileout2) 
    unlink(c(fileout1, fileout2)) 
    rm(fileout1, fileout2) 
}
baltimorePM25 <- filePM25 %>% 
      filter(fips == "24510") %>% 
      group_by(Pollutant, type, year) %>%
      summarise(Emission.total = sum(Emissions)) 

# Plotting graph 
png(filename = "plot3.png", width = 600, height = 600, units = "px") 
qplot(x = year, y = log10(Emission.total), data = baltimorePM25,  
      color = type, 
      geom = c("point", "line"), 
      main = "PM2.5 Emissions by Type of Baltimore City, Maryland (1999 ~ 2008)", 
      xlab = "Year", 
      ylab = "PM2.5 Emissions [ Mass @ log10(tonnage) ]" 
      )
dev.off()

# Houese keeping
# detach("package:maps", unload = TRUE)
detach("package:dplyr", unload = TRUE)
detach("package:ggplot2", unload = TRUE)
response <- readline("Do you want to perform garbage collection to free up memory? (Yes/No): ")
if (substr(response,1,1) %in% c("Y","y")) 
{
    rm(fileSCC, filePM25, baltimorePM25)
}
rm(response, datadir, pal, getRflib, is.defined, myplclust, .Rfliburl)
gc(full = TRUE)
