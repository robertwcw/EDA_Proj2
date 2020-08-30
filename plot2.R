################################################################################
## Project: EDA course project 2
## Script: plot2.R
## Date: 30/08/2020
## Author: 
################################################################################
.Rfliburl <- "https://raw.githubusercontent.com/robertwcw/Rflib/master"
source(file.path(.Rfliburl,"getRflib.R"), local = TRUE)
source(getRflib("is.defined.R"), local = TRUE)
source(getRflib("myplclust.R"), local = TRUE)
################################################################################
## National Emission Inventory data plot for year 1999, 2002, 2005 and 2008.
## Plot a graph to show the trend for total pollutant PM2.5 emissions in the
## Baltimore City, Maryland (fips 24510) between 1999 ~ 2008 for all pollutant 
## sources (point, non-point, road, non-road).
################################################################################
# Libraries
# library(maps)
library(dplyr)
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
                    # group_by(Pollutant, type, year) %>% 
                    group_by(Pollutant, year) %>% 
                    summarise(Emission.total = sum(Emissions)) 

# Preparing color palette based on RGB color space
pal <- c(rgb(0,0,1), rgb(0,1,0), rgb(1,0,0), rgb(1,0,1))

# Plotting graph
png(filename = "plot2.png", width = 600, height = 600, units = "px")
par(ann = FALSE, cex = 1, cex.sub = 1.1, ylog = TRUE) 
with(baltimorePM25, 
     plot(year, log10(Emission.total), type = "o", 
          # pch = 19, col = pal[as.factor(type)]), log = "y" 
          pch = 19, col = pal[3], log = "y"
         ) 
    ) 
# legend("topright", pch = 19, col = pal[as.factor(unique(baltimorePM25$type))], 
#        legend = as.factor(unique(baltimorePM25$type))
#     )
# with(subset(baltimorePM25, type == unique(type)[1]), 
#      lines(year, log10(Emission.total), lwd = 2, lty = 3, col = pal[1])
#     )
# with(subset(baltimorePM25, type == unique(type)[2]), 
#      lines(year, log10(Emission.total), lwd = 2, lty = 3, col = pal[2])
#     )
# with(subset(baltimorePM25, type == unique(type)[3]), 
#      lines(year, log10(Emission.total), lwd = 2, lty = 3, col = pal[3])
#     )
# with(subset(baltimorePM25, type == unique(type)[4]), 
#      lines(year, log10(Emission.total), lwd = 2, lty = 3, col = pal[4])
#     )
title(main = "Total PM2.5 Emissions of Baltimore City, Maryland (1999 ~ 2008)", 
      xlab = "Year", 
      ylab = "Mass of PM2.5 Emissions [ tonnage @ log10 ]" 
    )
dev.off()

# Houese keeping
# detach("package:maps", unload = TRUE)
detach("package:dplyr", unload = TRUE)
response <- readline("Do you want to perform garbage collection to free up memory? (Yes/No): ")
if (substr(response,1,1) %in% c("Y","y")) 
{
    rm(fileSCC, filePM25, baltimorePM25)
}
rm(response, datadir, pal, getRflib, is.defined, myplclust, .Rfliburl)
gc(full = TRUE)
