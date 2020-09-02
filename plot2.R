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
## Baltimore City, Maryland (fips 24510) between 1999 ~ 2008 for all type of  
## pollutant sources (point, non-point, road, non-road).
################################################################################
# Libraries
library(maps)
library(dplyr)
# Preparing data for EDA
data(county.fips)
# data(state.fips)
county.code <- ifelse(nchar(county.fips[,1]) == 4,
                      paste0("0",as.character(county.fips[,1])),
                      county.fips[,1])
datadir <- paste(".", "data", sep = "/")
if (!dir.exists(datadir)) 
{
    dir.create(datadir)
}
if (!is.defined(fileSCC) | !is.defined(filePM25)) 
{
    if (is.na(list.files(datadir)[1]) | is.na(list.files(datadir)[2])) 
    {
        fileurl <- paste0("https://d396qusza40orc.cloudfront.net/",
                          "exdata%2Fdata%2FNEI_data.zip")
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

baltimorePM25 <- filePM25 %>% filter(fips == "24510") %>% 
                            group_by(Pollutant, type, year) 

baltimorePM25$Pollutant <- factor(baltimorePM25$Pollutant)
baltimorePM25$year <- factor(baltimorePM25$year, 
                             labels = c("1999","2002","2005","2008"))
baltimorePM25$fips <- factor(baltimorePM25$fips)
baltimorePM25$type <- factor(baltimorePM25$type,
                            labels = c("NON-ROAD","NONPOINT","ON-ROAD","POINT"))

# Preparing color palette based on RGB color space
pal <- c(rgb(0,0,1), rgb(0,1,0), rgb(1,0,0), rgb(1,0,1))

# Plotting graph for total emissions in Baltimore, Maryland
png(filename = "plot2.png", width = 720, height = 720, units = "px")

par(ann = FALSE, cex = 1, cex.sub = 0.9) 
with(baltimorePM25, plot(Emissions ~ jitter(as.integer(year)), 
                         type = "p", 
                         pch = 4, 
                         cex = 0.8,
                         col = pal[unique(type)],
                         log = "y",
                         xaxt = "none"
                         )
    )

axis(1, 1:4, labels = c("1999","2002","2005","2008"), tick = TRUE)

# abline(lm(Emissions ~ year, data = baltimorePM25),
#        lwd = 3, lty = 1,col = "yellow3")
with(baltimorePM25, lines(lowess(x = as.integer(year),
                                 y = Emissions,
                                 f = 2/3,
                                 iter = 10
                                 ),
                          lwd = 3, lty = 1, col = "black"
                          )
     )

legend("bottomleft", pch = 4, col = pal[unique(baltimorePM25$type)],
       legend = unique(baltimorePM25$type),
       cex = 0.8
       )

title(main = paste0("PM2.5 Emissions Aggregate of Baltimore City, ",
                    "Maryland (1999 ~ 2008)"), 
      sub = "National Emissions Inventory Data (publish every 3 years)", 
      xlab = "Year", 
      ylab = "PM2.5 Emissions [ Mass @ log(tonnage) ]"
      )

dev.off()

# Houese keeping
detach("package:maps", unload = TRUE)
detach("package:dplyr", unload = TRUE)
response <- readline("Do you want to perform garbage collection to free up memory? (Yes/No): ")
if (substr(response,1,1) %in% c("Y","y")) 
{
    rm(fileSCC, filePM25)
}
rm(response, datadir, pal, baltimorePM25, county.code)
rm(getRflib, is.defined, myplclust, .Rfliburl, county.fips)
gc(full = TRUE)
