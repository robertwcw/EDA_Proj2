################################################################################
## Project: EDA course project 2
## Script: plot4.R
## Date: 31/08/2020
## Author: 
################################################################################
.Rfliburl <- "https://raw.githubusercontent.com/robertwcw/Rflib/master"
source(file.path(.Rfliburl,"getRflib.R"), local = TRUE)
source(getRflib("is.defined.R"), local = TRUE)
source(getRflib("myplclust.R"), local = TRUE)
################################################################################
## National Emission Inventory data plot for year 1999, 2002, 2005 and 2008.
## Plot a graph to show the trend for PM2.5 emissions from coal combustion-
## related sources across United States between 1999 ~ 2008.
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
coalSCC <- fileSCC %>%  subset(., Data.Category %in% c("Point","Nonpoint")) %>% 
                        subset(., grepl("[Cc][Oo][Aa][Ll]",Short.Name)) %>% 
                        subset(., grepl("[Cc][Oo][Mm][Bb]",Short.Name)) 
coalPM25 <- filePM25 %>% filter(SCC %in% coalSCC$SCC) 
                        # group_by(Pollutant, type) %>% 
                        # summarise(Emissions.mean = mean(Emissions)) 
coalPM25$type <- as.factor(coalPM25$type)
coalPM25$Pollutant <- as.factor(coalPM25$Pollutant)

# Plotting graph for PM2.5 pollutant from coal combustion related sources  
# (overall across the US)
gr0 <- qplot(year, Emissions, data = coalPM25, 
            facets = . ~ type,
            geom = c("point"), 
            log = "y", 
            # color = type,
            xlab = "Year", 
            ylab = "PM2.5 Emissions [ Mass @ log(tonnage) ]", 
            main = paste0("Coal Combustion Related PM2.5 Emissions ",
                          "for All States (1999 ~ 2008)") 
            ) + 
            geom_smooth(method = "lm") 
            # scale_color_discrete(name = "Source Type")
suppressWarnings(ggsave("plot4.png", plot = gr0))

# Houese keeping
# detach("package:maps", unload = TRUE)
detach("package:dplyr", unload = TRUE)
detach("package:ggplot2", unload = TRUE)
response <- readline(paste0("Do you want to perform garbage collection ",
                            "to free up memory? (Yes/No): "))
if (substr(response,1,1) %in% c("Y","y")) 
{
    rm(fileSCC, filePM25)
}
rm(getRflib, is.defined, myplclust, .Rfliburl)
rm(coalPM25, coalSCC, gr0, response, datadir)
gc(full = TRUE)
