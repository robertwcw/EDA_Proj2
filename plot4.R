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
    fileSCC <- as.data.table(readRDS(fileout1)) 
    filePM25 <- as.data.table(readRDS(fileout2)) 
    unlink(c(fileout1, fileout2)) 
    rm(fileout1, fileout2) 
}
coalSCC <- fileSCC %>%  subset(., Data.Category %in% c("Point","Nonpoint")) %>% 
                        subset(., grepl("[Cc][Oo][Aa][Ll]",Short.Name)) %>% 
                        subset(., grepl("[Cc][Oo][Mm][Bb]",Short.Name)) 
coalPM25 <- filePM25 %>% filter(SCC %in% coalSCC$SCC) %>%
                        group_by(Pollutant, type, year, fips) %>%
                        summarise(Emissions.mean = mean(Emissions))
coalPM25$Pollutant <- as.factor(coalPM25$Pollutant)
coalPM25$type <- factor(coalPM25$type, labels = c("Non-Point","Point"))
coalPM25$year <- factor(coalPM25$year, labels = c(1999,2002,2005,2008))
coalPM25$fips <- as.factor(coalPM25$fips)

# Plotting graph for PM2.5 pollutant from coal combustion related sources  
# (overall across the US)
gr0 <- qplot(jitter(as.integer(year)), Emissions.mean, data = coalPM25, 
            facets = . ~ type,
            log = "y", 
            xlab = "Year", 
            ylab = "PM2.5 Emissions [ Mass @ log(tonnage) ]", 
            main = paste0("Coal Combustion Related PM2.5 Emissions ",
                          "for All States (1999 ~ 2008)") 
            ) + geom_point(shape = 1) +
            geom_smooth(method = "lm") +
            scale_x_discrete(limits = c("1999","2002","2005","2008")) 
suppressWarnings(ggsave("plot4.png", plot = gr0))

# Houese keeping
# detach("package:maps", unload = TRUE)
detach("package:dplyr", unload = TRUE)
detach("package:ggplot2", unload = TRUE)
response <- readline(paste0("Do you want to retain the NEI data sets ",
                            "for subsequent use? (Yes/No): "))
if (!substr(response,1,1) %in% c("Y","y")) 
    {
    rm(fileSCC, filePM25)
    }
rm(getRflib, is.defined, myplclust, .Rfliburl)
rm(coalPM25, coalSCC, gr0, response, datadir)
gc(full = TRUE)
