################################################################################
## Project: EDA course project 2
## Script: plot3.R
## Date: 31/08/2020
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
library(maps)
library(dplyr)
library(ggplot2)
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
baltimorePM25$type <- factor(baltimorePM25$type,
                             labels = c("Non-Road","Non-Point","On-Road","Point"))
baltimorePM25$year <- factor(baltimorePM25$year, 
                             labels = c(1999,2002,2005,2008))
baltimorePM25$fips <- factor(baltimorePM25$fips)

# Plotting graph 
gr0 <- qplot(x = jitter(as.integer(year)), y = Emissions, data = baltimorePM25,
             color = type,
             log = "y",
             xlab = "Year",
             ylab = "PM2.5 Emissions [ Mass @ log(tonnage) ]"
             ) + geom_point(shape = 8) + 
                  geom_rug() +
                  geom_smooth(method = "lm")
gr0 <- gr0 + labs(title = paste0("PM2.5 Emissions Aggregate in Baltimore City, ",
                               "Maryland (1999 ~ 2008)"), 
                subtitle = "By Type of pollutant source", 
                caption = paste0("Data: National Emissions Inventory ",
                                 "(publish every 3-year)")
                )

gr0 <- gr0 + scale_color_discrete(name = "Source Type")

suppressWarnings(ggsave("plot3.png", plot = gr0))

# Houese keeping
detach("package:maps", unload = TRUE)
detach("package:dplyr", unload = TRUE)
detach("package:ggplot2", unload = TRUE)
response <- readline(paste0("Do you want to perform garbage collection ",
                            "to free up memory? (Yes/No): "))
if (substr(response,1,1) %in% c("Y","y")) 
{
    rm(fileSCC, filePM25)
}
rm(getRflib, is.defined, myplclust, .Rfliburl, county.fips, baltimorePM25)
rm(response, datadir, gr0, county.code)
gc(full = TRUE)
