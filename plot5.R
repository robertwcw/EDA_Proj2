################################################################################
## Project: EDA course project 2
## Script: plot5.R
## Date: 1/9/2020
## Author: 
################################################################################
.Rfliburl <- "https://raw.githubusercontent.com/robertwcw/Rflib/master"
source(file.path(.Rfliburl,"getRflib.R"), local = TRUE)
source(getRflib("is.defined.R"), local = TRUE)
source(getRflib("myplclust.R"), local = TRUE)
################################################################################
## National Emission Inventory data plot for year 1999, 2002, 2005 and 2008.
## Plot a graph to show the trend for PM2.5 emissions from motor vehicle sources
## in Baltimore City between 1999 ~ 2008.
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

mvSCC <- fileSCC %>% subset(., Data.Category %in% c("Onroad","Nonroad")) %>% 
                    subset(., grepl("[Mm][Oo][Tt][Oo][Rr]|
                                     [Vv][Ee][Hh][Ii][Cc][Ll][Ee]", Short.Name))

baltimorePM25 <- filePM25 %>% filter(fips == "24510" & SCC %in% mvSCC$SCC) 
baltimorePM25$type <- as.factor(baltimorePM25$type)

plotting 
gr0 <- qplot(x = jitter(year), y = jitter(Emissions), data = baltimorePM25,
             xlab = "Year", 
             ylab = "PM2.5 Emissions", 
             main = paste0("PM2.5 Emissions from Motor Vehicles ",
                           "in Baltimore City, Maryland (1999 ~ 2008)") 
             ) + geom_smooth() 
ggsave("plot5.png", plot = gr0)

# Houese keeping
# detach("package:maps", unload = TRUE)
detach("package:dplyr", unload = TRUE)
detach("package:ggplot2", unload = TRUE)
response <- readline(paste0("Do you want to perform garbage collection ",
                            "to free up memory? (Yes/No): "))
if (substr(response,1,1) %in% c("Y","y")) 
{
    rm(fileSCC, filePM25, baltimorePM25, mvSCC)
}
rm(gr0, response, datadir, getRflib, is.defined, myplclust, .Rfliburl)
gc(full = TRUE)



