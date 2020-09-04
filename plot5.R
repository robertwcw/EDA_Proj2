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
    fileSCC <- as.data.table(readRDS(fileout1)) 
    filePM25 <- as.data.table(readRDS(fileout2)) 
    unlink(c(fileout1, fileout2)) 
    rm(fileout1, fileout2) 
}

mvSCC <- fileSCC %>% 
    subset(., Data.Category %in% c("Onroad","Nonroad")) %>% 
    subset(., grepl("[Mm][Oo][Tt][Oo][Rr]|[Vv][Ee][Hh][Ii][Cc][Ll][Ee]", 
                    Short.Name))

baltimorePM25 <- filePM25 %>% 
    filter(fips == "24510" & SCC %in% mvSCC$SCC) %>%
    group_by(Pollutant, type, year, fips)

baltimorePM25$Pollutant <- as.factor(baltimorePM25$Pollutant)
baltimorePM25$type <- as.factor(baltimorePM25$type)
baltimorePM25$year <- as.factor(baltimorePM25$year)
baltimorePM25$fips <- as.factor(baltimorePM25$fips)

# plotting graph showing Motor vehicles pm2.5 emissions in Baltimore city
gr0 <- qplot(x = jitter(as.integer(year)), y = Emissions, data = baltimorePM25,
             log = "y",
             color = type,
             xlab = "Year", 
             ylab = "PM2.5 Emissions [ Mass @ log(tonage) ]", 
             main = paste0("Motor Vehicles Emissions in ",
                           "Baltimore City, Maryland (1999 ~ 2008)")
             ) + geom_point(shape = 1) +
                 geom_smooth(method = "lm") +
                 scale_color_discrete(name = "Source Type") +
                 scale_x_discrete(limits = c("1999","2002","2005","2008")) 
ggsave("plot5.png", plot = gr0)

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
rm(getRflib, is.defined, myplclust, .Rfliburl, mvSCC, baltimorePM25)
rm(gr0, response, datadir)
gc(full = TRUE)



