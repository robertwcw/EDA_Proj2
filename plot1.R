################################################################################
## Project: EDA course project 2
## Script: plot1.R
## Date: 27/08/2020
## Author: 
################################################################################
.Rfliburl <- "https://raw.githubusercontent.com/robertwcw/Rflib/master"
source(file.path(.Rfliburl,"getRflib.R"), local = TRUE)
source(getRflib("is.defined.R"), local = TRUE)
source(getRflib("myplclust.R"), local = TRUE)
################################################################################
## National Emission Inventory data plot for year 1999, 2002, 2005 and 2008.
## Plot the trends between 1999 ~ 2008 for pollutant PM2.5 emissions measured in
## mass tonnage across all states in the USA from all sources (point, non-point,
## road, non-road). Please refer to EPA website for pollutant source definition.
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
filePM25 <- filePM25 %>% filter(fips %in% county.code) %>% 
                          group_by(Pollutant, type, year, fips) %>%
                          summarise(Emissions.mean = mean(Emissions))
filePM25$Pollutant <- factor(filePM25$Pollutant)
filePM25$type <- factor(filePM25$type)
filePM25$year <- factor(filePM25$year, labels = c("1999","2002","2005","2008"))
filePM25$fips <- factor(filePM25$fips, exclude = c("   NA","00000"))

# Preparing color palette based on RGB color space
pal <- c(rgb(0,0,1), rgb(0,1,0), rgb(1,0,0), rgb(1,0,1))

# Plotting graph
png(filename = "plot1.png", width = 600, height = 600, units = "px")

par(ann = FALSE, cex = 1, cex.sub = 0.8, ylog = TRUE)
with(filePM25, plot(Emissions.mean ~ jitter(as.integer(year)), 
                    type = "p",
                    pch = 4, 
                    cex = 0.8, 
                    col = pal[unique(type)], 
                    log = "y",
                    xaxt = "none"
                    # yaxt = "none"
                    )
     )

# abline(lm(Emissions.mean ~ year, data = filePM25),
#        lwd = 3, lty = 1,col = "yellow3")
with(filePM25, lines(lowess(x = as.integer(year), y = Emissions.mean, f = 2/3, iter = 10),
                    lwd = 3, lty = 1, col = "black"
                    )
     )

axis(1, 1:4, labels = c("1999","2002","2005","2008"), tick = TRUE)

legend("bottomleft", legend = unique(filePM25$type),
       pch = 4, 
       cex = 0.8, 
       col = pal[unique(filePM25$type)]
       )

title(main = "PM2.5 Emissions of All States (1999 ~ 2008)", 
      sub = "National Emissions Inventory Data (publish every 3 years)", 
      xlab = "Year", 
      ylab = "PM2.5 Emissions [ Mass @ log(tonnage) ]" 
      )

dev.off()


# Houese keeping
detach("package:maps", unload = TRUE)
detach("package:dplyr", unload = TRUE)
response <- readline(paste0("Do you want to perform garbage collection ",
                            "to free up memory? (Yes/No): "))
if (substr(response,1,1) %in% c("Y","y")) 
    {
      rm(fileSCC, filePM25, emissionPM25)
    }
rm(county.fips, getRflib, is.defined, myplclust, .Rfliburl)
rm(county.code, datadir, response)
gc(full = TRUE)
