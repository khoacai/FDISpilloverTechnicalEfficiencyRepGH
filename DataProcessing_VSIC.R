###############
#This is the main R file for gathering, merging, filtering, and compiling data 
#from various sources, mainly from GSSO.
#This file measures and calculates the variables for regression models and 
#further statistical analyses in Stata files. 
#
#Copyright @2020 by Khoa Cai. All rights reserved.
###############


#Path to R code file
rootDirPath = "D:/KHOA/FDISpillover/"

setwd(rootDirPath)

libpath = "D:/KHOA/FDISpillover/RCode/"

#Other offset paths (almost no need to change)
path =  "./01_Fed/01_CB/01_Annual/fedCBAFunctions/"
invisible(lapply(paste0(libpath, path, list.files(path=paste0(libpath, path), pattern=".R", recursive=TRUE)), source))
path =  "./00_Common/"
invisible(lapply(paste0(libpath, path, list.files(path=paste0(libpath, path), pattern=".R", recursive=TRUE)), source))
path =  "./01_Fed/00_Common/"
invisible(lapply(paste0(libpath, path, list.files(path=paste0(libpath, path), pattern=".R", recursive=TRUE)), source))
path =  "./01_Fed/01_CB/00_Common/"
invisible(lapply(paste0(libpath, path, list.files(path=paste0(libpath, path), pattern=".R", recursive=TRUE)), source))


################
#FDISpillover
library(readxl)

ExImpVSICTotal2010 <- read_excel("Xuat nhap khau 2010-2011-2012-2013_2.xlsx", 
                                 sheet = "VSICTotal2010")

ExImpVSICTotal2011 <- read_excel("Xuat nhap khau 2010-2011-2012-2013_2.xlsx", 
                                 sheet = "VSICTotal2011")

ExImpVSICTotal2012 <- read_excel("Xuat nhap khau 2010-2011-2012-2013_2.xlsx", 
                                 sheet = "VSICTotal2012")

ExImpVSICTotal2013 <- read_excel("Xuat nhap khau 2010-2011-2012-2013_2.xlsx", 
                                 sheet = "VSICTotal2013")

ExImpVSICFDI2010 <- read_excel("Xuat nhap khau 2010-2011-2012-2013_2.xlsx", 
                               sheet = "VSICFDI2010")

ExImpVSICFDI2011 <- read_excel("Xuat nhap khau 2010-2011-2012-2013_2.xlsx", 
                               sheet = "VSICFDI2011")

ExImpVSICFDI2012 <- read_excel("Xuat nhap khau 2010-2011-2012-2013_2.xlsx", 
                               sheet = "VSICFDI2012")

ExImpVSICFDI2013 <- read_excel("Xuat nhap khau 2010-2011-2012-2013_2.xlsx", 
                               sheet = "VSICFDI2013")


require(dplyr)
ExImpVSIC2010 = inner_join(ExImpVSICTotal2010, ExImpVSICFDI2010)
ExImpVSIC2010$year = 2010
ExImpVSIC2011 = inner_join(ExImpVSICTotal2011, ExImpVSICFDI2011)
ExImpVSIC2011$year = 2011
ExImpVSIC2012 = inner_join(ExImpVSICTotal2012, ExImpVSICFDI2012)
ExImpVSIC2012$year = 2012
ExImpVSIC2013 = inner_join(ExImpVSICTotal2013, ExImpVSICFDI2013)
ExImpVSIC2013$year = 2013
ExImpVSIC1013 = rbind(ExImpVSIC2010,
                      ExImpVSIC2011,
                      ExImpVSIC2012,
                      ExImpVSIC2013)

library(readr)

VSIC_ManufactoringIndustryCode <- read_csv("VSIC_ManufactoringIndustryCode.csv")

ExImpVSIC1013 = inner_join(ExImpVSIC1013, VSIC_ManufactoringIndustryCode)

ExImpVSIC1013 = ExImpVSIC1013[, c("vsic","io","year","exptotal","imptotal","expfdi","impfdi")]


VSIC_total_1416L_noNAME <- read_csv("VSIC_total_1416L_noNAME.csv")

VSIC_FDI_1416L_noNAME <- read_csv("VSIC_FDI_1416L_noNAME.csv")

ExImpVSIC1416 = inner_join(VSIC_total_1416L_noNAME, VSIC_FDI_1416L_noNAME)

ExImpVSIC1016 = rbind(ExImpVSIC1013, ExImpVSIC1416)


VSICFDISpillover2010 <- read_excel("VSIC FDI_2010_2016 (24-10-2018) OK2.xlsx", 
                                   sheet = "2010 VSIC", col_types = c("numeric", 
                                                                      "numeric", "numeric", "numeric", 
                                                                      "numeric", "numeric", "numeric", 
                                                                      "numeric", "numeric", "numeric"))
VSICFDISpillover2010$year = 2010
VSICFDISpillover2011 <- read_excel("VSIC FDI_2010_2016 (24-10-2018) OK2.xlsx", 
                                   sheet = "2011 VSIC", col_types = c("numeric", 
                                                                      "numeric", "numeric", "numeric", 
                                                                      "numeric", "numeric", "numeric", 
                                                                      "numeric", "numeric", "numeric"))
VSICFDISpillover2011$year = 2011
VSICFDISpillover2012 <- read_excel("VSIC FDI_2010_2016 (24-10-2018) OK2.xlsx", 
                                   sheet = "2012 VSIC", col_types = c("numeric", 
                                                                      "numeric", "numeric", "numeric", 
                                                                      "numeric", "numeric", "numeric", 
                                                                      "numeric", "numeric", "numeric"))
VSICFDISpillover2012$year = 2012
VSICFDISpillover2013 <- read_excel("VSIC FDI_2010_2016 (24-10-2018) OK2.xlsx", 
                                   sheet = "2013 VSIC", col_types = c("numeric", 
                                                                      "numeric", "numeric", "numeric", 
                                                                      "numeric", "numeric", "numeric", 
                                                                      "numeric", "numeric", "numeric"))
VSICFDISpillover2013$year = 2013
VSICFDISpillover2014 <- read_excel("VSIC FDI_2010_2016 (24-10-2018) OK2.xlsx", 
                                   sheet = "2014 VSIC", col_types = c("numeric", 
                                                                      "numeric", "numeric", "numeric", 
                                                                      "numeric", "numeric", "numeric", 
                                                                      "numeric", "numeric", "numeric"))
VSICFDISpillover2014$year = 2014
VSICFDISpillover2015 <- read_excel("VSIC FDI_2010_2016 (24-10-2018) OK2.xlsx", 
                                   sheet = "2015 VSIC", col_types = c("numeric", 
                                                                      "numeric", "numeric", "numeric", 
                                                                      "numeric", "numeric", "numeric", 
                                                                      "numeric", "numeric", "numeric"))
VSICFDISpillover2015$year = 2015
VSICFDISpillover2016 <- read_excel("VSIC FDI_2010_2016 (24-10-2018) OK2.xlsx", 
                                   sheet = "2016 VSIC", col_types = c("numeric", 
                                                                      "numeric", "numeric", "numeric", 
                                                                      "numeric", "numeric", "numeric", 
                                                                      "numeric", "numeric", "numeric"))
VSICFDISpillover2016$year = 2016

VSICFDISpillover1016 = rbind(VSICFDISpillover2010,
                             VSICFDISpillover2011,
                             VSICFDISpillover2012,
                             VSICFDISpillover2013,
                             VSICFDISpillover2014,
                             VSICFDISpillover2015,
                             VSICFDISpillover2016)

VSICFDISpillover1016 = inner_join(ExImpVSIC1016, VSICFDISpillover1016)

attach(VSICFDISpillover1016)

VSICFDISpillover1016$h = fdirev/VSICFDISpillover1016$rev
VSICFDISpillover1016$BLK = BL*fdirev/VSICFDISpillover1016$rev
VSICFDISpillover1016$FLK = FL*(fdirev - expfdi)/(VSICFDISpillover1016$rev - exptotal)

detach(VSICFDISpillover1016)

########################


#######################
#MAIN

library(readxl)
gdpdeflator <- read_excel("Thong tin xu ly them (29-08-2018).xlsx", 
                          sheet = "DeflatorT")

library(readr)

provinces_ecozones_map <- read.csv("provinces_ecozones_map.csv", stringsAsFactors=FALSE)

VSICFDISpillover1016_MfIndustry <- read_csv("VSICFDISpillover1016_MfIndustry.csv")

VSICMfIndustryFirms1016 <- read.csv("VSICMfIndustryFirms1016FULL.csv", stringsAsFactors=FALSE)
VSICMfIndustryFirms1016$ecozone = NULL
VSICMfIndustryFirms1016 = VSICMfIndustryFirms1016[!(is.na(VSICMfIndustryFirms1016$province)),]
VSICMfIndustryFirms1016$year = VSICMfIndustryFirms1016$year + 2000

require(dplyr)

VSICMfIndustryFirms1016 = left_join(VSICMfIndustryFirms1016, provinces_ecozones_map)

VSICMfIndustryFirms1016 = inner_join(VSICMfIndustryFirms1016, gdpdeflator)

VSICMfIndustryFirms1016 = inner_join(VSICMfIndustryFirms1016, VSICFDISpillover1016_MfIndustry)

detach("package:dplyr")

require(data.table)

setnames(VSICMfIndustryFirms1016, "fxassetsend", "fxassets")
setnames(VSICMfIndustryFirms1016, "employeesend", "employees")
setnames(VSICMfIndustryFirms1016, "totalassetsend", "totalassets")
setnames(VSICMfIndustryFirms1016, "totalequityend", "totalequity")
setnames(VSICMfIndustryFirms1016, "giavon_", "cogs")
setnames(VSICMfIndustryFirms1016, "tonkhocuoi_", "inventory")
setnames(VSICMfIndustryFirms1016, "khoanthunhcuoi_", "receivable")
setnames(VSICMfIndustryFirms1016, "ldnucuoi_", "femaleemp")

detach("package:data.table")

deflatedVariables = c("totalrev", "totalcost", 
                      "totalwage", "wage1", "wage2", 
                      "fxassets", "material", "totalassets", "totalequity", 
                      "cogs", "inventory", "receivable", 
                      "exp", "expfdi", "exptotal", 
                      "imp", "impfdi", "imptotal")

VSICMfIndustryFirms1016 = cbind(VSICMfIndustryFirms1016, 
                                setNames(VSICMfIndustryFirms1016[deflatedVariables], 
                                         paste0("dftmp", deflatedVariables)))

require(dplyr)


deflate <- function(x, gdpdeflatorx) 
{
  return (x/gdpdeflatorx)
}

# deflate
VSICMfIndustryFirms1016 = VSICMfIndustryFirms1016 %>% mutate_at(vars(starts_with("dftmp")), 
                                                                funs(deflate, .args = list(.$gdpdeflator)))

detach("package:dplyr")

require(data.table)

setnames(VSICMfIndustryFirms1016, 
         paste0("dftmp", deflatedVariables), 
         paste0("df", deflatedVariables))


## This function is initially copied from code by Mai Feng at: 
## https://gist.github.com/maifeng/3e5a1f0dd65bf0fd9c2b8f4ac8f5fab3
# --------------------------------------------------------
# Author: Khoa Cai
# based on code by Mai Feng 
# --------------------------------------------------------
# Data cleaning by winsorization
# Winsorization: Replace the extreme observations using 99% and 1% percentiles

winsoriz <- function(x, cut=0.01)
{
  cut_point_top <- quantile(x, 1 - cut, na.rm=T)
  cut_point_bottom <- quantile(x, cut, na.rm=T)
  
  i = which(x >= cut_point_top) 
  x[i] = cut_point_top
  
  j = which(x <= cut_point_bottom) 
  x[j] = cut_point_bottom
  
  return (x)
}

VSICMfIndustryFirms1016$equityratio = VSICMfIndustryFirms1016$totalequity/VSICMfIndustryFirms1016$totalassets

winsorizedVariables = c(paste0("df", deflatedVariables), 
                        "blk", "flk", "h",  
                        "employees", "equityratio")


VSICMfIndustryFirms1016 = cbind(VSICMfIndustryFirms1016, 
                                setNames(VSICMfIndustryFirms1016[winsorizedVariables], 
                                         paste0("wstmp", winsorizedVariables)))

require(dplyr)

#winsorize
VSICMfIndustryFirms1016 = VSICMfIndustryFirms1016 %>% mutate_at(vars(starts_with("wstmp")), funs(winsoriz))

detach("package:dplyr")

require(data.table)

setnames(VSICMfIndustryFirms1016, 
         paste0("wstmp", winsorizedVariables), 
         paste0("ws", winsorizedVariables))


detach("package:data.table")

exportCSV(VSICMfIndustryFirms1016, file = "VSICMfIndustryFirms1016FULLDW.csv")

############



####NON-PARAMETERS STOCHASTIC FRONTIER

VSICMfIndustryFirms1016 = read.csv("VSICMfIndustryFirms1016FULLDW.csv", stringsAsFactors=FALSE)

library(npsf)

###RADIAL

VSICMfIndustryFirms1016 = VSICMfIndustryFirms1016[!(is.na(VSICMfIndustryFirms1016$wsdftotalrev) 
                                                    | is.na(VSICMfIndustryFirms1016$wsdffxassets) 
                                                    | is.na(VSICMfIndustryFirms1016$wsemployees) 
                                                    | is.na(VSICMfIndustryFirms1016$wsdfmaterial)
                                                    | VSICMfIndustryFirms1016$wsdftotalrev <= 0 
                                                    | VSICMfIndustryFirms1016$wsdffxassets <= 0 
                                                    | VSICMfIndustryFirms1016$wsemployees<=0 
                                                    | VSICMfIndustryFirms1016$wsdfmaterial <=0),]

VSICMfIndustryFirms1016$Trend = VSICMfIndustryFirms1016$year - 2009

require(dplyr)

VSICMfIndustryFirms1016 = VSICMfIndustryFirms1016 %>% 
  group_by(firmcd, grp = cumsum(c(1, diff(year) != 1))) %>% filter(n() >= 5)

VSICMfIndustryFirms1016 = as.data.frame(VSICMfIndustryFirms1016)

detach("package:dplyr")


##RADIAL-CRS
# Start the timer
ptm = proc.time()

te_r_crs = teradial(wsdftotalrev ~ wsdffxassets + wsemployees + wsdfmaterial + Trend, 
                    VSICMfIndustryFirms1016,
                    rts = c("C"),
                    base = c("output"),
                    ref = NULL, data.ref = NULL, subset.ref = NULL,
                    print.level = 2)

save(te_r_crs, file = "te_r_crsDR5.RData")
exportCSV(as.data.frame(te_r_crs[["te"]]), file = "te_r_crsDR5.csv")

te_r_crs = data.frame(te_r_crs = te_r_crs[["te"]])
VSICMfIndustryFirms1016 =  cbind(VSICMfIndustryFirms1016, te_r_crs)

ols = lm(te_r_crs ~ blk + flk, data = VSICMfIndustryFirms1016)
summary(ols)

library(plm)
VSICMfIndustryFirms1016 = VSICMfIndustryFirms1016[!duplicated(VSICMfIndustryFirms1016[c("firmcd", "year")]),]
preg = plm(te_r_crs ~ blk + flk, data = VSICMfIndustryFirms1016, index = c("firmcd","year"))
summary(preg)

# Stop the timer
proc.time() - ptm 

##RADIAL-NIRS
# Start the timer
ptm = proc.time()

te_r_nirs = teradial(wsdftotalrev ~ wsdffxassets + wsemployees + wsdfmaterial + Trend, 
                     VSICMfIndustryFirms1016,
                     rts = c("NI"),
                     base = c("output"),
                     ref = NULL, data.ref = NULL, subset.ref = NULL,
                     print.level = 2)

save(te_r_nirs, file = "te_r_nirsDR5.RData")
exportCSV(as.data.frame(te_r_nirs[["te"]]), file = "te_r_nirsDR5.csv")

te_r_nirs = data.frame(te_r_nirs = te_r_nirs[["te"]])
VSICMfIndustryFirms1016 =  cbind(VSICMfIndustryFirms1016, te_r_nirs)

ols = lm(te_r_nirs ~ blk + flk, data = VSICMfIndustryFirms1016)
summary(ols)

library(plm)
VSICMfIndustryFirms1016 = VSICMfIndustryFirms1016[!duplicated(VSICMfIndustryFirms1016[c("firmcd", "year")]),]
preg = plm(te_r_nirs ~ blk + flk, data = VSICMfIndustryFirms1016, index = c("firmcd","year"))
summary(preg)

# Stop the timer
proc.time() - ptm 

##RADIAL-VRS
# Start the timer
ptm = proc.time()

te_r_vrs = teradial(wsdftotalrev ~ wsdffxassets + wsemployees + wsdfmaterial + Trend, 
                    VSICMfIndustryFirms1016,
                    rts = c("V"),
                    base = c("output"),
                    ref = NULL, data.ref = NULL, subset.ref = NULL,
                    print.level = 2)

save(te_r_vrs, file = "te_r_vrsDR5.RData")
exportCSV(as.data.frame(te_r_vrs[["te"]]), file = "te_r_vrsDR5.csv")

te_r_vrs = data.frame(te_r_vrs = te_r_vrs[["te"]])
VSICMfIndustryFirms1016 =  cbind(VSICMfIndustryFirms1016, te_r_vrs)

ols = lm(te_r_vrs ~ blk + flk, data = VSICMfIndustryFirms1016)
summary(ols)

library(plm)
VSICMfIndustryFirms1016 = VSICMfIndustryFirms1016[!duplicated(VSICMfIndustryFirms1016[c("firmcd", "year")]),]
preg = plm(te_r_vrs ~ blk + flk, data = VSICMfIndustryFirms1016, index = c("firmcd","year"))
summary(preg)

# Stop the timer
proc.time() - ptm 


###NONRADIAL

##NONRADIAL-CRS
# Start the timer
ptm = proc.time()

te_nr_crs = tenonradial(wsdftotalrev ~ wsdffxassets + wsemployees + wsdfmaterial + Trend, 
                        VSICMfIndustryFirms1016,
                        rts = c("C"),
                        base = c("output"),
                        ref = NULL, data.ref = NULL, subset.ref = NULL,
                        print.level = 2)

save(te_nr_crs, file = "te_nr_crsDR5.RData")
exportCSV(as.data.frame(te_nr_crs[["te"]]), file = "te_nr_crsDR5.csv")

te_nr_crs = data.frame(te_nr_crs = te_nr_crs[["te"]])
VSICMfIndustryFirms1016 =  cbind(VSICMfIndustryFirms1016, te_nr_crs)

ols = lm(te_nr_crs ~ blk + flk, data = VSICMfIndustryFirms1016)
summary(ols)

library(plm)
VSICMfIndustryFirms1016 = VSICMfIndustryFirms1016[!duplicated(VSICMfIndustryFirms1016[c("firmcd", "year")]),]
preg = plm(te_nr_crs ~ blk + flk, data = VSICMfIndustryFirms1016, index = c("firmcd","year"))
summary(preg)

# Stop the timer
proc.time() - ptm 

##NONRADIAL-NIRS
# Start the timer
ptm = proc.time()

te_nr_nirs = tenonradial(wsdftotalrev ~ wsdffxassets + wsemployees + wsdfmaterial + Trend, 
                         VSICMfIndustryFirms1016,
                         rts = c("NI"),
                         base = c("output"),
                         ref = NULL, data.ref = NULL, subset.ref = NULL,
                         print.level = 2)

save(te_nr_nirs, file = "te_nr_nirsDR5.RData")
exportCSV(as.data.frame(te_nr_nirs[["te"]]), file = "te_nr_nirsDR5.csv")

te_nr_nirs = data.frame(te_nr_nirs = te_nr_nirs[["te"]])
VSICMfIndustryFirms1016 =  cbind(VSICMfIndustryFirms1016, te_nr_nirs)

ols = lm(te_nr_nirs ~ blk + flk, data = VSICMfIndustryFirms1016)
summary(ols)

library(plm)
VSICMfIndustryFirms1016 = VSICMfIndustryFirms1016[!duplicated(VSICMfIndustryFirms1016[c("firmcd", "year")]),]
preg = plm(te_nr_nirs ~ blk + flk, data = VSICMfIndustryFirms1016, index = c("firmcd","year"))
summary(preg)

# Stop the timer
proc.time() - ptm 

##NONRADIAL-VRS
# Start the timer
ptm = proc.time()

te_nr_vrs = tenonradial(wsdftotalrev ~ wsdffxassets + wsemployees + wsdfmaterial + Trend, 
                        VSICMfIndustryFirms1016,
                        rts = c("V"),
                        base = c("output"),
                        ref = NULL, data.ref = NULL, subset.ref = NULL,
                        print.level = 2)

save(te_nr_vrs, file = "te_nr_vrsDR5.RData")
exportCSV(as.data.frame(te_nr_vrs[["te"]]), file = "te_nr_vrsDR5.csv")

te_nr_vrs = data.frame(te_nr_vrs = te_nr_vrs[["te"]])
VSICMfIndustryFirms1016 =  cbind(VSICMfIndustryFirms1016, te_nr_vrs)

ols = lm(te_nr_vrs ~ blk + flk, data = VSICMfIndustryFirms1016)
summary(ols)

library(plm)
VSICMfIndustryFirms1016 = VSICMfIndustryFirms1016[!duplicated(VSICMfIndustryFirms1016[c("firmcd", "year")]),]
preg = plm(te_nr_vrs ~ blk + flk, data = VSICMfIndustryFirms1016, index = c("firmcd","year"))
summary(preg)

# Stop the timer
proc.time() - ptm 


###BOOTTRAP CORRECTION
##BC-CRS
# Start the timer
ptm = proc.time()

te_bc_crs = teradialbc(wsdftotalrev ~ wsdffxassets + wsemployees + wsdfmaterial, VSICMfIndustryFirms1016,
                       rts = c("C"),
                       base = c("output"),
                       reps = 1999, 
                       ref = NULL, data.ref = NULL, subset.ref = NULL,
                       print.level = 2)

save(te_bc_crs, file = "te_bc_crs.RData")
exportCSV(as.data.frame(te_bc_crs[["te"]]), file = "te_bc_crs2.csv")

te_bc_crs = data.frame(te_bc_crs = te_bc_crs[["te"]])
VSICMfIndustryFirms1016 =  cbind(VSICMfIndustryFirms1016, te_bc_crs)

ols = lm(te_bc_crs ~ blk + flk, data = VSICMfIndustryFirms1016)
summary(ols)

library(plm)
VSICMfIndustryFirms1016 = VSICMfIndustryFirms1016[!duplicated(VSICMfIndustryFirms1016[c("firmcd", "year")]),]
preg = plm(te_bc_crs ~ blk + flk, data = VSICMfIndustryFirms1016, index = c("firmcd","year"))
summary(preg)


# Stop the timer
proc.time() - ptm 

##BC-NIRS
# Start the timer
ptm = proc.time()

te_rbc_nirs = teradialbc(wsdftotalrev ~ wsdffxassets + wsemployees + wsdfmaterial, VSICMfIndustryFirms1016,
                         rts = c("NI"),
                         base = c("output"),
                         reps = 1999, 
                         ref = NULL, data.ref = NULL, subset.ref = NULL,
                         print.level = 2)

save(te_bc_nirs, file = "te_bc_nirs.RData")
exportCSV(as.data.frame(te_bc_nirs[["te"]]), file = "te_bc_nirs2.csv")

te_bc_nirs = data.frame(te_bc_nirs = te_bc_nirs[["te"]])
VSICMfIndustryFirms1016 =  cbind(VSICMfIndustryFirms1016, te_bc_nirs)

ols = lm(te_bc_nirs ~ blk + flk, data = VSICMfIndustryFirms1016)
summary(ols)

library(plm)
VSICMfIndustryFirms1016 = VSICMfIndustryFirms1016[!duplicated(VSICMfIndustryFirms1016[c("firmcd", "year")]),]
preg = plm(te_bc_nirs ~ blk + flk, data = VSICMfIndustryFirms1016, index = c("firmcd","year"))
summary(preg)


# Stop the timer
proc.time() - ptm 

##BC-VRS
# Start the timer
ptm = proc.time()

te_bc_vrs = teradialbc(wsdftotalrev ~ wsdffxassets + wsemployees + wsdfmaterial, VSICMfIndustryFirms1016,
                       rts = c("V"),
                       base = c("output"),
                       reps = 1999, 
                       ref = NULL, data.ref = NULL, subset.ref = NULL,
                       print.level = 2)


save(te_bc_vrs, file = "te_bc_vrs.RData")
exportCSV(as.data.frame(te_bc_vrs[["te"]]), file = "te_bc_vrs2.csv")

te_bc_vrs = data.frame(te_bc_vrs = te_bc_vrs[["te"]])
VSICMfIndustryFirms1016 =  cbind(VSICMfIndustryFirms1016, te_bc_vrs)

ols = lm(te_bc_vrs ~ blk + flk, data = VSICMfIndustryFirms1016)
summary(ols)

library(plm)
VSICMfIndustryFirms1016 = VSICMfIndustryFirms1016[!duplicated(VSICMfIndustryFirms1016[c("firmcd", "year")]),]
preg = plm(te_bc_vrs ~ blk + flk, data = VSICMfIndustryFirms1016, index = c("firmcd","year"))
summary(preg)

# Stop the timer
proc.time() - ptm 

exportCSV(VSICMfIndustryFirms1016, file = "VSICMfIndustryFirms1016DWPOSITIVEDR5.csv")

esamples = data.frame(te_r_crsE = te_r_crs[["esample"]], 
                      te_r_nirsE = te_r_nirs[["esample"]], 
                      te_r_vrsE = te_r_vrs[["esample"]],
                      te_nr_crsE = te_nr_crs[["esample"]], 
                      te_nr_nirsE = te_nr_nirs[["esample"]], 
                      te_nr_vrsE = te_nr_vrs[["esample"]])

VSICMfIndustryFirms1016 = cbind(VSICMfIndustryFirms1016, esamples)


VSICMfIndustryFirms1016$Trend = VSICMfIndustryFirms1016$year - 2010
