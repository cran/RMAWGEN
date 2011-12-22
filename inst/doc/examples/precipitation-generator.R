# 
# This file contains a script example about to perform a precipitation stovhastic generation 
#
#
# Emanuele Cordano on 17-12-2011
#
###############################################################################
rm(list=ls())
set.seed(1222)
library(RMAWGEN)
data(trentino)

year_max <- 1990
year_min <- 1961
origin <- "1961-1-1"

n_GPCA_iter <- 100

station <- c("T0090","T0083","T0099","T0001") #//,"T0010","")

vstation <- 	vstation <- c("B2440","B6130","B8570","B9100","LAVIO","POLSA","SMICH","T0001",
		"T0010","T0014","T0018","T0032","T0064","T0083","T0090","T0092","T0094","T0099",
		"T0102","T0110","T0129","T0139","T0147","T0149","T0152","T0157","T0168","T0179","T0189","T0193","T0204","T0210","T0211","T0327","T0367","T0373")		

station <- vstation
generation00 <- ComprehensivePrecipitationGenerator(station=station,prec_all=PRECIPITATION,year_min=year_min,year_max=year_max,p=1,n_GPCA_iteration=n_GPCA_iter,n_GPCA_iteration_residuals=0,sample="monthly")


data_gen <- extractmonths(data=generation00$prec_gen,when=c("Jun","Jul","Aug"),origin="1961-1-1")
data_mes <- extractmonths(data=generation00$prec_mes,when=c("Jun","Jul","Aug"),origin="1961-1-1")

c_mes <- continuity_ratio(data_gen,valmin=1.0)
c_gen <- continuity_ratio(data_mes,valmin=1.0)

print(generation00$var)

