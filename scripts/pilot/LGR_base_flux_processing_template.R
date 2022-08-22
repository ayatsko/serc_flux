######################################
### DAILY SET-UP ###
#####################################

## Set year,month and date of files being processed
year='2019'
month='June'
date='2019-06-12'


## Identify working directory for corresponding date ###CHANGE FOR NASA###
# Shared SI drive
wd=paste('//SI-SERCFS01/DDrive/SERC/Shared/Biogeochemistry/GCREW/4-SMARTX/3-Data/CH4 and NEE data/',
           year,'/',month,'/',date,sep='')


######################################
# 1. Format LGR raw data
######################################

## Load data.table package
library(data.table)

## Set working directory to 'Raw data' folder 
setwd(paste(wd,'/Raw data',sep=''))

## Get names of all LGR files with GHG concentration
## data ('...f####.txt')
filenames=list.files(pattern='f0',full.names=T)

## Read in LGR datafiles to list
dat=lapply(filenames,read.csv,skip=1)

## Remove rows with NAs
dat=lapply(dat,na.omit)

## Combine all files into single dataframe
dat.all=do.call(rbind,dat)

## Pull out date and time data
date_time=strptime(dat.all[,1],format='%m/%d/%Y %H:%M:%S')

## Add year,month,day,JD,hour,min,sec columns to dataframe
Year=as.numeric(format(date_time,'%Y'))
Month=as.numeric(format(date_time,'%m'))
Day=as.numeric(format(date_time,'%d'))
fDOY=as.numeric(julian(date_time,'2020-01-01'))  #Change for year
Hour=as.numeric(format(date_time,'%k'))
Min=as.numeric(format(date_time,'%M'))
Sec=as.numeric(format(date_time,'%S'))
dat.all=cbind(date_time,Year,Month,Day,fDOY,Hour,Min,Sec,dat.all[,-1])

## Save LGR data as data.table
dat.all=data.table(dat.all)

# Step 2 was SMARTX only # 

######################################
# 3. Format plot metadata
######################################

## Read in file with plot data
plots=read.csv(paste('plot_metadata_',date,'.csv',sep=''))

## Pull out date and time data
plot_date_time_start=strptime(paste(plots$Date,plots$Time_start),'%m/%d/%Y %H:%M')

## Add 30 seconds to set start time to be within flux perio 
plot_date_time_start=plot_date_time_start+30

## Add 5 min (300 sec) to flux start time to get flux end time
plot_date_time_end=plot_date_time_start+300

## Add fDOY columns for start and endtime to dataframe
fDOY_start=as.numeric(julian(plot_date_time_start,'2020-01-01'))  #Change for year
fDOY_end=as.numeric(julian(plot_date_time_end,'2020-01-01'))  #Change for year
plots=cbind(fDOY_start,fDOY_end,plots)


######################################
# 4. Merge LGR files and plot metadata 
######################################

## Load sqldf package
library(sqldf)

## Merge LGR/log and plot metadata files by time of flux (in fractional DOY)

## 'inner join' removes all rows that are not during chamber placement
## 'left join' keeps all data and adds NAs for data not during chamber placement
dat_merged=sqldf('select * from dat_merge1 inner join plots
                 on (dat_all.fDOY between plots.fDOY_start and plots.fDOY_end)')


######################################
### Visually check data clipping ###
######################################

## Plot raw (top) and clipped (bottom) data 
## and see if clipped data looks reasonable
# CH4
par(mfrow=c(2,1),mar=c(4,4,1,1))
with(dat.all,plot(fDOY,X.CH4.d_ppm,ylim=c(1.95,3)))
with(dat_merged,plot(fDOY,X.CH4.d_ppm,ylim=c(1.95,3)))


######################################
# 5. Add 'flag' columns and export merged 
# data to .csv file saved on server
######################################

# Add columns to flag outlier CH4 points
# and populate with 'Y' as default
dat_merged$Flag_CH4='Y'
dat_merged$Flag_CO2='Y'
dat_merged$CH4_notes=''
dat_merged$CO2_notes=''

## Set working directory to 'Working files' folder
setwd(paste(wd,'/Working files',sep=''))

# Export merged file
write.csv(dat_merged,file=paste('merged_data_',date,'.csv',sep=''),row.names=F)


######################################
### Plot individual fluxes for QC ###
######################################

## Identify start of fluxes
flux.times=unique(dat_merged$Time_start)

## Set up display to include 16 plots  
### NASA: If you're doing more than 8 chambers at a time you'll need to edit this code ###
par(mfrow=c(4,4),mar=c(1,1,1,1),oma=c(4,4,0,0))

## Set number of fluxes to plot (max 16 at a time--8 CH4 and 8 CO2)
if (length(flux.times)<8) {
  end=length(flux.times)
} else {
  end=8
}

## Look at the plots and decide what points need to be flagged
## in the Excel file.

######################################
# 6. Read in .csv, remove flagged points, 
# and save CH4 and CO2 data separately 
######################################

## Set working directory to 'Working files' folder
setwd(paste(wd,'/Working files',sep=''))

## Read in flagged datafile
dat.flag=read.csv(paste('merged_data_',date,'_flagged.csv',sep=''))

## For CH4:
## Remove rows of data that don't have flag 'Y'
dat.CH4=subset(dat.flag,Flag_CH4=='Y')
## Remove columns of data that are not relevant
## and reorder remaining columns  ### CHANGE FOR NASA ###
dat.CH4=dat.CH4[,c('Date','Year','Month','Day','Hour','Min','Sec','fDOY',
                   'Time_start','Plot','Area','Volume','Light','X.CH4.d_ppm',
                   'X.CH4.d_ppm_sd','AICTemp','AOCTemp','STemp','Airtemp_Avg','RH_Avg',
                   'VP_Avg','VPD_Avg','SVP_Avg','Wind_Speed_Avg','Wind_Direction_Avg')]

## For CO2:
## Remove rows of data that don't have flag 'Y'
dat.CO2=subset(dat.flag,Flag_CO2=='Y')
## Remove columns of data that are not relevant
## and reorder remaining columns  ### CHANGE FOR NASA ###
dat.CO2=dat.CO2[,c('Date','Year','Month','Day','Hour','Min','Sec','fDOY',
                   'Time_start','Plot','Light','Area','Volume','Light','X.CO2.d_ppm',
                   'X.CO2.d_ppm_sd','AICTemp','AOCTemp','STemp','Airtemp_Avg','RH_Avg',
                   'VP_Avg','VPD_Avg','SVP_Avg','Wind_Speed_Avg','Wind_Direction_Avg')]


readline('Step 6 completed. Press enter to plot individual fluxes for quality control.')


######################################
### Plot individual fluxes for final QC ###
######################################

## This just lets you make sure the flagging worked properly

## Set up display to include 30 plots
par(mfrow=c(5,6),mar=c(1,1,1,1),oma=c(4,4,0,0))

## Identify start of fluxes
flux.times=unique(dat.CH4$Time_start)

## Set number of CH4 fluxes to plot (max 30 at a time)
if (length(flux.times)<30) {
  end=length(flux.times)
} else {
  end=30
}

## Plot up to 30 CH4 fluxes along with linear regression of flux and R2
for (i in flux.times[1:end]) {
  test=subset(dat.CH4,Time_start==i)
  mod=with(test,lm(X.CH4.d_ppm~fDOY))
  with(test,plot(fDOY,X.CH4.d_ppm))
  abline(mod,col='red')
  r2=summary(mod)$r.squared
  legend('topleft',format(summary(mod)$r.squared,digits=3),bty='n')
  legend('bottomright',i)
}


######################################
# 7. Convert ppm to moles for CH4 and CO2
######################################

## Use known volume of chamber (in m3) to convert 
## ppm of gas to liters of gas
# CH4
dat.CH4$CH4.d_L=
  # parts CH4 per million parts air * volume of air in chamber (m3) * 1000 L per m3
  (dat.CH4$X.CH4.d_ppm/1000000) * dat.CH4$Volume * 1000 
#CO2
dat.CO2$CO2.d_L=
  # parts CO2 per million parts air * volume of air in chamber (m3) * 1000 L per m3
  (dat.CO2$X.CO2.d_ppm/1000000) * dat.CO2$Volume * 1000 

## Use ideal gas law to calculate umol of CH4 or mmol of CO2
### CHANGE FOR NASA -- this is where you need some temperature data ###
# CH4
dat.CH4$CH4.d_umol=
  # (atm pressure * L CH4) / (R in L*atm/°K*mol * °K temp) * 10^6 umol/mol
  ((1*dat.CH4$CH4.d_L)/(0.08206*(dat.CH4$Airtemp_Avg+273)))*10^6
# CO2
dat.CO2$CO2.d_mmol=
  # (atm pressure * L CO2) / (R in L*atm/°K*mol * °K temp) 
  ((1*dat.CO2$CO2.d_L)/(0.08206*(dat.CO2$Airtemp_Avg+273)))*10^3


######################################
# 8. Calculate mean and SE for log variables for each flux
######################################


### CHANGE for NASA - use your column names ###
dat.CH4.mean=aggregate(cbind(AICTemp,AOCTemp,STemp,Airtemp_Avg,
                             RH_Avg)~Time_start,dat.CH4,mean)
names(dat.CH4.mean)=paste(names(dat.CH4.mean),'.mean',sep='')
dat.CO2.mean=aggregate(cbind(AICTemp,AOCTemp,STemp,Airtemp_Avg,
                             RH_Avg)~Time_start,dat.CO2,mean)
names(dat.CO2.mean)=paste(names(dat.CO2.mean),'.mean',sep='')


## Load plotix package
library(plotrix)

## Calculate SE of each variable
### CHANGE for NASA - use your column names ###
dat.CH4.se=aggregate(cbind(AICTemp,AOCTemp,STemp,Airtemp_Avg,
                           RH_Avg)~Time_start,dat.CH4,std.error)
names(dat.CH4.se)=paste(names(dat.CH4.se),'.se',sep='')
dat.CO2.se=aggregate(cbind(AICTemp,AOCTemp,STemp,Airtemp_Avg,
                           RH_Avg)~Time_start,dat.CO2,std.error)
names(dat.CO2.se)=paste(names(dat.CO2.se),'.se',sep='')

######################################
# 9. Calculate chamber fluxes 
######################################

## Create new dataframes to hold final fluxes 
fluxes.CH4=data.frame(matrix(NA,nrow=length(flux.times)))
fluxes.CO2=data.frame(matrix(NA,nrow=length(flux.times)))

## Add named columns
# CH4
fluxes.CH4$Time_start=flux.times
fluxes.CH4$flux.CH4=0
fluxes.CH4$R2.CH4=0
fluxes.CH4$p.CH4=0
# CO2
fluxes.CO2$Time_start=flux.times
fluxes.CO2$flux.CO2=0
fluxes.CO2$R2.CO2=0
fluxes.CO2$p.CO2=0

## Remove initial empty column
fluxes.CO2=fluxes.CO2[,-1]
fluxes.CH4=fluxes.CH4[,-1]

## For each start time
for (i in flux.times) {
  ## CH4 ##
  # Subset data for one chamber measurement
  temp1=subset(dat.CH4,Time_start==i)
  # Set corresponding row of output table
  j=which(flux.times==i)
  # Determine if start time has a CH4 flux
  if (nrow(temp1)>0) {
    # If so:  
    # Calulate flux in umol/day using linear regression
    mod=with(temp1,lm(CH4.d_umol~fDOY))
    # Save flux rate and R2 and p-value of slope in corresponding row of dataframe
    # flux rate, converted from umol/day to umol/m2/day
    fluxes.CH4$flux.CH4[j]=coef(mod)[2]/0.16 #area of chamber in m2
    # R2 of slope
    fluxes.CH4$R2.CH4[j]=summary(mod)$r.squared
    # p-value of slope
    fluxes.CH4$p.CH4[j]=summary(mod)$coefficients[2,4]
    # If not:
    # Fill rows of table with NA    
  } else {
    fluxes.CH4$flux.CH4[j]=NA
    fluxes.CH4$R2.CH4[j]=NA
    fluxes.CH4$p.CH4[j]=NA
  }
  ## CO2 ##
  # Subset data for one chamber measurement
  temp2=subset(dat.CO2,Time_start==i)
  # Calulate flux in mol/day using linear regression
  mod=with(temp2,lm(CO2.d_mmol~fDOY))
  # Save flux rate and R2 and p-value of slope in corresponding row of dataframe
  # flux rate, converted from mol/day to mol/m2/day
  fluxes.CO2$flux.CO2[j]=coef(mod)[2]/0.16 #area of chamber
  # R2 of slope
  fluxes.CO2$R2.CO2[j]=summary(mod)$r.squared
  # p-value of slope
  fluxes.CO2$p.CO2[j]=summary(mod)$coefficients[2,4]
}

## Merge flux data with plot metadata, keeping desired columns
### CHANGE for NASA - use your column names ###
# CO2
fluxes.CO2=merge(plots[,c('Date','Plot','Time_start','Light')],fluxes.CO2)
# CH4
fluxes.CH4=merge(plots[,c('Date','Plot','Time_start','Light')],fluxes.CH4)

## Merge flux data with log data, keeping desired columns
### CHANGE for NASA - use your column names ###
# CO2
fluxes.CO2=merge(fluxes.CO2,as.data.frame(dat.CO2.mean)[,c('Time_start.mean','AICTemp.mean',
                                                           'AOCTemp.mean','STemp.mean',
                                                           'Airtemp_Avg.mean','RH_Avg.mean')],
                 by.x='Time_start',by.y='Time_start.mean')
fluxes.CO2=merge(fluxes.CO2,as.data.frame(dat.CO2.se)[,c('Time_start.se','AICTemp.se',
                                                         'AOCTemp.se','STemp.se','Airtemp_Avg.se','RH_Avg.se')],
                 by.x='Time_start',by.y='Time_start.se')
# CH4
fluxes.CH4=merge(fluxes.CH4,as.data.frame(dat.CH4.mean)[,c('Time_start.mean','AICTemp.mean',
                                                           'AOCTemp.mean','STemp.mean',
                                                           'Airtemp_Avg.mean','RH_Avg.mean')],
                 by.x='Time_start',by.y='Time_start.mean')
fluxes.CH4=merge(fluxes.CH4,as.data.frame(dat.CH4.se)[,c('Time_start.se','AICTemp.se',
                                                         'AOCTemp.se','STemp.se','Airtemp_Avg.se','RH_Avg.se')],
                 by.x='Time_start',by.y='Time_start.se')

## Set units for each column
### CHANGE for NASA - need units to match with new columns ###
# CO2
units.CO2=c('<NA>','<NA>','<NA>','<NA>','mmol/m2/d','<NA>','<NA>',
            '°C','°C','°C','°C','<NA>','°C','°C','°C','°C','<NA>')
# CH4
units.CH4=c('<NA>','<NA>','<NA>','<NA>','mmol/m2/d','<NA>','<NA>',
            '°C','°C','°C','°C','<NA>','°C','°C','°C','°C','<NA>')

## Add row with units
options(warn=-1) # suppress warnings about NA not being a factor level
# CO2
fluxes.CO2=rbind(units.CO2,fluxes.CO2)
# CH4
fluxes.CH4=rbind(units.CH4,fluxes.CH4)

## Set working directory 
setwd(wd)

## Export fluxes as .csv file
# CO2
write.csv(fluxes.CO2,file=paste('final_CO2_fluxes_',date,'.csv',sep=''),row.names=F)
# CH4
write.csv(fluxes.CH4,file=paste('final_CH4_fluxes_',date,'.csv',sep=''),row.names=F)

