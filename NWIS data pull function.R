AR_USGS_RETRIEVAL <- function(startDate, endDate){
  
  packages <- c('dataRetrieval', 'data.table','dplyr','tidyverse','lubridate','magrittr')
  
  for (x in packages) {
    if(!require(x, character.only = T, quietly = T)){
      install.packages(x)
      library(x, character.only = T)
    }
  }

  
  params = c("00010", #temp
             "00300", #DO
             "00400") #pH
  
  site = data.frame()
  
  sites <- read.csv("E:/2022 Integrated Report/Data Assessments/WQ-CONTINUOUS ASSESSMENTS/LONGTERM/longterm assessment work/Input/Continuous sites.csv")
  
  sites$site_no<- as.character(paste0("0",sites$StationID))
  
  
  siteid <- sites$site_no
  
  for(x in siteid){
    
    streamdat<- readNWISuv(x, #Station Code
                           parameterCd = params, #Water Quality Codes
                           startDate = startDate, #Start of Period of Record
                           endDate = endDate,  #End of Period of Record
                           tz= "America/Chicago") #Time zone
    
    #Renames columns to meaning full names
    streamdat <- renameNWISColumns(streamdat)    
    
    streamdat <- left_join(streamdat, sites, by.x = site_no)
    
    
    #Add Date_hour column
    streamdat$Date<-strftime(streamdat$dateTime, format= "%Y-%m-%d %H")
    streamdat$Year<-strftime(streamdat$dateTime, format = "%Y")
    streamdat$Month<-strftime(streamdat$dateTime, format = "%m")
    streamdat$Day<-strftime(streamdat$dateTime, format = "%d")
    streamdat$Time<-strftime(streamdat$dateTime, format = "%H")
  
    head(streamdat)
    
    Hourly_Averages <- streamdat %>%
      group_by(Date, Time) %>%
      summarise(Avg_Hourly_Temp = if("Wtemp_Inst" %in% colnames(.)) mean(Wtemp_Inst, na.rm = TRUE),
                Avg_Hourly_DO = if("DO_Inst" %in% colnames(.)) mean(DO_Inst, na.rm = TRUE),
                Avg_Hourly_pH = if("pH_Inst" %in% colnames(.)) mean(pH_Inst, na.rm = TRUE))
    
    local <- paste0(streamdat$site_no[1],"-",streamdat$Description[1])
    
    path <- paste0("E:/2022 Integrated Report/Data Assessments/WQ-CONTINUOUS ASSESSMENTS/LONGTERM/longterm assessment work/Output/","test/",local)
    
    site <- rbind(site,local)
    
    dir.create(path)
    
    setwd(path)

    write.csv(Hourly_Averages, paste0(local,".csv"))
  }
  write.csv(site, paste0("E:/2022 Integrated Report/Data Assessments/WQ-CONTINUOUS ASSESSMENTS/LONGTERM/longterm assessment work/Output/","test/","Sites.csv"))
  
}

#AR_USGS_RETRIEVAL("2016-04-01","2021-03-31")
