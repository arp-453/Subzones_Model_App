library(tidyverse)
library(dplyr)
library(janitor)

### Import files------------------------------------
Subzone_Raw <- read.csv(file = "subzone_check1.csv",
                        na.strings = c("NA", "-", " ")) %>%
# Delete rows and cells containing NA
  na.omit()%>%
  view()

###Check for duplicates in the Unique ID Column 
Subzone_Raw <- Subzone_Raw %>% 
  distinct(subzone, .keep_all = TRUE)%>%
# filter out the out-of-range   
  filter(subzone < 17419) %>%
  filter(subzone > 0) %>%
view()

###Check IDs consecutive, found the out-of-range from above 
subzone_consec = as.numeric(unlist(Subzone_Raw[1]))
max <-  max(subzone_consec, na.rm = FALSE)
min <-  min(subzone_consec, na.rm = FALSE) 
#Check that max-min+1 = max 
max - min + 1
# ID where the wrong min was coming from
# which.min(subzone_consec)
# ID where wrong max was coming from
# which.max(subzone_consec)


#### FIPS code check that all have:
#### IL = 17, IN = 18, WI = 55

Fips_IL <- Subzone_Raw %>%
  select(FIPS, state)%>%
  filter(str_detect(state, "IL"))%>%
  filter(FIPS != 17)%>%
# Should be empty if every row with state = IL, FIPS = 17 
view()  
  
Fips_WI <- Subzone_Raw %>%
  select(FIPS, state)%>%
  filter(str_detect(state, "WI"))%>%
  filter(FIPS != 55)%>%
view()    
  
Fips_IN <- Subzone_Raw %>%
  select(FIPS, state)%>%
  filter(str_detect(state, "IN"))%>%
  filter(FIPS != 18)%>%
view()    
  
####Check that Chicago is within Cook 
Chicago_Flag <- Subzone_Raw%>%
  filter(chicago != 0)%>%
# check on if the indicator is > 1 anywhere
  #filter(chicago > 1) %>%
  #Check on non-Cook places. 
  filter(str_detect(county, "COOK") == FALSE)%>%
  view()
#Found 10 non-Cook Chicago flags ! 

#Make sure counties match states 
Counties_IL <- Subzone_Raw %>%
  select(subzone, county, state)%>%
  filter(str_detect(state, "IL"))%>%
# ID where "MCHNRY" is:
# filter(str_detect(county, "MCHNRY"))%>%
# Isolate the county options and then look them up on google
  select(county, state)%>%
  unique()%>%
  view()

Counties_WI <- Subzone_Raw %>%
  select(county, state)%>%
  filter(str_detect(state, "WI"))%>%
  unique()%>%
  view()

Counties_IN <- Subzone_Raw %>%
  select(county, state)%>%
  filter(str_detect(state, "IN"))%>%
  unique()%>%
  view()

#Make sure states are all spelled correctly
States_Sp <- Subzone_Raw %>%
#  select(subzone, state)%>%
# Found "IA" at subzone 5998
#  filter(str_detect(state, "IA"))%>%
  select(state)%>%
  unique()%>%
  view()
  
  
  
  
  
  
  
  







  
