## ---- echo=FALSE, message=FALSE,warning=FALSE, out.height="35%",fig.align="center"----
library(pacman)
p_load(mtvc,dplyr,readxl,tidyverse)
knitr::include_graphics("G:\\Il mio Drive\\journal\\FINALE\\git\\tve.png")


## ---- echo=FALSE, message=FALSE,warning=FALSE---------------------------------
# load dataset
df=read_xlsx("G:\\Il mio Drive\\journal\\FINALE\\git\\intermittence.xlsx")


## -----------------------------------------------------------------------------
df=df %>% 
  mutate(across(c(startfu,stopfu,datedrug,datevax),as.Date),
         vax=ifelse(!(is.na(datevax)),
                    1,
                    0)) %>% 
  arrange(id,datevax,datedrug) %>% 
  mutate(expiredrug=as.Date(ifelse(is.na(datedrug),
                              NA,
                              as.Date(datedrug+5))))


## -----------------------------------------------------------------------------
expire=df %>% 
  unique() %>% 
  group_by(id) %>% 
  mutate(expiredrug=as.Date(case_when(lead(datedrug)<=expiredrug~lead(datedrug)+5,
                                         is.na(lead(datedrug)) & lag(expiredrug)<=datedrug~datedrug+5,
                                         T~expiredrug)))


## -----------------------------------------------------------------------------
long=expire %>%
  gather(.,motivation,day,c(startfu,stopfu,
                            datedrug, datevax,
                            expiredrug)) %>% 
  arrange(id,day) %>% 
  filter(!(is.na(day))) %>%  # deletes obs that have NAs because prescription of drug/vax is missing
  unique() %>% # deletes obs that are duplicated because of NAs 
  group_by(id) %>% # groups by patient
  mutate(start=as.numeric(ifelse(row_number()==1, # set start of the study 
                                 0,
                                 day-min(day))),
         stop=as.numeric(lead(day)-min(day))) %>% 
  filter(!(is.na(stop))) %>% 
  mutate(t=row_number(), # set number of obs in time
         vax=ifelse(motivation=='datevax', # sets vax indicator to one if associated that is due to vax
                    1,
                    0),
         drug=NA) %>% # initializes drug indicator to missing
  mutate(stop.lag=lag(stop), # creates variable that will be used to set start and stop times
         start.lag=lag(start),
         stop=ifelse(start==stop,
                     stop+0.1,
                     stop),
         start=case_when(is.na(start.lag)~start,
                         start==start.lag~start+0.1,
                         T~start))


## -----------------------------------------------------------------------------
# now switch on vaccine based on the date it happened
for (i in 2:dim(long)[1]){
  long$vax[i]=ifelse(long$vax[i-1]==1 & 
                       long$id[i] == long$id[i-1],
                        1,
                     long$vax[i])
}


## -----------------------------------------------------------------------------
# now switch on and off the drug indicator
for (i in 2:dim(long)[1]) {
  long$drug[i]=ifelse(long$motivation[i]=='datedrug',
                      1,
                      0)
  long$drug[i]=case_when(long$id[i]!=long$id[i-1]~0,
                         long$motivation[i] %in% c('datedrug','expiredrug')~long$drug[i],
                         T~long$drug[i-1])
}


## -----------------------------------------------------------------------------
for (i in 1:dim(long)[1]){
  long$drug[i]=ifelse(is.na(long$drug[i]),
                      0,
                      long$drug[i])
}
# now add event
long=long %>% 
  group_by(id) %>% 
  mutate(tevent=ifelse(row_number()!=n(),
                       0,
                       event)) %>% 
  select(id,start,stop,drug,vax,tevent)


## -----------------------------------------------------------------------------
print(long[long$id %in% c(1,2,3,4),])


## ---- echo=FALSE--------------------------------------------------------------
knitr::purl("G:\\Il mio Drive\\journal\\FINALE\\git\\README.Rmd")

