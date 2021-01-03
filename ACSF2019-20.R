#======================================================================================================
# Visualisation of Australian's consumption foods, the amounts of Nutrients from those foods, based on supermarket sales data. 
# Data source is Apparent Consumption of Selected Foodstuffs, Australia, 2019-20
# https://www.abs.gov.au/statistics/health/health-conditions-and-risks/apparent-consumption-selected-foodstuffs-australia/2019-20/4316DO001_201920.xls
#======================================================================================================
library(tidyverse)
library(readxl)
library(writexl)
library(highcharter)
library(rsconnect)
library(RColorBrewer)
library(lubridate)
#======================================================================================================

`%!in%` = Negate(`%in%`)

#---- Load Datacube  -------------------------

#-- Download datacube
daturl <- "https://www.abs.gov.au/statistics/health/health-conditions-and-risks/apparent-consumption-selected-foodstuffs-australia/2019-20/4316DO001_201920.xls"

download.file(daturl,"datacube.xls", mode = "wb" )


g2 <- read_excel("./datacube.xls", sheet = 3, range = "A7:AA147" )   # T1.2
n2<- read_excel("./datacube.xls", sheet = 5, range = "A7:D1063" )   # T2.2
kJ5 <- read_excel("./datacube.xls", sheet = 11, range = "A8:G148" )   # T5.1
kJdisc52 <- read_excel("./datacube.xls", sheet = 12, range = "A8:I148" )  # T5.2
kj53 <- read_excel("./datacube.xls", sheet = 13, range = "A7:AA147" )   # T1


g1 <- read_excel("./datacube.xls", sheet = 2, range = "A8:G148" )   # T1
Protein6 <- read_excel("./datacube.xls", sheet = 14, range = "A8:G148" )  # T6
Carb7 <- read_excel("./datacube.xls", sheet = 15, range = "A8:G148" )  # T7
Fat8 <- read_excel("./datacube.xls", sheet = 16, range = "A8:G148" )  # T8
Added_sugar9 <- read_excel("./datacube.xls", sheet = 17, range = "A8:G148" )  # T9
Free_sugar10 <- read_excel("./datacube.xls", sheet = 18, range = "A8:G148" )  # T10
SatFat11 <- read_excel("./datacube.xls", sheet = 19, range = "A8:G148" )  # T11    
Sodium12 <- read_excel("./datacube.xls", sheet = 20, range = "A8:G148" )  # T12
Fibre13 <- read_excel("./datacube.xls", sheet = 21, range = "A8:G148" )  # T13

FFG4 <- read_excel("./datacube.xls", sheet = 8, range = "A8:C28" )  # T4.1
FFG43 <- read_excel("./datacube.xls", sheet = 10, range = "A9:F87" )  # T4.3

SSB <- read_excel("./datacube.xls", sheet = 22, range = "A6:H4374" )  # T14.2

#---Load labels - download from Github
laburl <- "https://github.com/Atyepa/AUSNUT/raw/master/AUSNUT_maj_subma_shortlabels.xlsx"

download.file(laburl,"AUSNUT_maj_subma_shortlabels.xlsx", mode = "wb" )

Labels <- read_excel ("./AUSNUT_maj_subma_shortlabels.xlsx")


#---Tidy Nutrient tables. Give names, remove NAs ---
g <- g1 %>%
    drop_na() %>%  
    rename("Class_lvl" = "...1",   "Code" =  "...2", "Label" = "...3", "2018-19,Tonnes" = "2018-19(d)...4",
           "2019-20,Tonnes" = "2019-20...5", "2018-19,g" = "2018-19(d)...6", "2019-20,g" = "2019-20...7") %>% 
    filter(`2019-20,Tonnes` !="n.a") %>%    # drop "n.a." row
    pivot_longer(cols = 4:7, names_to = "Year", values_to = "Val") %>% 
    separate(Year, into = c("Year","Unit"), sep = "," ) %>% 
    mutate(Val = as.numeric(Val)) %>% 
    mutate(Nutrient = "Weight") %>%
    group_by(Unit, Year) %>%
    mutate(pc = round(Val/max(Val)*100,1)) %>%
    select(1:3, Year, Nutrient, Unit, Val, pc)


kJ <- kJ5 %>%
    drop_na() %>%  
    rename("Class_lvl" = "...1",   "Code" =  "...2", "Label" = "...3", "2018-19,kJ" = "2018-19(d)...4",
           "2019-20,kJ" = "2019-20...5", "2018-19,pc" = "2018-19(d)...6", "2019-20,pc" = "2019-20...7") %>% 
    filter(`2019-20,kJ` !="n.a") %>%    # drop "n.a." row
    pivot_longer(cols = 4:7, names_to = "Year", values_to = "Val") %>% 
    separate(Year, into = c("Year","Unit"), sep = "," ) %>% 
    mutate(Val = as.numeric(Val)) %>% 
    mutate(Nutrient = "Energy") %>%
    spread(Unit, Val) %>% 
    rename(Val = kJ) %>% 
    mutate(Unit = "kJ") %>% 
    select(1:3, Year, Nutrient, Unit, Val, pc)
    
    
kJdiscD <- kJdisc52 %>%
    drop_na() %>%  
    rename("Class_lvl" = "...1",   "Code" =  "...2", "Label" = "...3", "2018-19,kJ" = "2018-19(d)...4",
           "2019-20,kJ" = "2019-20...5", "2018-19,pc_T" = "2018-19(d)...6", "2019-20,pc_T" = "2019-20...7", 
           "2018-19,pc_D" = "2018-19(d)...8", "2019-20,pc_D" = "2019-20...9") %>% 
    filter(`2019-20,kJ` !="n.a", Code != "29") %>%    # drop n.a. & alcohol rows
    pivot_longer(cols = 4:9, names_to = "Year", values_to = "Val") %>% 
    separate(Year, into = c("Year","Unit"), sep = "," ) %>% 
    mutate(Val = as.numeric(Val)) %>% 
    spread(Unit, Val) %>% 
    rename(Val = kJ) %>% 
    mutate(Unit = "kJ") %>% 
    mutate(Nutrient = "Disc. Energy (of disc kJ)") %>% 
    select(1:3, Year, Nutrient, Unit, Val, pc_D) %>% 
    rename(pc = pc_D) 


kJdisc <- kJdisc52 %>%
    drop_na() %>%  
    rename("Class_lvl" = "...1",   "Code" =  "...2", "Label" = "...3", "2018-19,kJ" = "2018-19(d)...4",
           "2019-20,kJ" = "2019-20...5", "2018-19,pc_T" = "2018-19(d)...6", "2019-20,pc_T" = "2019-20...7", 
           "2018-19,pc_D" = "2018-19(d)...8", "2019-20,pc_D" = "2019-20...9") %>% 
    filter(`2019-20,kJ` !="n.a", Code != "29") %>%    # drop n.a. & alcohol rows
    pivot_longer(cols = 4:9, names_to = "Year", values_to = "Val") %>% 
    separate(Year, into = c("Year","Unit"), sep = "," ) %>% 
    mutate(Val = as.numeric(Val)) %>% 
    spread(Unit, Val) %>% 
    rename(Val = kJ) %>% 
    mutate(Unit = "kJ") %>% 
    mutate(Nutrient = "Disc. Energy (of total kJ)") %>% 
    select(1:3, Year, Nutrient, Unit, Val, pc_T) %>% 
    rename(pc = pc_T) %>% 
    bind_rows(kJdiscD)


Protein <- Protein6 %>%
    drop_na() %>%  
    rename("Class_lvl" = "...1",   "Code" =  "...2", "Label" = "...3", "2018-19,g" = "2018-19(d)...4",
           "2019-20,g" = "2019-20...5", "2018-19,pc" = "2018-19(d)...6", "2019-20,pc" = "2019-20...7") %>% 
    filter(`2019-20,g` !="n.a") %>%    # drop "n.a." row
    pivot_longer(cols = 4:7, names_to = "Year", values_to = "Val") %>% 
    separate(Year, into = c("Year","Unit"), sep = "," ) %>% 
    mutate(Val = as.numeric(Val)) %>% 
    mutate(Nutrient = "Protein") %>%
    spread(Unit, Val) %>% 
    rename(Val = g) %>% 
    mutate(Unit = "g") %>% 
    select(1:3, Year, Nutrient, Unit, Val, pc)


Carb <- Carb7 %>%
    drop_na() %>%  
    rename("Class_lvl" = "...1",   "Code" =  "...2", "Label" = "...3", "2018-19,g" = "2018-19(d)...4",
           "2019-20,g" = "2019-20...5", "2018-19,pc" = "2018-19(d)...6", "2019-20,pc" = "2019-20...7") %>% 
    filter(`2019-20,g` !="n.a") %>%    # drop "n.a." row
    pivot_longer(cols = 4:7, names_to = "Year", values_to = "Val") %>% 
    separate(Year, into = c("Year","Unit"), sep = "," ) %>% 
    mutate(Val = as.numeric(Val)) %>% 
    mutate(Nutrient = "Carbohydrate") %>%
    spread(Unit, Val) %>% 
    rename(Val = g) %>% 
    mutate(Unit = "g") %>% 
    select(1:3, Year, Nutrient, Unit, Val, pc)


Fat <- Fat8 %>%
    drop_na() %>%  
    rename("Class_lvl" = "...1",   "Code" =  "...2", "Label" = "...3", "2018-19,g" = "2018-19(d)...4",
           "2019-20,g" = "2019-20...5", "2018-19,pc" = "2018-19(d)...6", "2019-20,pc" = "2019-20...7") %>% 
    filter(`2019-20,g` !="n.a") %>%    # drop "n.a." row
    pivot_longer(cols = 4:7, names_to = "Year", values_to = "Val") %>% 
    separate(Year, into = c("Year","Unit"), sep = "," ) %>% 
    mutate(Val = as.numeric(Val)) %>% 
    mutate(Nutrient = "Total fat") %>%
    spread(Unit, Val) %>% 
    rename(Val = g) %>% 
    mutate(Unit = "g") %>% 
    select(1:3, Year, Nutrient, Unit, Val, pc)


Added_sugar<- Added_sugar9 %>%
    drop_na() %>%  
    rename("Class_lvl" = "...1",   "Code" =  "...2", "Label" = "...3", "2018-19,g" = "2018-19(d)...4",
           "2019-20,g" = "2019-20...5", "2018-19,pc" = "2018-19(d)...6", "2019-20,pc" = "2019-20...7") %>% 
    filter(`2019-20,g` !="n.a") %>%    # drop "n.a." row
    pivot_longer(cols = 4:7, names_to = "Year", values_to = "Val") %>% 
    separate(Year, into = c("Year","Unit"), sep = "," ) %>% 
    mutate(Val = as.numeric(Val)) %>% 
    mutate(Nutrient = "Added sugar") %>%
    spread(Unit, Val) %>% 
    rename(Val = g) %>% 
    mutate(Unit = "g") %>% 
    select(1:3, Year, Nutrient, Unit, Val, pc)



Free_sugar<- Free_sugar10 %>%
    drop_na() %>%  
    rename("Class_lvl" = "...1",   "Code" =  "...2", "Label" = "...3", "2018-19,g" = "2018-19(d)...4",
           "2019-20,g" = "2019-20...5", "2018-19,pc" = "2018-19(d)...6", "2019-20,pc" = "2019-20...7") %>% 
    filter(`2019-20,g` !="n.a") %>%    # drop "n.a." row
    pivot_longer(cols = 4:7, names_to = "Year", values_to = "Val") %>% 
    separate(Year, into = c("Year","Unit"), sep = "," ) %>% 
    mutate(Val = as.numeric(Val)) %>% 
    mutate(Nutrient = "Free sugar") %>%
    spread(Unit, Val) %>% 
    rename(Val = g) %>% 
    mutate(Unit = "g") %>% 
    select(1:3, Year, Nutrient, Unit, Val, pc)


SatFat <- SatFat11 %>%
    drop_na() %>%  
    rename("Class_lvl" = "...1",   "Code" =  "...2", "Label" = "...3", "2018-19,g" = "2018-19(d)...4",
           "2019-20,g" = "2019-20...5", "2018-19,pc" = "2018-19(d)...6", "2019-20,pc" = "2019-20...7") %>% 
    filter(`2019-20,g` !="n.a") %>%    # drop "n.a." row
    pivot_longer(cols = 4:7, names_to = "Year", values_to = "Val") %>% 
    separate(Year, into = c("Year","Unit"), sep = "," ) %>% 
    mutate(Val = as.numeric(Val)) %>% 
    mutate(Nutrient = "Saturated fat") %>%
    spread(Unit, Val) %>% 
    rename(Val = g) %>% 
    mutate(Unit = "g") %>% 
    select(1:3, Year, Nutrient, Unit, Val, pc)



Sodium <- Sodium12 %>%
    drop_na() %>%  
    rename("Class_lvl" = "...1",   "Code" =  "...2", "Label" = "...3", "2018-19,g" = "2018-19(d)...4",
           "2019-20,g" = "2019-20...5", "2018-19,pc" = "2018-19(d)...6", "2019-20,pc" = "2019-20...7") %>% 
    filter(`2019-20,g` !="n.a") %>%    # drop "n.a." row
    pivot_longer(cols = 4:7, names_to = "Year", values_to = "Val") %>% 
    separate(Year, into = c("Year","Unit"), sep = "," ) %>% 
    mutate(Val = as.numeric(Val)) %>% 
    mutate(Nutrient = "Sodium") %>%
    spread(Unit, Val) %>% 
    rename(Val = g) %>% 
    mutate(Unit = "mg") %>% 
    select(1:3, Year, Nutrient, Unit, Val, pc)


Fibre <- Fibre13 %>%
    drop_na() %>%  
    rename("Class_lvl" = "...1",   "Code" =  "...2", "Label" = "...3", "2018-19,g" = "2018-19(d)...4",
           "2019-20,g" = "2019-20...5", "2018-19,pc" = "2018-19(d)...6", "2019-20,pc" = "2019-20...7") %>% 
    filter(`2019-20,g` !="n.a") %>%    # drop "n.a." row
    pivot_longer(cols = 4:7, names_to = "Year", values_to = "Val") %>% 
    separate(Year, into = c("Year","Unit"), sep = "," ) %>% 
    mutate(Val = as.numeric(Val)) %>% 
    mutate(Nutrient = "Dietary fibre") %>%
    spread(Unit, Val) %>% 
    rename(Val = g) %>% 
    mutate(Unit = "g") %>% 
    select(1:3, Year, Nutrient, Unit, Val, pc)


#----Append Nutrient dfs ----
Nutrients <- g %>%
    bind_rows(kJ,kJdisc, Protein, Carb, Fat, Added_sugar, Free_sugar, SatFat, Sodium, Fibre) %>% 
    mutate(Code = as.numeric(Code), period = "Finyr") %>% 
    rename(Value = Val) %>% 
    select(1:3,period, Year, Nutrient, Unit, Value, pc) 

#---Remove intermediate objects
rm(n2, kJ5, kJdisc52, kJdiscD, g1, Protein6, Carb7, Fat8, Added_sugar9, Free_sugar10, SatFat11, 
   Sodium12, Fibre13, FFG4, FFG43, SSB, g, kJ,kJdisc, Protein, Carb, Fat, Added_sugar, Free_sugar, 
   SatFat, Sodium, Fibre)

#----Add short labels (better fit on graph)---
Nutrients <- Nutrients %>%
    left_join(select(Labels,short_label, code), by = c("Code" = "code")) %>%
    mutate(Label = coalesce(short_label, Label))%>% 
    select(-short_label)

#  Make "Beverages", "Discretionary status" & "Total" Class_lvls = "Major" Class_lvls
#  Make codes for these also:  "Beverages" = 40s, "Discretionary status" = 50s, "Total" = 1
DF_finyr <- Nutrients %>% 
        mutate(Code = case_when(
        Label == "Total sugar sweetened beverages" ~ 40, 
        Label == "Total intense sweetened and other non-alcoholic beverages(f)" ~ 41, 
        Label == "Total selected beverages" ~ 42, 
        Label == "Discretionary foods" ~ 50, 
        Label == "Non-discretionary foods" ~ 51, 
        Label == "Total foods and beverages(g)" ~ 1, 
        TRUE ~ Code)) %>% 
    mutate(Class_lvl = case_when(
        Class_lvl %in% c("Beverages", "Discretionary status", "Total")  ~ "Major", TRUE ~ Class_lvl ) ) %>% 
    mutate(Label = case_when(
        Label == "Total intense sweetened and other non-alcoholic beverages(f)" ~ "Total beverages excl. SSBs",
        Label == "Total foods and beverages(g)" ~ "Total foods and beverages",
        TRUE ~ Label)) 


#------------------------------
#----Months tables 
#--- grams (1.2)
#--- kJ  (5.3)
#------------------------------

#---Tidy long, fix names
g12 <- g2 %>%
    drop_na() %>%
    rename(Foodgroup = `Food group(c)`, Code = `Food group code`, Class_lvl = `Classification level` ) %>% 
    gather("Month", "Value", 4:27) %>% 
    mutate(Code = as.numeric(Code), Value = as.numeric(Value), Nutrient = "Weight", Unit = "g")

g53 <- kj53 %>%
    drop_na() %>%
    rename(Foodgroup = `Food group(c)`, Code = `Food group code`, Class_lvl = `Classification level` ) %>% 
    gather("Month", "Value", 4:27) %>% 
    mutate(Code = as.numeric(Code), Value = as.numeric(Value), Nutrient = "Energy", Unit = "kJ")

#---Append, make Codes & Class_lvl for the 'summary' level foodgroups ---
mth <- g12 %>% 
    bind_rows(g53) %>% 
    mutate(Code = case_when(
        Foodgroup == "Total sugar sweetened beverages" ~ 40, 
        Foodgroup == "Total intense sweetened and other non-alcoholic beverages(f)" ~ 41, 
        Foodgroup == "Total selected beverages" ~ 42, 
        Foodgroup == "Discretionary foods" ~ 50, 
        Foodgroup == "Non-discretionary foods" ~ 51, 
        Foodgroup == "Total foods and beverages(g)" ~ 1, 
        TRUE ~ Code)) %>% 
    filter(Code !=29) %>% 
    mutate(Class_lvl = case_when(
        Class_lvl %in% c("Beverages", "Discretionary status", "Total")  ~ "Major", TRUE ~ Class_lvl ) ) %>% 
    mutate(Foodgroup = case_when(
        Foodgroup == "Total intense sweetened and other non-alcoholic beverages(f)" ~ "Total beverages excl. SSBs",
        Foodgroup == "Total foods and beverages(g)" ~ "Total foods and beverages",
        TRUE ~ Foodgroup)) 
    
#----Add short labels ---
mth <- mth %>%
    left_join(select(Labels,short_label, code), by = c("Code" = "code")) %>%
    mutate(Label = coalesce(short_label, Foodgroup)) %>% 
    select(1,2,Label, Month, Nutrient, Unit, Value)

#---Fix Months---
mth <- mth %>% 
    mutate(Month = case_when(
        Month ==  "Jul 2018(d)"  ~     "Jul-01-2018",
        Month ==  "Aug 2018(d)"  ~    "Aug-01-2018",
        Month ==  "Sep 2018(d)"  ~    "Sep-01-2018" ,
        Month ==  "Oct 2018(d)"  ~    "Oct-01-2018",
        Month ==  "Nov 2018(d)"  ~    "Nov-01-2018",
        Month ==  "Dec 2018(d)"  ~   "Dec-01-2018",
        Month ==  "Jan 2019(d)"  ~   "Jan-01-2019",
        Month ==  "Feb 2019(d)"  ~    "Feb-01-2019" , 
        Month ==  "Mar 2019(d)"  ~    "Mar-01-2019",
        Month ==  "Apr 2019(d)"  ~    "Apr-01-2019"  ,
        Month ==  "May 2019(d)"  ~   "May-01-2019" ,
        Month ==  "Jun 2019(d)"  ~    "Jun-01-2019" ,
        Month ==  "Jul 2019"     ~    "Jul-01-2019", 
        Month ==  "Aug 2019"     ~    "Aug-01-2019", 
        Month ==  "Sep 2019"     ~    "Sep-01-2019",
        Month ==  "Oct 2019"     ~    "Oct-01-2019" ,
        Month ==  "Nov 2019"     ~    "Nov-01-2019", 
        Month ==  "Dec 2019"     ~    "Dec-01-2019",
        Month ==  "Jan 2020"     ~    "Jan-01-2020" ,
        Month ==  "Feb 2020"     ~   "Feb-01-2020" ,
        Month ==  "Mar 2020"     ~    "Mar-01-2020" ,
        Month ==  "Apr 2020"     ~   "Apr-01-2020" ,
        Month ==   "May 2020"    ~    "May-01-2020",
        Month ==  "Jun 2020"     ~    "Jun-01-2020" 
    )) %>% 
    mutate(Month = mdy(Month)) 

#---Make pc and clabel---
DF_mth <- mth %>% 
    mutate(period = "Month") %>% 
    group_by(Month, Nutrient, Unit) %>% 
    mutate(pc = round(Value/max(Value)*100,1)) %>% 
    ungroup() %>% 
    select(1:3,period, Month, Nutrient, Unit, Value, pc) 

rm(g12, g2, g53,  kj53, mth)

#---Bind DF_finyr to DF_mth, + concatenate Nutrient with unit,
#--- + make a column of Meastype of estimate (daily amount per capita | gross annual)

DF_fymth <- DF_finyr %>% 
    bind_rows(DF_mth) %>% 
    mutate(Nutrient = paste0(Nutrient, " (", Unit, ")")) %>% 
    mutate(Meastype = case_when(Unit == "Tonnes" ~ "gross annual", 
                                TRUE ~ "daily amount per capita")) %>% 
    ungroup() %>% 
  mutate(cLabel = paste0(Code, ", ", Label))


#---Make 2 columns: Major groups & Sub-major groups + make pc_maj (% within maj)
DF_fymth <- DF_fymth %>% 
  mutate(Maj = as.numeric(str_sub(Code,1,2)))%>%
  mutate(Sub = as.numeric(str_sub(Code,1,3))) %>% 
  left_join(select(Labels,short_label, code), by = c("Maj" = "code")) %>% 
  rename("Maj_label" = "short_label") %>% 
  mutate(`Major groups` = paste0(Maj,", ", Maj_label)) %>% 
  mutate(`Sub-major groups` = paste0(Sub,", ", Label)) %>% 
  group_by(`Major groups`, Month, Year, Nutrient, Unit) %>% 
  mutate(pc_maj = round(Value/max(Value)*100,1))
  
rm(DF_finyr, DF_mth)

#---make nutrient lists for UI ---
Nutmth <- as.list(c("Weight (g)", "Energy (kJ)"))

Nutyr <- as.list(c("Weight (g)", "Weight (Tonnes)", "Energy (kJ)", 
                   "Disc. Energy (of disc kJ) (kJ)", "Disc. Energy (of total kJ) (kJ)", 
                   "Carbohydrate (g)", "Protein (g)", "Total fat (g)", 
                   "Added sugar (g)", "Free sugar (g)", "Saturated fat (g)", 
                   "Sodium (mg)", "Dietary fibre (g)"))


#----Make a food groups (as list) for UI ----
DF_fymth <- DF_fymth %>%
    mutate(cLabel = paste0(Code,", ", Label))

fd02 <- DF_fymth %>%
    filter(Class_lvl == "Major") %>%
    group_by(cLabel) %>%
    tally()

fd02 <- as.list(fd02$cLabel)

fd03 <- DF_fymth %>%
    filter(Class_lvl == "Sub-major") %>%
    group_by(cLabel) %>%
    tally()

fd03 <- as.list(fd03$cLabel)

rm(Nutrients)

#---make Maj & Sub-maj list for  UI (Month & nutgrp)---
mthmaj <- DF_fymth %>% 
  filter(Code %!in% c(1,40,41,42,50,51)) %>% 
  group_by(`Major groups`) %>%
  tally()

mthmaj <- as.list(mthmaj$`Major groups`)

mthsub <- DF_fymth %>% 
  filter(Code %!in% c(1,40,41,42,50,51)) %>% 
  group_by(`Sub-major groups`) %>%
  tally()

mthsub <- as.list(mthsub$`Sub-major groups`)


# ------Make categoricals into factors-------------
DF_fymth$Class_lvl <- as.factor(DF_fymth$Class_lvl)
#DF_fymth$Code <- as.factor(DF_fymth$Code)
#DF_fymth$Unit <- as.factor(DF_fymth$Unit)
DF_fymth$Label <- as.factor(DF_fymth$Label)


#--- Get start/end months
m <- DF_fymth %>% 
    select(-Year) %>% 
    filter(period == "Month") 

dmax <- max(m$Month)
dmin <- min(m$Month)

#---Today's date 
now <- format(today(),"%d %B %Y")


#=====================================================================
# SHINY DASHBOARD Hchart
#=====================================================================
#=====================================================================
#== Shiny App
library(shiny)
library(shinydashboard)
library(shinyWidgets)
#=====================================================================

#---Colours---
abscol <- c("#336699", "#669966", "#99CC66", "#993366", "#CC9966", "#666666", 
            "#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462",
            "#B3DE69", "#FCCDE5","#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F")
#=====================================================================

ui <- fluidPage(
    
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    
    headerPanel("Apparent Consumption of Selected Foodstuffs, Australia, 2019-20"),
    
    sidebarPanel(
      radioButtons ("choosetype", "Explore:",
                    choices = c("Gram, kJ or nutrient contribution from major & sub-major food groups" = "nutgrp", 
                                "Specific food groups for gram, kJ or nutrient contribution" = "foodgrp",
                                "Australian Dietary Guidelines food groups" = "ADGgrp",
                                "Selected non-alcoholic beverages" = "bevgrp"),
                    selected = c("foodgrp")),    
        
      radioButtons ("choosetime", "Select time aggregate:",
                      choices = c("Annual" = "Finyr", "Monthly" = "Month"),
                      selected = c("Finyr"),inline = TRUE),
      
  ## Where choosetime = Month
      conditionalPanel(
        condition = "input.choosetime == 'Month'",
        
        sliderInput("dateRange","Select start-end months:",
                    min = as.Date(dmin),
                    max = as.Date(dmax),
                    value=as.Date(c(dmin, dmax)),
                    step = 5,
                    timeFormat="%b %Y"),
        
        
        selectInput("Nut_mth", "Select grams or kJ", 
                    choices = c(Nutmth), selected = "Weight (g)")),
  ## end conditional Month
      
  ## Where choosetime = Finyr
      conditionalPanel(
        condition = "input.choosetime == 'Finyr'",
        
        checkboxGroupInput("finyr", "Select years:" , choices= c("2018-19", "2019-20"), selected = c("2018-19", "2019-20"), inline = TRUE), 
        
        
        selectInput("Nut_yr", "Select measure: tonnes, grams, kJ or selected nutrients:", 
                    choices = c(Nutyr), selected = "Weight (g)")),
  ## end conditional Finyr
        
  ## Where choosetype = foodgrp
        conditionalPanel(
            condition = "input.choosetype == 'foodgrp'",
        
        
        pickerInput("Majgrp", "Select from AUSNUT Major food groups:", choices = c(fd02), 
                    selected = "1, Total foods and beverages", multiple = TRUE),
        
         pickerInput("Mingrp", "Select from AUSNUT Minor food groups:", choices = c(fd03),  multiple = TRUE)), 
  ##end conditional foodgrp  
      
    
  ## Where choosetype = nutgrp + choosetime = Finyr
       conditionalPanel(
           condition = "input.choosetype == 'nutgrp' & input.choosetime == 'Finyr'",
           radioButtons ("Class_lvl", "Classification level:", choices= c("Major", "Sub-major"),
                         selected = c("Major"),inline = TRUE) 
           ),
  ## end conditional nutgrp + Finyr
        
  ## Where choosetype = nutgrp + choosetime = Month
  conditionalPanel(
    condition = "input.choosetype == 'nutgrp' & input.choosetime == 'Month'",
    selectInput("mthmaj", "Select Major food group:", choices = c(mthmaj), selected = "11, Non-alcoholic beverages" )
    ),
  ## end conditional nutgrp + Month
  
  
      radioButtons ("meas", "View as:", choices= c("Amount" = "Val", "Percent" = "pc"),
                  selected = c("Val"),inline = TRUE),    
  
  conditionalPanel(
    condition = "input.choosetype == 'nutgrp' & input.choosetime == 'Month' & input$meas == 'pc'",  
  radioButtons("pctype", "Percent of:", choices = c("Total foods", "Major group"), selected = "Total foods")
  ),
  
  downloadButton("downloadTb", "Download graph/table selection:"),
    
    # adding the div tag to the sidepanel
    
    tags$div(class="header", checked=NA, br(),
             tags$strong("Notes:")),
    tags$div(class="header", checked=NA,
             tags$a(href="https://www.abs.gov.au/statistics/health/health-conditions-and-risks/apparent-consumption-selected-foodstuffs-australia/2019-20",
                    target="_blank", "Source: ABS Apparent Consumption of Selected Foodstuffs, Australia, 2019-20"), 
             br(),
             tags$a(href="https://www.abs.gov.au/statistics/health/health-conditions-and-risks/apparent-consumption-selected-foodstuffs-australia/2019-20/4316DO001_201920.xls",
                    "Download the published data tables (xls)"),
             br(),
             tags$a(href="https://github.com/Atyepa/Labour-Force-Australia-ShinyApp/blob/main/LFS_tseries.R#L123",
                    target="_blank", "See the ShinyApp code at GitHub"))
    ),
    mainPanel(
        
        tabsetPanel(type = "tabs",
                    tabPanel("Graph", highchartOutput("hcontainer",height = "720px")),
                    tabPanel("Table", verbatimTextOutput("message"), DT::dataTableOutput("table"))
        ),
        
        # adding the div tag to the mainpanel
        tags$div(class="header", checked=NA,
                 tags$p("Source: ABS Apparent Consumption of Selected Foodstuffs, Australia, 2019-20"),
                 tags$p(paste0("Data retrieved from ABS, ", now))
               
    )))


#==========================================
# Server
#=========================================
server <- function(input, output) {  
    
    
    Nm <- reactive({
        list (N_mth =input$Nut_mth) })  
    
    Ny <- reactive({
        list (N_yr =input$Nut_yr) })  
    
    Fy <- reactive({
        list (finyr =input$finyr) })
    
    p <- reactive({
        list (choosetime =input$choosetime) })  
    
    M <-  reactive({
        list (Majgrp =input$Majgrp) })  
    
    m <-  reactive({
        list (Mingrp =input$Mingrp) })
    
    C <-  reactive({
      list (Class_lvl =input$Class_lvl) }) 
    
    Maj <-  reactive({
      list (mthmaj =input$mthmaj) })
    
   
# Units for month     
    Um <- reactive({ 
            DF_fymth %>%
            filter(Nutrient %in% Nm()$N_mth) %>%
            group_by(Nutrient) %>%
            summarise(Unit = max(Unit),.groups = 'drop') %>% 
            ungroup() %>% 
            select(Unit) %>% 
            group_by(Unit) %>% 
            summarise(Unit = max(Unit),.groups = 'drop')
        })
   
# Units for year         
    Uy <- reactive({ 
            DF_fymth %>%
            filter(Nutrient %in% Ny()$N_yr) %>%
            group_by(Nutrient) %>%
            summarise(Unit = max(Unit),.groups = 'drop') %>% 
            ungroup() %>% 
            select(Unit) %>% 
            group_by(Unit) %>% 
            summarise(Unit = max(Unit),.groups = 'drop')
            })
        
# Subset df into finyr (foodgrp)
        DF_fy <- reactive({ DF_fymth %>%
            filter(Nutrient %in% Ny()$N_yr)%>%
            filter(period == p()$choosetime) %>% 
            filter(Year %in% Fy()$finyr) %>%     
            filter(cLabel %in% M()$Majgrp | cLabel %in% m()$Mingrp ) 
    })
    
# Subset df into finyr (nutgrp)
        NUT_fy <- reactive({ DF_fymth %>%
            filter(Nutrient %in% Ny()$N_yr)%>%
            filter(period == p()$choosetime) %>% 
            filter(Year %in% Fy()$finyr) %>%     
            filter(Class_lvl == C()$Class_lvl) %>% 
            filter(Code %!in% c(1, 40, 41, 42, 50, 51)) %>% 
            group_by(Code) %>% 
            mutate(avpc = mean(pc))
        })
        
# Subset df into mth (nutgrp)
        NUT_mth <- reactive({ DF_fymth %>%
            filter(Class_lvl == "Sub-major") %>% 
            filter(Nutrient %in% Nm()$N_mth)%>%
            filter(`Major groups` %in% Maj()$mthmaj) %>% 
            filter(period == p()$choosetime) %>% 
            group_by(Code, period, Nutrient) %>% 
            mutate(nxt = if_else(Month< max(Month), lead(Month), Month))%>%
            filter(nxt >= input$dateRange[1]) %>%  
            filter(Month <= input$dateRange[2]) %>%  
            group_by(Code) %>% 
            mutate(Month = format(Month, "%b %Y")) %>% 
            mutate(avpc = round(mean(pc),1)) %>% 
            ungroup() %>% 
            select(Class_lvl, Code, Maj_label, `Sub-major groups`, Nutrient, Month, Value, pc, pc_maj, avpc) 
        }) 

# Subset df into mth (foodgrp)      
        DF_mth <- reactive({ DF_fymth %>%
                filter(Nutrient %in% Nm()$N_mth)%>%
                filter(period == p()$choosetime) %>% 
                filter(cLabel %in% M()$Majgrp | cLabel %in% m()$Mingrp ) %>% 
                group_by(Code, period, Nutrient) %>% 
                mutate(nxt = if_else(Month< max(Month), lead(Month), Month))%>%
                filter(nxt >= input$dateRange[1]) %>%  
                filter(Month <= input$dateRange[2]) %>%  
                ungroup() %>% 
                mutate(Month = format(Month, "%b %Y"))
        })
        
# Clean dfs for table display/xlsx (foodgrp)
        tab_fy <- reactive({ DF_fymth %>%
                filter(Nutrient %in% Ny()$N_yr)%>%
                filter(period == p()$choosetime) %>% 
                filter(cLabel %in% M()$Majgrp | cLabel %in% m()$Mingrp ) %>% 
                        pivot_wider(names_from = Year, values_from = c("Value", "pc")) %>%
                        select(Class_lvl, Code, Label, Nutrient, `Value_2018-19`, `Value_2019-20`, `pc_2018-19`, `pc_2019-20` ) %>%
                        rename(`AUSNUT classification level` = Class_lvl, `Food code` = Code,
                            `Value, 2018-19` = `Value_2018-19`,  `Value 2019-20` = `Value_2019-20`,
                               `%, 2018-19` = `pc_2018-19`, `%, 2019-20` = `pc_2019-20`)
                })
            
        
        tab_mth <- reactive({ DF_mth() %>% 
                select(Class_lvl, Code, Label, Month, Nutrient, Value, pc, pc_maj) %>% 
                rename(`AUSNUT classification level` = Class_lvl, `Food code` = Code, `% of total` = pc, `% of Major group` = pc_maj)
        })
        

        tab_nut_mth <- reactive({ NUT_mth() %>% 
            rename(`%` = pc, `Major group` = Maj_label, ) %>% 
            select(`Major group`, `Sub-major groups`, Month, Nutrient, Value, `%`)})
        
        
# Clean dfs for table display/xlsx (nutgrp)
        tab_nut_fy <- reactive({ DF_fymth %>%
            filter(Nutrient %in% Ny()$N_yr)%>%
            filter(period == p()$choosetime) %>% 
            filter(Class_lvl == C()$Class_lvl) %>% 
            filter(Code %!in% c(1, 40, 41, 42, 50, 51)) %>% 
            arrange(desc(Value)) %>% 
            pivot_wider(names_from = Year, values_from = c("Value", "pc")) %>%
            select(Class_lvl, Code, Label, Nutrient, `Value_2018-19`, `Value_2019-20`, `pc_2018-19`, `pc_2019-20` ) %>%
            rename(`AUSNUT classification level` = Class_lvl, `Food code` = Code,
                   `Value, 2018-19` = `Value_2018-19`,  `Value 2019-20` = `Value_2019-20`,
                   `%, 2018-19` = `pc_2018-19`, `%, 2019-20` = `pc_2019-20`)
        })
        
                
    # Output table for download
       tabi <- reactive ({
            if(input$choosetime == "Month" & input$choosetype == "foodgrp") {tab_mth()}
            if (input$choosetime == "Finyr" & input$choosetype == "foodgrp") {tab_fy()}
            if (input$choosetime == "Finyr" & input$choosetype == "nutgrp") {tab_nut_fy()} 
         
        })
        
       
      output$table = DT::renderDataTable({
            
        if(input$choosetime == "Finyr" & input$choosetype == "foodgrp" ) {
            tab <- tab_fy()   }
                
        if(input$choosetime == "Month" & input$choosetype == "foodgrp") {
        tab <- tab_mth()  }
        
        if(input$choosetime == "Finyr" & input$choosetype == "nutgrp" ) {
          tab <- tab_nut_fy() }
        
        
        if(input$choosetime == "Month" & input$choosetype == "nutgrp" ) {
          tab <- tab_nut_mth() }
        
        
        tab 
        })
             
        
    output$hcontainer <- renderHighchart({
        
        if(input$meas=="pc" & input$choosetime == "Finyr") {
            hc <- DF_fy() %>% 
                arrange (desc (pc)) %>%
                hchart(.,
                       type = "column",
                       hcaes(x = Label,
                             y = pc,
                             group = paste0(Nutrient, ", ", Year))) %>%
                hc_xAxis(title = list(text = paste0("Selected food groups"))) %>%
                hc_yAxis(title = list(text = paste0(" % of ", input$Nut_yr))) %>% 
                hc_colors(abscol)
            
        }
        
        
    if(input$meas=="Val" & input$choosetime == "Finyr") {
            hc <- DF_fy() %>% 
                arrange (desc (pc)) %>%
                hchart(.,
                       type = "column",
                       hcaes(x = Label,
                             y = Value,
                             group = paste0(Nutrient, ", ", Year))) %>%
                hc_xAxis(title = list(text = paste0("Selected food groups"))) %>% 
                hc_yAxis(title = list(text = paste0(Uy()$Unit))) %>% 
                hc_colors(abscol)
            
        }
        
        
        if(input$meas=="pc" & input$choosetime == "Month") {
            hc <- DF_mth() %>% 
                       hchart(.,
                       type = "line",
                       hcaes(x = Month,
                             y = pc,
                             group = paste0(Label, ", ", Nutrient))) %>%
                hc_xAxis(title = list(text = paste0("Month"))) %>%
                hc_yAxis(title = list(text = paste0(" % of ", input$Nut_mth))) %>% 
                hc_colors(abscol)
            
        }        
        
        
        if(input$meas=="Val" & input$choosetime == "Month") {
            hc <- DF_mth() %>% 
                hchart(.,
                       type = "line",
                       hcaes(x = Month,
                             y = Value,
                             group = paste0(Label, ", ", Nutrient))) %>%
                hc_xAxis(title = list(text = paste0("Month"))) %>%
                hc_yAxis(title = list(text = paste0(Um()$Unit))) %>% 
                hc_colors(abscol)
            
        }        
        
      
      if(input$meas=="Val" & input$choosetime == "Finyr" & input$choosetype == "nutgrp") {
        hc <- NUT_fy() %>% 
          arrange (desc (pc)) %>%
          filter(avpc > 1) %>% 
          hchart(.,
                 type = "bar",
                 hcaes(x = Label,
                       y = Value,
                       group = paste0(Nutrient, ", ", Year))) %>%
          hc_xAxis(title = list(text = paste0(input$Class_lvl, " food groups"))) %>% 
          hc_yAxis(title = list(text = paste0(Uy()$Unit))) %>% 
          hc_title(text = paste0("Daily per capita consumption of ", input$Nut_yr, ", selected ", input$Class_lvl, " food groups")) %>% 
          hc_colors(abscol)
      }
        
      if(input$meas=="pc" & input$choosetime == "Finyr" & input$choosetype == "nutgrp") {
        hc <- NUT_fy() %>% 
          arrange (desc (pc)) %>%
          filter(avpc > 1) %>% 
          hchart(.,
                 type = "bar",
                 hcaes(x = Label,
                       y = pc,
                       group = paste0(Nutrient, ", ", Year))) %>%
          hc_xAxis(title = list(text = paste0(input$Class_lvl, " food groups"))) %>% 
          hc_yAxis(title = list(text = paste0("% of ", input$Nut_yr))) %>% 
          hc_title(text = paste0("Proportion of total ", input$Nut_yr, ", from selected ", input$Class_lvl, " food groups")) %>% 
          hc_colors(abscol)
      }
      
      
      if(input$meas=="Val" & input$choosetime == "Month" & input$choosetype == "nutgrp") {
        hc <- NUT_mth() %>% 
          hchart(.,
                 type = "line",
                 hcaes(x = Month,
                       y = Value,
                       group = `Sub-major groups`)) %>%
          hc_xAxis(title = list(text = paste0(input$Class_lvl, " food groups"))) %>% 
          hc_yAxis(title = list(text = paste0(Um()$Unit))) %>% 
          hc_title(text = paste0("Daily per capita consumption of sub-major foods within ", input$mthmaj, ", by month")) %>% 
        hc_colors(abscol)
    }
    
      
      if(input$meas=="pc" & input$choosetime == "Month" & input$choosetype == "nutgrp" & input$pctype == "Total foods") {
        hc <- NUT_mth() %>% 
                hchart(.,
                 type = "line",
                 hcaes(x = Month,
                       y = pc,
                       group = `Sub-major groups`)) %>%
          hc_yAxis(title = list(text = paste0("% of total foods"))) %>% 
          hc_title(text = paste0("Average daily per capita consumption of ", input$mthmaj, ", by month")) %>% 
          hc_colors(abscol)
      }
      
      if(input$meas=="pc" & input$choosetime == "Month" & input$choosetype == "nutgrp" & input$pctype == "Major group") {
        hc <- NUT_mth() %>% 
                hchart(.,
                 type = "line",
                 hcaes(x = Month,
                       y = pc_maj,
                       group = `Sub-major groups`)) %>%
          hc_yAxis(title = list(text = paste0("% of Major group"))) %>% 
          hc_title(text = paste0("Average daily per capita consumption of ", input$mthmaj, ", by month")) %>% 
          hc_colors(abscol)
      }
      
      
      
      
      
        hc
        
    })
    # Downloadable xlsx --
    output$downloadTb <- downloadHandler(
        filename = function() { paste0("ACSF19-20, ", input$choosetime,", ", input$Nut_yr, ", selected groups", ".xlsx") },
        content = function(file) { write_xlsx(tabi(), path = file) }
    )
}

#========================================  
shinyApp(ui, server)
#========================================
