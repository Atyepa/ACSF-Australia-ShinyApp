#===================================================================================================================
# DATA Visualisation modules for each table in Apparent Consumption of Selected Foodstuffs, Australia, 2020-21
# Provides detailed consumption of foods, the amounts of Nutrients from those foods, based on supermarket sales data. 
# Data source is Apparent Consumption of Selected Foodstuffs, Australia, 2020-21
# https://www.abs.gov.au/statistics/health/health-conditions-and-risks/apparent-consumption-selected-foodstuffs-australia/
#====================================================================================================================
library(tidyverse) 
library(openxlsx)
library(readxl)     
library(writexl)    
library(plotly)
library(highcharter)  
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(radiant.data)
library(ggthemes)
library(rsconnect)

#--- Preliminaries---

#-- Download datacube
daturl <- "https://www.abs.gov.au/statistics/health/health-conditions-and-risks/apparent-consumption-selected-foodstuffs-australia/2020-21/4316DO001_202021_ESTIMATES.xlsx"


download.file(daturl,"datacube.xlsx", mode = "wb" )

datapath <- "./datacube.xlsx"

options(warn=-1)

`%!in%` = Negate(`%in%`)

abscol <- c("#336699", "#669966", "#99CC66", "#993366", "#CC9966", "#666666", 
            "#8DD3C7",  "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69", 
            "#FCCDE5","#D9D9D9", "#BC80BD", "#CCEBC5", "#ffcc99")

#======================================================================================================

#---- Load latest Datacube from local directory  -------------------------

#---Annual aggregates---
iA1.1 <- read_excel(datapath, sheet = 2, range = "A7:I148")
iA2.1 <- read_excel(datapath, sheet = 4, range = "A8:E57")
iA3.1 <- read_excel(datapath, sheet = 6, range = "A8:G27")
iA3.2 <- read_excel(datapath, sheet = 7, range = "A6:AI44")
iA4.1 <- read_excel(datapath, sheet = 8, range = "A8:G28")
iA4.3 <- read_excel(datapath, sheet = 10, range = "A9:M87")
iA4.4 <- read_excel(datapath, sheet = 11, range = "A9:M87")
iA5.1 <- read_excel(datapath, sheet = 12, range = "A8:I148")
iA5.2 <- read_excel(datapath, sheet = 13, range = "A8:L148")
iA6.1 <- read_excel(datapath, sheet = 15, range = "A8:I148")
iA7.1 <- read_excel(datapath, sheet = 16, range = "A8:I148")
iA8.1 <- read_excel(datapath, sheet = 17, range = "A8:I148")
iA9.1 <- read_excel(datapath, sheet = 18, range = "A8:I148")
iA10.1 <- read_excel(datapath, sheet = 19, range = "A8:I148")
iA11.1 <- read_excel(datapath, sheet = 20, range = "A8:I148")
iA12.1 <- read_excel(datapath, sheet = 21, range = "A8:I148")
iA13.1 <- read_excel(datapath, sheet = 22, range = "A8:I148")
iA14.1 <- read_excel(datapath, sheet = 23, range = "A7:K189")


#--- Monthly aggregates---
iM1.2 <- read_excel(datapath, sheet = 3, range = "A7:AM147")
iM2.2 <- read_excel(datapath, sheet = 5, range = "A7:D1591")
iM3.2 <- read_excel(datapath, sheet = 7, range = "A6:AI44")
iM4.2 <- read_excel(datapath, sheet = 9, range = "A7:S45")
iM5.3 <- read_excel(datapath, sheet = 14, range = "A7:AM147") 
iM14.2 <- read_excel(datapath, sheet = 24, range = "A6:H6558") 

#---ADG subgroups groupings- 
compgrp <- read.xlsx("https://github.com/Atyepa/AUSNUT/raw/master/Compgrps.xlsx")

#=======================
#---DATA PREPARATION---
#=======================

#--------------------------
#--- Annual aggregates --  
#--------------------------

#--- Table 1.1: Quantity of foods available from AUSNUT food groups, gross annual tonnes and mean daily grams per capita
iA1.1names <- c("Class_level", "Code", "Label", "Tonnes,2018-19",  "Tonnes,2019-20", "Tonnes,2020-21", "g,2018-19",  "g,2019-20", "g,2020-21" )

iA1.1pcNames <- c("Class_level", "Code", "Label", "2018-19",  "2019-20", "2020-21")

# calc pc 
A1.1pc <- iA1.1 %>% 
   drop_na() %>% 
   select(1:6) %>% 
   setNames(iA1.1pcNames) %>% 
   pivot_longer(cols = 4:6, names_to = "Year", values_to = "Val") %>% 
   mutate(Val = as.double(Val)) %>% 
   group_by(Class_level, Year) %>%
   mutate(pc = round(Val/sum(Val)*100,1)) %>% 
   mutate(pc = case_when(Class_level == "Beverages" ~ pc *2, TRUE ~ pc)) %>% 
   select(-Val) %>% 
   rename(Val = pc) %>% 
   mutate(Nutrient = "Weight", Unit = "Pc") %>% 
   select(Class_level, Code, Label, Year, Unit, Val, Nutrient)

A1.1_ <- iA1.1 %>% 
   drop_na() %>% 
   setNames(iA1.1names) %>% 
   pivot_longer(cols = 4:9, names_to = "unityear", values_to = "Val") %>% 
   separate(unityear, into = c("Unit", "Year"), sep = ",", extra = "merge")%>% 
   mutate(Val = as.double(Val)) %>% 
   mutate(Nutrient = "Weight") %>%
   mutate(Code = factor(Code)) %>% 
   select(Class_level, Code, Label, Year, Unit, Val, Nutrient)


# Join Pc   
A1.1 <- A1.1_ %>% 
   bind_rows(A1.1pc) %>% 
   arrange(Unit, Year, Code)


#--- Table 2.1: Nutrients available, mean daily amount per capita
A2.1 <- iA2.1 %>% 
   drop_na() %>% 
   rename(`2018-19` = `2018-19(c)`) %>% 
   pivot_longer(3:5, names_to = "Year", values_to = "Val") %>% 
   mutate(Type = "All_nutrients")

#-- Table 3.1: Energy from macronutrients, mean daily kJ per capita and proportion of total energy
iA3.1names <- c("Nutrient", "kJ_2018-19", "kJ_2019-20", "kJ_2020-21",  "Pc_2018-19", "Pc_2019-20", "Pc_2020-21" )

A3.1 <- iA3.1 %>% 
   drop_na() %>% 
   setNames(iA3.1names) %>% 
   pivot_longer(2:7, names_to = "Year", values_to = "Val") %>% 
   separate(Year, into = c("Unit", "Year"), sep = "_", extra = "merge") %>% 
   mutate(Type = "Macronutrient_kJ")

#-- Table 4.1  Mean daily serves per capita, Australian Dietary Guidelines food groups
iA4.1names <- c("ADG_group", "Grams,2018-19", "Grams,2019-20", "Grams,2020-21",  "Serves,2018-19", "Serves,2019-20", "Serves,2020-21" )

A4.1 <- iA4.1 %>% 
   drop_na() %>% 
   setNames(iA4.1names)  %>% 
   mutate(nrow = 1, nrow = cumsum(nrow)) %>% 
   select(ADG_group, nrow, everything()) %>% 
   pivot_longer(3:8, names_to = "typ_yr", values_to = "Val") %>% 
   separate(typ_yr, c("Unit", "Year"), sep = ",") %>% 
   
   mutate(Nutrient = "ADG") %>% 
   
   mutate(Type = case_when(Unit == "Serves" ~ "ADG_serves", TRUE ~ "ADG_grams")) %>%  
   
   mutate(ADG_group = case_when(ADG_group == "Meats, poultry, fish, eggs, tofu, nuts and seeds and legumes/beans" ~ "Meats", 
                                ADG_group == "Milk, yoghurt, cheese and/or alternatives" ~ "Dairy",
                                ADG_group == "Grains and cereals" ~ "Grains",
                                ADG_group == "Unsaturated spreads and oils" ~ "UnsatFat", 
                                ADG_group == "Vegetables and legumes/beans" ~ "Veg", 
                                TRUE ~ ADG_group)) %>% 
   mutate(Disc_type = case_when(nrow < 7 ~ "NonDisc", 
                                nrow %in% 7:12 ~ "Disc", 
                                nrow > 12 ~ "Total", TRUE ~ "NA")) %>% 
   select(Nutrient, Unit, Year,  Val, Type, ADG_group, Disc_type)



#-- Table 4.3 & 4.4 ADG Sub food groups serves/grams  per capita
iA4.3names <- c("ADG_subgroup", "Grams,2018-19", "Grams,2019-20", "Grams,2020-21",   
                "% (g),2018-19", "% (g),2019-20", "% (g),2020-21", 
                "Serves,2018-19", "Serves,2019-20", "Serves,2020-21", 
                "% (Serves),2018-19", "% (Serves),2019-20", "% (Serves),2020-21" )

# Vector of ADG_group
ADG_group <- c("Grains", 	"Grains", 	"Grains", 	"Grains", 	"Grains", 	"Grains", 	"Grains", 	"Grains", 	"Grains", 	
               "Veg", 	"Veg", 	"Veg", 	"Veg", 	"Veg", 	"Veg", 
               "Fruit", 	"Fruit", 	"Fruit", 	"Fruit",
               "Dairy", 	"Dairy", 	"Dairy", 	"Dairy", 	"Dairy", 	"Dairy", 	"Dairy", 	"Dairy", 	"Dairy", 	"Dairy", 	"Dairy", 	"Dairy", 	"Dairy", 	"Dairy", 	"Dairy",
               "Meats", 	"Meats", 	"Meats", 	"Meats", 	"Meats", 	"Meats", 	"Meats", 	"Meats", 	"Meats", 	"Meats", 	"Meats", 	"Meats", 	"Meats", 	
               "UnsatFat", 	"UnsatFat", 	"UnsatFat", 	"UnsatFat")


A4.3 <- iA4.3 %>% 
   drop_na() %>% 
   setNames(iA4.3names) %>% 
   mutate(Disc_type = "NonDisc") %>% 
   mutate(ADG_group = ADG_group)

A4.4 <- iA4.4 %>% 
   drop_na() %>% 
   setNames(iA4.3names) %>% 
   mutate(Disc_type = "Total")%>% 
   mutate(ADG_group = ADG_group)

# Append 4.3 + 4.4, add ADG_group, tidy long
A4.3_4 <- A4.3 %>% 
   bind_rows(A4.4) %>% 
   mutate(`Grams,2018-19` = as.numeric(`Grams,2018-19`)) %>% 
   select(ADG_group, ADG_subgroup, Disc_type, everything()) %>% 
   pivot_longer(4:15, names_to = "names", values_to = "Val") %>% 
   separate(names, into = c("Unit", "Year"), sep = ",", extra = "merge") %>%  # Split names into Unit + Year   
   mutate(Type = case_when(Unit %in% c("Serves", "Grams") ~ "val",
                           Unit %in% c("% (g)", "% (Serves)") ~ "pc", TRUE ~ Unit)) %>% 
   mutate(Unit = case_when(Unit %in% c("Serves", "% (Serves)") ~ "Serves",
                           Unit %in% c("Grams", "% (g)") ~ "Grams", TRUE ~ Unit)) %>%
   
   mutate(Nutrient = "ADG_sub") %>% 
   select(Nutrient, Unit, Year, Val, Type, ADG_group, Disc_type, ADG_subgroup ) %>% 
   distinct()


#---Table 5.2: Discretionary energy - mean daily kJ per capita and proportion available from selected AUSNUT food groupings
iA5.2names <- c("Class_level", "Code", "Label", "kJ,2018-19",  "kJ,2019-20", "kJ,2020-21", "Pc,2018-19",  "Pc,2019-20", "Pc,2020-21" )

A5.2 <- iA5.2 %>% 
   drop_na() %>% 
   select(-7:-9) %>% 
   setNames(iA5.2names) %>% 
   mutate_at(4:9,as.numeric) %>% 
   pivot_longer(4:9, names_to = "Year", values_to = "Val") %>% 
   separate(Year, into = c("Unit", "Year"), sep = ",", extra = "merge") %>% 
   mutate(Nutrient = "Disc. Energy")


#--Tables 5.1 to 13.1---
iA5.1names <- c("Class_level", "Code", "Label", "kJ,2018-19",  "kJ,2019-20", "kJ,2020-21", "Pc,2018-19",  "Pc,2019-20", "Pc,2020-21" )

A5.1 <- iA5.1 %>% 
   drop_na() %>% 
   setNames(iA5.1names) %>% 
   mutate_at(4:9,as.numeric) %>% 
   pivot_longer(4:9, names_to = "Year", values_to = "Val") %>% 
   separate(Year, into = c("Unit", "Year"), sep = ",", extra = "merge") %>% 
   mutate(Nutrient = "Energy")


iA6_13names <- c("Class_level", "Code", "Label", "g,2018-19",  "g,2019-20", "g,2020-21", "Pc,2018-19",  "Pc,2019-20", "Pc,2020-21" )

A6.1 <- iA6.1 %>% 
   drop_na() %>% 
   setNames(iA6_13names) %>% 
   mutate_at(4:9,as.numeric) %>% 
   pivot_longer(4:9, names_to = "Year", values_to = "Val") %>% 
   separate(Year, into = c("Unit", "Year"), sep = ",", extra = "merge")%>% 
   mutate(Nutrient = "Protein")

A7.1 <- iA7.1 %>% 
   drop_na() %>% 
   setNames(iA6_13names) %>% 
   mutate_at(4:9,as.numeric) %>% 
   pivot_longer(4:9, names_to = "Year", values_to = "Val") %>% 
   separate(Year, into = c("Unit", "Year"), sep = ",", extra = "merge")%>% 
   mutate(Nutrient = "Carbohydrate")


A8.1 <- iA8.1 %>% 
   drop_na() %>% 
   setNames(iA6_13names) %>% 
   mutate_at(4:9,as.numeric) %>% 
   pivot_longer(4:9, names_to = "Year", values_to = "Val") %>% 
   separate(Year, into = c("Unit", "Year"), sep = ",", extra = "merge")%>% 
   mutate(Nutrient = "Total fat")

A9.1 <- iA9.1 %>% 
   drop_na() %>% 
   setNames(iA6_13names) %>% 
   mutate_at(4:9,as.numeric) %>% 
   pivot_longer(4:9, names_to = "Year", values_to = "Val") %>% 
   separate(Year, into = c("Unit", "Year"), sep = ",", extra = "merge")%>% 
   mutate(Nutrient = "Added sugars")

A10.1 <- iA10.1 %>% 
   drop_na() %>% 
   setNames(iA6_13names) %>% 
   mutate_at(4:9,as.numeric) %>% 
   pivot_longer(4:9, names_to = "Year", values_to = "Val") %>% 
   separate(Year, into = c("Unit", "Year"), sep = ",", extra = "merge")%>% 
   mutate(Nutrient = "Free sugars")

A11.1 <- iA11.1 %>% 
   drop_na() %>% 
   setNames(iA6_13names) %>% 
   mutate_at(4:9,as.numeric) %>% 
   pivot_longer(4:9, names_to = "Year", values_to = "Val") %>% 
   separate(Year, into = c("Unit", "Year"), sep = ",", extra = "merge")%>% 
   mutate(Nutrient = "Saturated fat")

A12.1 <- iA12.1 %>% 
   drop_na() %>% 
   setNames(iA6_13names) %>% 
   mutate_at(4:9,as.numeric) %>% 
   pivot_longer(4:9, names_to = "Year", values_to = "Val") %>% 
   separate(Year, into = c("Unit", "Year"), sep = ",", extra = "merge")%>% 
   mutate(Nutrient = "Sodium")

A13.1 <- iA13.1 %>% 
   drop_na() %>% 
   setNames(iA6_13names) %>% 
   mutate_at(4:9,as.numeric) %>% 
   pivot_longer(4:9, names_to = "Year", values_to = "Val") %>% 
   separate(Year, into = c("Unit", "Year"), sep = ",", extra = "merge")%>% 
   mutate(Nutrient = "Dietary fibre")

# Table 14.1: Selected non-alcoholic beverages, amount available, energy, added sugars and free sugars
iA14.1names <- c("Class_order", "Beverage_group", "Measure",  "Unit", "Unit_type", 
                 "Amt,2018-19",  "Amt,2019-20", "Amt,2020-21", "Pc,2018-19",  "Pc,2019-20", "Pc,2020-21" )

A14.1 <- iA14.1 %>% 
   drop_na() %>% 
   setNames(iA14.1names) %>% 
   mutate_at(6:11,as.numeric) %>% 
   pivot_longer(6:11, names_to = "Year", values_to = "Val") %>% 
   separate(Year, into = c("Val_type", "Year"), sep = ",", extra = "merge")


#-----------------------
#-- Monthly aggregates   
#-----------------------
# Table 1.2 - (grams monthly)
M1.2 <- iM1.2 %>% 
   drop_na() %>% 
   rename("Class_level" = "Classification level",   "Code" =  "Food group code", "Label" = "Food group(c)" ) %>% 
   pivot_longer(cols = 4:39, names_to = "mth", values_to = "Val")%>% 
   mutate(Val = as.double(Val)) %>% 
   mutate(Nutrient = "Weight", Unit = "g") %>%
   mutate(Date = str_sub(mth, 1,8)) %>%        # Clean footnotes out, make into date   
   mutate(Date = dmy(paste0("01 ",Date))) %>% 
   select(Class_level, Code, Label, Date, Unit, Val, Nutrient) 

# Table 2.2 - (nutrients monthly)
M2.2 <- iM2.2 %>% 
   drop_na() %>% 
   rename(Date = `Month(b)`, Val = `Mean daily per capita(c)`) %>% 
   mutate(Date = dmy(paste0("01 ",Date))) %>% 
   mutate(Type = "All_nutrients") %>% 
   mutate(Nutrient = case_when(Nutrient == "Total long chain omega 3 fatty acids" ~ "LC omega-3 fatty acids", 
                               Nutrient == "Total Fat" ~ "Total fat", 
                               TRUE ~ Nutrient))

# Table 3.2 - Energy from macronutrients  (as.Date(42705, origin = "1899-12-30"))
im3.2names <- c("Date",
                "Total energy_kJ", 	"Protein_kJ", 	"Total fat_kJ", 	"Saturated fat_kJ", 	"Monounsaturated fat_kJ", 	"Polyunsaturated fat_kJ", 
                "Linoleic acid_kJ", 	"Alpha-Linolenic acid_kJ", 	"LC omega-3 fatty acids_kJ", 	"Trans fatty acids_kJ", 	"Carbohydrate_kJ", 
                "Total sugars_kJ", 	"Free sugars_kJ", 	"Added sugars_kJ", 	"Starch_kJ", 	"Dietary Fibre_kJ", 	"Alcohol_kJ", 
                "Total energy_%", "Protein_%", 	"Total fat_%", 	"Saturated fat_%", 	"Monounsaturated fat_%", 	"Polyunsaturated fat_%", 
                "Linoleic acid_%", 	"Alpha-Linolenic acid_%", "LC omega-3 fatty acids_%", "Trans fatty acids_%", 	"Carbohydrate_%", 
                "Total sugars_%", 	"Free sugars_%", "Added sugars_%", 	"Starch_%", 	"Dietary Fibre_%", 	"Alcohol_%" )

M3.2 <- iM3.2 %>% 
   drop_na() %>%  
   setNames(im3.2names) %>% 
   mutate(Date = as.Date(as.integer(Date), origin = "1899-12-30")) %>% 
   pivot_longer(2:35, names_to = "Nutrients", values_to = "Val") %>% 
   mutate(Val = as.numeric(Val)) %>% 
   separate(Nutrients, into = c("Nutrient", "Unit"), sep = "_", extra = "merge") %>% 
   mutate(Type = "Macronutrient_kJ")

# Table 4.2 - Mean daily serves, ADGs 
iM4.2names <- c("Date", "Grains_NonDisc", 	"Grains_Disc", 	"Grains_Total", 	"Veg_NonDisc", 	"Veg_Disc", 	"Veg_Total", 
                "Fruit_NonDisc", 	"Fruit_Disc", 	"Fruit_Total", 	"Dairy_NonDisc", 	"Dairy_Disc", 	"Dairy_Total", 
                "Meats_NonDisc", 	"Meats_Disc", 	"Meats_Total", 	"UnsatFat_NonDisc", 	"UnsatFat_Disc", 	"UnsatFat_Total")

M4.2 <- iM4.2 %>% 
   drop_na() %>% 
   setNames(iM4.2names) %>% 
   mutate(row = 1, row = cumsum(row)) %>% 
   filter(row>1) %>% 
   select(-row) %>% 
   mutate(Date = as.Date(as.integer(Date), origin = "1899-12-30")) %>% 
   pivot_longer(2:19, names_to = "ADG_groups", values_to = "Val") %>% 
   mutate(Val = as.numeric(Val)) %>% 
   separate(ADG_groups, into = c("ADG_group", "Disc_type"), sep = "_", extra = "merge") %>% 
   mutate(Unit = "Serves", Nutrient = "ADG", Type = "ADG_serves") %>%
   select(Date, Nutrient, Unit, Val, Type, ADG_group, Disc_type) 


# Table 5.3 - Energy - mean daily kJ per capita available from selected AUSNUT food groupings
M5.3 <- iM5.3 %>% 
   drop_na() %>% 
   rename("Class_level" = "Classification level",   "Code" =  "Food group code", "Label" = "Food group(c)" ) %>% 
   pivot_longer(cols = 4:39, names_to = "mth", values_to = "Val")%>% 
   mutate(Val = as.double(Val)) %>% 
   mutate(Nutrient = "Energy", Unit = "kJ") %>%
   mutate(Date = str_sub(mth, 1,8)) %>%        # Clean footnotes out, make into date   
   mutate(Date = dmy(paste0("01 ",Date))) %>% 
   select(Class_level, Code, Label, Date, Unit, Val, Nutrient) 


# Table 14.2 - Selected non-alcoholic beverages, amount available, energy, added sugars etc
iM14.2names <- c("Date", "Class_order", "Beverage_group", "Measure", "Unit", "Unit_type", "Amt", "Pc")

M14.2 <- iM14.2 %>% 
   drop_na() %>% 
   setNames(iM14.2names) %>%  
   mutate(Unit = case_when(Unit == "Grams" ~ "g", TRUE ~ Unit)) %>% 
   mutate(Date = as.Date(Date)) 

#---------------------
# Clean out input data
#---------------------

rm(iA1.1, 	iA2.1, 	iA3.1, 	iA3.2, 	iA4.1, 	iA5.1, 	iA5.2, 	iA6.1, iA7.1, 	iA8.1, 	iA9.1, 	
   iA10.1,	iA11.1,	iA12.1,	iA13.1,	iA14.1,	iM1.2, 	iM2.2, 	iM3.2, 	iM4.2, 	iM5.3, 	iM14.2)

#-----------------------------------------------
#--- Assemble tables with common structures ---
#-----------------------------------------------

# Table 1.1, 5.1, 5.2, 6.1 to 13.1
A_Ausnut <- A1.1 %>% 
   bind_rows(A5.1, A5.2, A6.1, A7.1, A8.1, A9.1, A10.1, A11.1, A12.1, A13.1) %>% 
   mutate(Label = str_remove(Label, ("\\(g\\)"))) %>% 
   mutate(Label = str_remove(Label, ("\\(f\\)"))) %>% 
   mutate(Code = str_remove(Code, "n.a.")) %>% 
   mutate(cLabel = paste0(Code, " ", Label)) %>%
   mutate(Nutrient = str_remove(Nutrient, ("\\(e\\)"))) %>%    # <- Fix name variations by removing certain strings e.g. (e) (g) (d) (f)
   mutate(Nutrient = str_remove(Nutrient, ("\\(g\\)"))) %>%
   mutate(Nutrient = str_remove(Nutrient, ("\\(d\\)"))) %>% 
   mutate(cLabel = case_when(Class_level == "Total" ~ "Total foods and beverages", TRUE ~ cLabel)) %>% 
   mutate(Label = case_when(Class_level == "Total" ~ "Total foods and beverages", TRUE ~ Label)) %>% 
   mutate(Nutrient = case_when(Nutrient == "Weight" & Unit == "Tonnes" ~ "Weight (gross, annual)", TRUE ~ Nutrient)) %>% 
   mutate(Unit = case_when(Unit == "Pc" ~ "percent", TRUE ~ Unit))%>% 
   mutate(Val_pc = case_when(Unit == "percent" ~ "%", TRUE ~ "Value"))


# Tables 2.1+ 3.1 + 4.1 + 4.3 + 4.4
A_nut <- A2.1 %>% 
   bind_rows(A3.1,A4.1) %>% 
   mutate(Nutrient = str_remove_all(Nutrient, ("\\(d\\)"))) %>%  # <- Fix naming variations by removing certain strings e.g. (e) (g) (d) 
   mutate(Nutrient = str_remove_all(Nutrient, ("\\(g\\)"))) %>%
   mutate(Nutrient = str_remove_all(Nutrient, ("\\(f\\)"))) %>%
   mutate(Nutrient = str_remove_all(Nutrient, ("\\(e\\)"))) %>%  
   mutate(Year = str_remove_all(Year, ("\\(c\\)"))) %>%  
   mutate(Nutrient = case_when(Nutrient == "Free sugar" ~ "Free sugars", 
                               Nutrient == "Added sugar" ~ "Added sugars", 
                               Nutrient == "Total Fat" ~ "Total fat", 
                               Nutrient == "Total long chain omega 3 fatty acids" ~ "LC omega-3 fatty acids", 
                               TRUE ~ Nutrient)) %>%
   bind_rows(A4.3_4)

# Join labels for ADG comparable  + make Unit_label
A_nut <- A_nut %>% 
   left_join(select(compgrp,3:4), by = c("ADG_subgroup")) %>%
   rename(Comp_grp = compgrp_name) %>% 
   mutate(Unit_label = case_when(Unit == "Pc" ~ "%", 
                                 Type == "pc" & Unit == "Grams" ~ "% (grams)",
                                 Type == "pc" & Unit == "Serves" ~ "% (serves)",
                                 TRUE ~ Unit))

# Tables 1.2 + 5.3
M_Ausnut <- M1.2 %>% 
   bind_rows(M5.3)%>% 
   mutate(Nutrient = str_remove(Nutrient, ("\\(e\\)"))) %>%   # <- Fix naming variations by removing certain strings e.g. (e) (g) (d) (f)
   mutate(Nutrient = str_remove(Nutrient, ("\\(g\\)"))) %>%
   mutate(Nutrient = str_remove(Nutrient, ("\\(d\\)"))) %>% 
   mutate(Label = str_remove(Label, ("\\(g\\)"))) %>% 
   mutate(Label = str_remove(Label, ("\\(f\\)"))) %>% 
   mutate(cLabel = paste0(Code, " ", Label))                                   

# Tables 2.2, 3.2, 4.2
M_nut <- M2.2 %>% 
   bind_rows(M3.2, M4.2) %>% 
   mutate(Nutrient = str_remove_all(Nutrient, ("\\(d\\)"))) %>%  # <- Fix naming variations by removing certain strings e.g. (e) (g) (d) 
   mutate(Nutrient = str_remove_all(Nutrient, ("\\(g\\)"))) %>%
   mutate(Nutrient = str_remove_all(Nutrient, ("\\(f\\)"))) %>%
   mutate(Nutrient = str_remove_all(Nutrient, ("\\(e\\)"))) %>% 
   mutate(Nutrient = case_when(Nutrient == "Energy" ~ "Total energy", TRUE ~ Nutrient)) %>% 
   mutate(Unit = case_when(Unit == "%" ~ "Pc", TRUE ~ Unit))

#-- Table 14.1  filter the overall Pc - will make it dynamic
A_Bev <- A14.1 %>% 
   mutate(`Unit_type` = str_remove_all(`Unit_type`, ("\\(b\\)"))) %>% 
   mutate(cLabel = paste0(Class_order," ", `Beverage_group`))%>% 
   mutate(`Beverage_group`= str_remove_all(`Beverage_group`, ("\\(e\\)"))) %>%
   mutate(`Beverage_group`= str_remove_all(`Beverage_group`, ("\\(f\\)"))) %>% 
   mutate(Unit = case_when(Unit == "Grams"~ "g", TRUE ~ Unit)) %>% 
   filter(Val_type == "Amt")

# Tables 14.2      # Make 'Val_type' as per A_Bev
M_Bev <- M14.2 %>% 
   pivot_longer(7:8, names_to = "Val_type", values_to = "Val") %>% 
   mutate(`Unit_type` = str_remove_all(`Unit_type`, ("\\(b\\)"))) %>% 
   mutate(`Beverage_group`= str_remove_all(`Beverage_group`, ("\\(d\\)"))) %>%
   mutate(`Beverage_group`= str_remove_all(`Beverage_group`, ("\\(e\\)"))) %>% 
   mutate(cLabel = paste0(Class_order," ", `Beverage_group`)) %>% 
   mutate(Measure = case_when(Measure == "Added sugar" ~ "Added sugars", 
                              Measure == "Free sugar" ~ "Free sugars",
                              TRUE ~ Measure)) %>% 
   filter(Val_type == "Amt")


#-----------------------------------------------------------------------------------
#--- Prepare UI elements ---
#
# Class_level, Code, Label, Unit, Nutrient, Type, ADG_group, Disc_type, Year, Month
#-----------------------------------------------------------------------------------

Class_level <- A_Ausnut %>% 
   group_by(Class_level) %>% 
   tally() 
Class_level <- as.list(as.character(Class_level$Class_level))

Label <- A_Ausnut %>% 
   group_by(Label) %>% 
   tally()
Label <- as.list(as.character(Label$Label))


Two_dig <- A_Ausnut %>%
   filter(Class_level %in% c("Major", "Total")) %>%
   group_by(cLabel) %>%
   tally()
Two_dig <- as.list(as.character(Two_dig$cLabel))

Thr_dig <- A_Ausnut %>%
   filter(Class_level == "Sub-major") %>%
   group_by(cLabel) %>%
   tally()
Thr_dig <- as.list(as.character(Thr_dig$cLabel))

#-- Units : 2x- Ausnut  Vs Nutrients  
A_Unit <- A_Ausnut %>% 
   group_by(Unit) %>% 
   tally()
A_Unit <- as.list(as.character(A_Unit$Unit))   #  <- Ausnut units (annual)


N_Unit <-M_nut %>% 
   group_by(Unit) %>% 
   tally()
N_Unit <- as.list(as.character(N_Unit$Unit)) # <- Nutrient units

#-- Nutrients : 2x- Ausnut  Vs Nutrients  
A_Nutrient <- A_Ausnut %>%
   group_by(Nutrient) %>%
   tally()
A_Nutrient <- as.list(as.character(A_Nutrient$Nutrient))

N_Nutrient <- A_nut %>%
   group_by(Nutrient) %>%
   tally()
N_Nutrient <- as.list(as.character(N_Nutrient$Nutrient))


Type <-  M_nut %>% 
   group_by(Type) %>% 
   tally()
Type <- as.list(as.character(Type$Type))

ADG_group <- A_nut %>% 
   group_by(ADG_group) %>% 
   tally()

ADG_group <- as.list(as.character(ADG_group$ADG_group))


ADG_subgroup <- A4.3_4 %>% 
   group_by(ADG_subgroup) %>% 
   tally()
ADG_subgroup <- as.list(as.character(ADG_subgroup$ADG_subgroup))


Disc_type <- M_nut %>% 
   group_by(Disc_type) %>% 
   tally()
Disc_type <- as.list(as.character(Disc_type$Disc_type))


Date <- M_nut %>% 
   group_by(Date) %>% 
   tally()
Date <- as.list(as.character(Date$Date))


Year <- A_nut %>% 
   group_by(Year) %>% 
   tally()

Year <- as.list(as.character(Year$Year))

Bev_grp <- A_Bev %>% 
   mutate(Beverage_group = as.character(Beverage_group)) %>% 
   group_by(Beverage_group) %>% 
   tally()
Bev_grp <- as.list(as.character(Bev_grp$Beverage_group))

Bev_measure <- A_Bev %>% 
   mutate(Measure = as.character(Measure)) %>% 
   group_by(Measure) %>% 
   tally()
Bev_measure <- as.list(as.character(Bev_measure$Measure))

Bev_unit <- A_Bev %>% 
   mutate(Unit = as.character(Unit)) %>% 
   group_by(Unit) %>% 
   tally()
Bev_unit <- as.list(as.character(Bev_unit$Unit))

#--- Get start/end months
dmax <- max(M_nut$Date)
dmin <- min(M_nut$Date)

#---Today's date 
now <- format(today(),"%d %B %Y")


#-----------------------------------
# ---Make categoricals into factors---
#-----------------------------------

# For A_ausnut$Nutrient
Anut_lvl <- c("Weight", "Weight (gross, annual)", "Energy", 	"Disc. Energy", 	"Protein", 	"Carbohydrate", 	"Added sugars",
              "Free sugars", 	"Total fat", 	"Saturated fat", 	"Sodium", 	"Dietary fibre")


#-- For A_nut$Nutrient & M_nut$Nutrient --
Nnut_lvl <- c( "Energy", "Total energy", "Moisture", 	"Protein", 	"Carbohydrate", 	"Total sugars", 	"Added sugars", 	"Free sugars", 
               "Starch", "Total fat", "Polyunsaturated fat", 	"Monounsaturated fat", 	"Saturated fat", 	"Linoleic acid", 
               "Alpha-Linolenic acid", 	"LC omega-3 fatty acids", 	"Trans fatty acids", 	"Dietary Fibre", 	"Alcohol", 	"Preformed Vitamin A", 
               "Pro Vitamin A", 	"Vitamin A retinol equivalent", 	"Riboflavin (B2)", 	"Thiamin (B1)", 	"Niacin (B3)", 	"Niacin equivalent", 
               "Vitamin B6", 	"Vitamin B12", 	"Folate, natural", 	"Folic acid", 	"Total Folates", 	"Folate equivalent", 	"Vitamin C", 
               "Vitamin E", 	"Calcium", 	"Iodine", 	"Iron", 	"Magnesium", 	"Phosphorus", 	"Potassium", 	"Selenium", 	"Sodium", 
               "Zinc", 	"Cholesterol", 	"Caffeine", 	"ADG", "ADG_sub")

#- For M_Ausnut$Nutrient
MAnut_lvl <- c("Weight", "Energy")


A_Ausnut <- A_Ausnut %>% 
   mutate(cLabel = factor(cLabel), 
          Year = factor(Year),
          Nutrient = factor(Nutrient, levels = Anut_lvl))

M_Ausnut <- M_Ausnut %>% 
   mutate(cLabel = factor(cLabel), 
          Unit = factor(Unit), 
          Nutrient = factor(Nutrient, levels = MAnut_lvl))

A_nut <- A_nut %>% 
   mutate(Nutrient = factor(Nutrient, levels = Nnut_lvl), 
          Year = factor(Year),
          Unit = factor(Unit),
          Type = factor(Type))

M_nut <- M_nut %>% 
   mutate(Nutrient = factor(Nutrient, levels = Nnut_lvl), 
            Type = factor(Type))

bev_lvl <- c("Cordials", 	"Cordials - intense sweetened", 	"Cordials - sugar sweetened", 	"Electrolyte drinks", "Electrolyte drinks - intense sweetened",
             "Electrolyte drinks - sugar sweetened", 	"Energy drinks", 	"Energy drinks - intense sweetened", 	"Energy drinks - sugar sweetened",
             "Fruit and vegetable drinks", 	"Fruit and vegetable juice blends", 	"Fruit and vegetable juices", 	"Fruit drinks", 	"Fruit juices",
             "Packaged water", 	"Soft drinks", "Soft drinks - intense sweetened", 
             "Soft drinks - sugar sweetened", "Total intense-sweetened beverages", 	"Total other non-alcoholic beverages", 	"Total selected non-alcoholic beverages", 	
             "Total sugar-sweetened beverages", 	"Vegetable drinks", 	"Vegetable juices", 	"Water, fortified",
             "Water, plain or sparkling")

A_Bev <- A_Bev %>% 
   mutate(Beverage_group = factor(Beverage_group, levels = bev_lvl),
          Measure = factor(Measure),
          Year = factor(Year),
          Unit_type = factor(Unit_type), 
          Val_type = factor(Val_type))


M_Bev <- M_Bev %>% 
   mutate(Beverage_group = factor(Beverage_group, levels = bev_lvl),
          Measure = factor(Measure),
          Unit_type = factor(Unit_type), 
          Val_type = factor(Val_type))

#=====================================================================
# SHINY DASHBOARD app 
#=====================================================================

ui <- shinyUI(fluidPage(
   tags$style(type="text/css",
              ".shiny-output-error { visibility: hidden; }",
              ".shiny-output-error:before { visibility: hidden; }"
   ),
   
   headerPanel("Apparent Consumption of Selected Foodstuffs, Australia, 2020-21"),
   
   sidebarPanel(
      radioButtons ("choosetype", "Explore apparent consumption amounts from:",
                    choices = c("Tables 1, 5-13: g, kJ and selected nutrients from AUSNUT foods" = "AUSNUT", 
                                "Tables 2-3: All macro & micronutrients " = "NUT", 
                                "Table 4: ADG food groups" = "ADG", 
                                "Table 14: Sweetened beverages"= "SSB"), 
                    selected = c("AUSNUT")),    
      
      radioButtons ("choosetime", "Time aggregate:",
                    choices = c("Annual" = "Finyr", "Monthly" = "Month"),
                    selected = c("Finyr"),inline = TRUE),
      
      #----CONDITION  choosetime = Month----
      conditionalPanel(
         condition = "input.choosetime == 'Month'",
         
         sliderInput("dateRange","Start-end months:",
                     min = as.Date(dmin),
                     max = as.Date(dmax),
                     value=as.Date(c(dmin, dmax)),
                     step = 5,
                     timeFormat="%b %Y")),
      
      ## CONDITION: choosetime = Finyr
      conditionalPanel(
         condition = "input.choosetime == 'Finyr'",
         
         checkboxGroupInput("finyr", "Years:" ,
                            choices= c("2018-19", "2019-20", "2020-21"), 
                            selected = c("2018-19", "2019-20", "2020-21"), inline = TRUE)), 
      
      
      #----CONDITION 1: choosetype = AUSNUT & choosetime = Finyr----
      conditionalPanel(
         condition = "input.choosetype == 'AUSNUT' &  input.choosetime == `Finyr`",
         
         checkboxGroupInput("Class1", "Classification level:", 
                            choices = c("Major", "Sub-major", "Total", "Beverages", "Disc. total" = "Discretionary status" ), 
                            selected = "Major", inline = TRUE),
         
         pickerInput("Majgrp1", "AUSNUT major food groups:", choices = c(Two_dig), 
                     selected = c(Two_dig), multiple = TRUE, options = list(`actions-box` = TRUE)), 
         
         pickerInput("Mingrp1", "AUSNUT sub-major food groups:", choices = c(Thr_dig),
                     selected = c(Thr_dig), multiple = TRUE, options = list(`actions-box` = TRUE)), 
         
         pickerInput("Othgrp", "Other AUSNUT aggregations:", 
                     choices = c("Discretionary foods", "Non-discretionary foods", "Total selected beverages", "Total sugar sweetened beverages",
                                 "Total intense sweetened and other non-alcoholic beverages", "Total foods and beverages"), 
                     multiple = TRUE, options = list(`actions-box` = TRUE)),
         
         numericInput("Num", "Max number of groups to display", value = 9),
         
         selectInput("A_Nutrient", "Select from nutrients/weight/kJ from foods:", 
                     choices = c("Weight", "Weight (gross, annual)", "Energy", "Disc. Energy", "Protein", "Carbohydrate", "Added sugars",
                                 "Free sugars", "Total fat", "Saturated fat", "Sodium", "Dietary fibre"), 
                     selected = "Energy", multiple = FALSE), 
         
         radioButtons("Percent", "Value as %", choices = c("Value", "%"), selected = "Value", inline = TRUE), 
      ),      
      
      #----CONDITION 2: choosetype = AUSNUT & choosetime = Month----
      conditionalPanel(
         condition = "input.choosetype == 'AUSNUT' &  input.choosetime == `Month`",
         
         checkboxGroupInput("Class2", "Classification level:",                                                                                                          
                            choices = c("Major", "Sub-major", "Total", "Beverages", "Disc. total" = "Discretionary status" ), selected = "Total", inline = TRUE),
         
         pickerInput("Majgrp2", "AUSNUT major food groups:", choices = c(Two_dig), multiple = TRUE, options = list(`actions-box` = TRUE)),
         
         pickerInput("Mingrp2", "AUSNUT sub-major food groups:", choices = c(Thr_dig),  multiple = TRUE, options = list(`actions-box` = TRUE)),
         
         pickerInput("Othgrp2", "Other AUSNUT aggregations:", 
                     choices = c("Discretionary foods", "Non-discretionary foods", "Total selected beverages", "Total sugar sweetened beverages",
                                 "Total intense sweetened and other non-alcoholic beverages", "Total foods and beverages"), 
                     selected = "Total foods and beverages", multiple = TRUE, options = list(`actions-box` = TRUE)), 
         
         radioButtons("gkJ", "Select weight (g) or energy (kJ):", choices = c("g", "kJ"), selected = "kJ", inline = TRUE)),      
      
      
      #----CONDITION 3/4: choosetype = NUT & choosetime = Finyr OR Month----
      conditionalPanel(
         condition = "input.choosetype == 'NUT'",
         
         pickerInput("Macronut", "Select from energy & macronutrients:",
                     choices = c("Total energy", "Protein", "Carbohydrate", "Total sugars", "Added sugars", "Free sugars", 
                                 "Starch", 	"Total fat", "Polyunsaturated fat", "Monounsaturated fat", "Saturated fat", "Linoleic acid", 
                                 "Alpha-Linolenic acid", "LC omega-3 fatty acids", 	"Trans fatty acids", "Dietary Fibre", 	"Alcohol"),
                     selected = c("Carbohydrate", "Protein", "Total fat", "Dietary Fibre") , multiple = TRUE, options = list(`actions-box` = TRUE)),
         
         pickerInput("Micronut", "or micronutrients & proximates:",
                     choices = c("Preformed Vitamin A", 	"Pro Vitamin A", 	"Vitamin A retinol equivalent", 	"Riboflavin (B2)", "Thiamin (B1)", 
                                 "Niacin (B3)",  "Niacin equivalent", "Vitamin B6",  "Vitamin B12", "Folate, natural", "Folic acid", "Total Folates",
                                 "Folate equivalent", "Vitamin C", "Vitamin E", 	"Calcium", 	"Iodine", "Iron", "Magnesium", "Phosphorus", 
                                 "Potassium", "Selenium", "Sodium", "Zinc", "Cholesterol", "Caffeine", "Moisture"),
                     multiple = TRUE, options = list(`actions-box` = TRUE)), 
         
         
         
         radioButtons("Type", "Select measure type:", choices =  c("Macronutrients (kJ/ % of kJ)" = "Macronutrient_kJ",
                                                                   "Micro / macronutrients (g/mg/µg)" = "All_nutrients"),  selected = c("Macronutrient_kJ")),  
         conditionalPanel(
            condition = "input.Type == 'Macronutrient_kJ'",
            radioButtons("Units3a", "Units:", choices = c("kJ", "%" = "Pc"), selected = c("Pc"), inline = TRUE)),
         
         conditionalPanel(
            condition = "input.Type == 'All_nutrients'",
            checkboxGroupInput("Units3b", "Units:", choices = c("g", "mg", "µg"), selected = c("g", "mg", "µg"), inline = TRUE)),
         
         conditionalPanel(
            condition = "input.Type == 'Macronutrient_kJ' | input.Type == 'All_nutrients' ",
            radioButtons("graphstack", "Stacked/normal barplot:", choices = c("Stacked", "Normal"), selected = c("Stacked"), inline = TRUE))),
      
      
      #----CONDITION 5/6: Where choosetype = ADG  & choosetime = Finyr OR Month----
      conditionalPanel(
         condition = "input.choosetype == 'ADG'",
         
         conditionalPanel( condition = "input.choosetime == 'Finyr'",
                           
                           radioButtons("gram_serve", "Grams or Serves:", choices = c("Grams", "Serves"), selected = c("Serves"), inline = TRUE),
                           
                           radioButtons("Subgrp", "ADG groups / subgroups:", choices = c("ADG" = "ADG", "ADG subgroup" = "ADG_sub"), 
                                        selected = c("ADG"), inline = TRUE),
                           
                           conditionalPanel( condition = "input.Subgrp == 'ADG_sub'",
                                             
                                             pickerInput("ADGgrp1", "Select from ADG foods:", choices = c("Grains","Veg", "Fruit", "Dairy", "Meats", "UnsatFat"), 
                                                         selected = c("Grains") , multiple = FALSE),
                                             
                                             radioButtons("ADGstack", "Normal or Stacked:", choices = c("Normal", "Stacked"), selected = c("Normal"), inline = TRUE),
                                             radioButtons("Type2", "Value or percent:",
                                                          choices = c("Value" = "val", "Percent" = "pc"), selected = c("val"), inline = TRUE),
                                             
                                             pickerInput("Comp_grp", "Comparable ADG food subcomponents:",
                                                         choices = c("Component subgroup",
                                                                     "Refined status (grains/cereals)",
                                                                     "Milk type", "Milk fat level", "Fat level (dairy products)",
                                                                     "Lean status (meats)", "Meat processing status"), 
                                                         selected = c("Component subgroup") , multiple = FALSE)),                       													
                           
                           conditionalPanel( condition = "input.Subgrp == 'ADG'",
                                             
                                             pickerInput("ADGgrp2", "Select from ADG foods:", choices = c("Grains","Veg", "Fruit", "Dairy", "Meats", "UnsatFat"), 
                                                         selected = c("Grains","Veg", "Fruit", "Dairy", "Meats", "UnsatFat"), 
                                                         multiple = TRUE, options = list(`actions-box` = TRUE))),
                           
                           radioButtons("Disctype1", "Discretionary status:", choices = c("NonDisc", "Disc", "Total"),
                                        selected = "NonDisc", inline = TRUE)
                           
         ),             
         
         conditionalPanel( condition = "input.choosetime == 'Month'",
                           
                           pickerInput("ADGgrp3", "Select from ADG foods:", choices = c("Grains","Veg", "Fruit", "Dairy", "Meats", "UnsatFat"), 
                                       selected = c("Grains","Veg", "Fruit", "Dairy", "Meats") , multiple = TRUE, options = list(`actions-box` = TRUE)),
                           
                           radioButtons("Disctype2", "Discretionary status:", choices = c("NonDisc", "Disc", "Total"), selected = "NonDisc", inline = TRUE)
         )
      ),                 
      
      
      #----CONDITION 7/8: Where choosetype = SSB---- 
      conditionalPanel(
         condition = "input.choosetype == 'SSB'",
         
         pickerInput("Bev_grp", "Select from beverages:", choices = 
                        c("Cordials", 
                          "Cordials - intense sweetened", 
                          "Cordials - sugar sweetened", 
                          "Electrolyte drinks", 
                          "Electrolyte drinks - intense sweetened", 
                          "Electrolyte drinks - sugar sweetened", 
                          "Energy drinks", 
                          "Energy drinks - intense sweetened", 
                          "Energy drinks - sugar sweetened", 
                          "Fruit and vegetable drinks", 
                          "Fruit and vegetable juice blends", 
                          "Fruit and vegetable juices", 
                          "Fruit drinks", 
                          "Fruit juices", 
                          "Packaged water", 
                          "Soft drinks", 
                          "Soft drinks - intense sweetened", 
                          "Soft drinks - sugar sweetened", 
                          "Total intense-sweetened beverages", 
                          "Total other non-alcoholic beverages", 
                          "Total selected non-alcoholic beverages", 
                          "Total sugar-sweetened beverages", 
                          "Vegetable drinks", 
                          "Vegetable juices", 
                          "Water, fortified", 
                          "Water, plain or sparkling"),
                     selected = c("Soft drinks - intense sweetened", "Soft drinks - sugar sweetened") , 
                     multiple = TRUE, options = list(`actions-box` = TRUE)), 
         
         pickerInput("BevM", "Measure of interest:", choices = c(Bev_measure), selected = "Volume", multiple = FALSE),   
         
         radioButtons("AmtPc", "Amount or proportion(%):", choices = c("Amount" = "Val", "Proportion" = "Pc"), selected = "Val", inline = TRUE),
         
         radioButtons("graphstack2", "Normal or stacked %:", choices = c("Normal", "Stacked"), selected = c("Stacked"), inline = TRUE),
         
         
         conditionalPanel(
            condition = "input.BevM == 'Volume'",
            radioButtons("BevUnit1", "Choose units:",
                         choices = c("Megalitres (000s)", "mL"), selected = "mL")),
         
         conditionalPanel(
            condition = "input.BevM == 'Weight'",
            radioButtons("BevUnit2", "Choose units:",
                         choices = c("Tonnes (000s)", "Grams" = "g"), selected = "g")),
         
         conditionalPanel(
            condition = "input.BevM == 'Energy'",
            radioButtons("BevUnit3", "Choose units:",
                         choices = c("kJ"), selected = "kJ")),
         
         conditionalPanel(
            condition = "input.BevM == 'Added sugars' | input.BevM == 'Free sugars' ", 
            radioButtons("BevUnit4", "Choose units:", 
                         choices = c("Grams" = "g" ), selected = "g")) 
      ),
      
      width = 3),
   
   #---- close Sidepanel, now mainpanel ----
   
   mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Graph", highchartOutput("hcontainer",height = "760px")),
                  tabPanel("Table", verbatimTextOutput("message"), DT::dataTableOutput("table"))),
      
      # adding the div tag to the mainpanel
      tags$div(class="header", checked=NA,
               tags$p("Source: ", tags$a(href="https://www.abs.gov.au/statistics/health/health-conditions-and-risks/apparent-consumption-selected-foodstuffs-australia/latest-release",
                      target="_blank", "ABS Apparent Consumption of Selected Foodstuffs, Australia, 2020-21")),
               tags$p(paste0("Data retrieved from ABS, ", now))),
      
      downloadButton("downloadTb", "Download graph/table selection:"))
   
))


#=========================================
# Server
#=========================================
server <- function(input, output) {  
   
   Type <- reactive({
      list(N_mth =input$choosetype) })  
   
   Time <- reactive({
      list(choosetime =input$choosetime) })  
   
   Fy <- reactive({
      list (finyr =input$finyr) })
   
   Maj1 <-  reactive({
      list(Majgrp1 =input$Majgrp1) })  
   
   Min1 <-  reactive({
      list(Mingrp1 =input$Mingrp1) })
   
   Othgrp <-  reactive({
      list(Othgrp =input$Othgrp) })
   
   Othgrp2 <-  reactive({
      list(Othgrp2 =input$Othgrp2) })
   
   Class1 <- reactive({
      list(Class1 =input$Class1) })
   
   Class2 <- reactive({
      list(Class2 =input$Class2) })
   
   Percent <- reactive({
      list(Percent =input$Percent) }) 	
   
   Maj2 <-  reactive({
      list(Majgrp2 =input$Majgrp2) })  
   
   Min2 <-  reactive({
      list(Mingrp2 =input$Mingrp2) })
   
   Num <- reactive({
      list(Num =input$Num) })
   
   NutA <-  reactive({
      list(A_Nutrient =input$A_Nutrient) }) 
   
   Macronut <-  reactive({
      list(Macronut =input$Macronut) }) 
   
   Micronut <-  reactive({
      list(Micronut =input$Micronut) }) 
   
   
   UnitA <-  reactive({
      list(A_Unit =input$A_Unit) })
   
   Type <-  reactive({
      list(Type =input$Type) })
   
   Units <-  reactive({
      list(Units =input$Units) })
   
   Units3a <-  reactive({
      list(Units3a =input$Units3a) })
   
   Units3b <-  reactive({
      list(Units3b =input$Units3b) }) 
   
   gkJ <-  reactive({
      list(gkJ = input$gkJ) })  
   
   #-------
   Grm_Srv <- reactive({
      list(Grm_Srv = input$gram_serve)})
   
   Subgrp <- reactive({
      list(Subgrp = input$Subgrp)})  
   
   ADGgrp1 <-  reactive({
      list(ADGgrp1 = input$ADGgrp1)})
   
   ADGgrp2 <-  reactive({
      list(ADGgrp2 = input$ADGgrp2)})
   
   ADGgrp3 <-  reactive({
      list(ADGgrp3 = input$ADGgrp3)})
   
   Disctype1 <- reactive({
      list(Disctype1 = input$Disctype1)})
   
   Disctype2 <- reactive({
      list(Disctype2 = input$Disctype2)})
   
   Comp_grp <- reactive({
      list(Comp_grp = input$Comp_grp)})
   
   Type2 <- reactive({
      list(Type2 = input$Type2)})
   
   
   
   #  Units lookup   - AUSNUT tables
   U <- reactive({ A_Ausnut %>%
         filter(Val_pc == Percent()$Percent) %>% 
         filter(Nutrient %in% NutA()$A_Nutrient) %>% 
         group_by(Nutrient) %>%
         summarise(unit = max(Unit)) })
   
   #  Units lookup   - Nut tables
   U2 <- reactive({ A_nut %>%
         filter(Nutrient %in% Macronut()$Macronut | Nutrient %in% Micronut()$Micronut)%>%
         filter(Type == Type()$Type) %>% 
         filter(Unit %in% Units3a()$Units3a | Unit %in% Units3b()$Units3b) %>% 
         group_by(Type) %>%
         summarise(Unit_label = max(Unit_label)) })
   
   #  Units lookup   - M_nut tables
   U2m <- reactive({ M_nut %>%
         filter(Nutrient %in% Macronut()$Macronut | Nutrient %in% Micronut()$Micronut)%>%
         filter(Type == Type()$Type) %>% 
         filter(Unit %in% Units3a()$Units3a | Unit %in% Units3b()$Units3b) %>% 
         group_by(Type) %>%
         summarise(Unit_label = max(Unit)) })
   
      #  Units lookup  - ADG summary
   U3 <- reactive({ A_nut %>%
         filter(Nutrient == "ADG") %>% 
         filter(Unit == Grm_Srv()$Grm_Srv) %>% 
         group_by(Type) %>%
         summarise(Unit_label = max(Unit_label)) })
   
   #  Units lookup  - ADG subgroup
   U4 <- reactive({ A_nut %>%
         filter(Nutrient == "ADG_sub") %>% 
         filter(Unit == Grm_Srv()$Grm_Srv) %>% 
         filter(Type == Type2()$Type2) %>%
         group_by(Type) %>%
         summarise(Unit_label = max(Unit_label)) })
   
   
   #---- Dataframes for plotting / tabulation----
   df_Ausn_fy <- reactive({ A_Ausnut %>%
         filter(Class_level %in% Class1()$Class1) %>% 
         filter(Nutrient %in% NutA()$A_Nutrient)%>%
         filter(Year %in% Fy()$finyr) %>%     
         filter(Val_pc == Percent()$Percent) %>%  
         filter(cLabel %in% Maj1()$Majgrp1 | cLabel %in% Min1()$Mingrp1 | Label %in% Othgrp()$Othgrp) %>% 
         group_by(Year) %>% 
         arrange(desc(Val)) %>% 
         mutate(a =1) %>%
         mutate(c = cumsum(a)) %>% 
         ungroup() %>% 
         filter(c<= Num()$Num) %>%
         select(-a,-c,-cLabel)
   }) 
   
   
   df_Ausn_mth <- reactive({ M_Ausnut %>%
         filter(Unit == gkJ()$gkJ)%>%
         filter(Class_level %in% Class2()$Class2) %>% 
         filter(cLabel %in% Maj2()$Majgrp2 | cLabel %in% Min2()$Mingrp2 | Label %in% Othgrp2()$Othgrp2) %>% 
         group_by(Nutrient, cLabel) %>% 
         arrange(Date) %>% ungroup() %>% 
         mutate(nxt = if_else(Date< max(Date), lead(Date), Date))%>%
         filter(nxt >= input$dateRange[1]) %>%  
         filter(Date <= input$dateRange[2]) %>% 
         select(-cLabel, -nxt)
   })
   
   
   df_Nut_fy <- reactive({ A_nut %>%
         filter(Nutrient %in% Macronut()$Macronut | Nutrient %in% Micronut()$Micronut)%>%
         filter(Year %in% Fy()$finyr) %>%     
         filter(Type == Type()$Type) %>% 
         filter(Unit %in% Units3a()$Units3a | Unit %in% Units3b()$Units3b) %>%
         group_by(Year) %>% 
         arrange(desc(Val)) %>% 
         ungroup() %>% 
         select(-ADG_group, -Disc_type, -Type)
   })
   
   
   df_Nut_mth <- reactive({ M_nut %>%
         filter(Nutrient %in% Macronut()$Macronut | Nutrient %in% Micronut()$Micronut)%>%
         group_by(Nutrient, Unit, Type) %>% 
         arrange(Date) %>% ungroup() %>% 
         mutate(nxt = if_else(Date< max(Date), lead(Date), Date))%>%
         filter(nxt>= input$dateRange[1]) %>%  
         filter(Date <= input$dateRange[2]) %>%
         filter(Type == Type()$Type) %>% 
         filter(Unit %in% Units3a()$Units3a | Unit %in% Units3b()$Units3b) %>% 
         select(-ADG_group, -Disc_type, -nxt)
   })   
   
   
   df_ADG_fy <- reactive({ A_nut %>%
         filter(Nutrient == "ADG")%>%
         filter(Unit == Grm_Srv()$Grm_Srv) %>%
         filter(Year %in% Fy()$finyr) %>%
         filter(ADG_group %in% ADGgrp2()$ADGgrp2) %>%
         filter(Disc_type == Disctype1()$Disctype1) %>%
         select(-Type)
   }) 
   
   
   df_ADGsubgrp <- reactive({A_nut %>%
         filter(Nutrient == "ADG_sub")%>%
         filter(ADG_group == ADGgrp1()$ADGgrp1) %>%
         filter(Comp_grp == Comp_grp()$Comp_grp) %>% 
         filter(Unit == Grm_Srv()$Grm_Srv) %>% 
         filter(Type == Type2()$Type2) %>% 
         filter(Year %in% Fy()$finyr) %>%     
         filter(Disc_type %in% Disctype1()$Disctype1) %>% 
         select(-Type)
   })   
   
   
   df_ADG_mth <- reactive({ M_nut %>%
         filter(Nutrient == "ADG") %>% 
         filter(ADG_group %in% ADGgrp2()$ADGgrp2) %>% 
         filter(Disc_type == Disctype2()$Disctype2) %>% 
         group_by(Unit, ADG_group, Disc_type) %>% 
         arrange(Date) %>%
         mutate(nxt = if_else(Date< max(Date), lead(Date), Date))%>%
         filter(nxt>= input$dateRange[1]) %>%  
         filter(Date <= input$dateRange[2]) %>%
         ungroup() %>% 
         select(-Type, -nxt)
   })   
   
   
   # Beverage_group      
   Bev_grp <- reactive({
      list(Bev_grp = input$Bev_grp)})
   
   # Measure   
   BevM <- reactive({
      list(BevM = input$BevM)})
   
   # "Val" | "Pc"    
   AmtPc <-  reactive({
      list(AmtPc = input$AmtPc)})
   
   # "Megalitres (000s)" | "mL"
   BevUnit1 <- reactive({
      list(BevUnit1 = input$BevUnit1)})
   
   # "Tonnes (000s)" | "g"  
   BevUnit2 <- reactive({
      list(BevUnit2 = input$BevUnit2)})
   
   # "kJ"   
   BevUnit3 <- reactive({
      list(BevUnit3 = input$BevUnit3)})
   # "g"   
   BevUnit4 <- reactive({
      list(BevUnit4 = input$BevUnit4)})
   
   
   df_SSB_fy <- reactive({ A_Bev %>%
         filter(Beverage_group %in% Bev_grp()$Bev_grp)%>%
         filter(Measure == BevM()$BevM) %>%
         filter(Unit == BevUnit1()$BevUnit1 | Unit == BevUnit2()$BevUnit2 | Unit == BevUnit3()$BevUnit3 | Unit == BevUnit4()$BevUnit4) %>% 
         filter(Year %in% Fy()$finyr) %>% 
         select(-cLabel, -Val_type) %>% 
         group_by(Year) %>% 
         mutate(Tot = sum(Val)) %>% 
         group_by(Year, Beverage_group) %>% 
         mutate(Pc = round(Val/Tot*100,1)) %>% 
         ungroup()
   })
   
   #  Units lookup  - SSB x finyr
   U5 <- reactive({ df_SSB_fy() %>%
         summarise(Unit = max(Unit)) })   
   
   df_SSB_mth <- reactive({ M_Bev %>%
         filter(Beverage_group %in% Bev_grp()$Bev_grp)%>%
         filter(Measure == BevM()$BevM) %>% 
         filter(Unit == BevUnit1()$BevUnit1 | Unit == BevUnit2()$BevUnit2 | Unit == BevUnit3()$BevUnit3 | Unit == BevUnit4()$BevUnit4) %>% 
         group_by(Beverage_group) %>% 
         arrange(Date) %>% ungroup() %>% 
         mutate(nxt = if_else(Date< max(Date), lead(Date), Date))%>%
         filter(nxt>= input$dateRange[1]) %>%  
         filter(Date <= input$dateRange[2]) %>% 
         ungroup() %>% 
         select(-nxt, -cLabel) %>% 
         group_by(Date) %>% 
         mutate(Tot = sum(Val)) %>% 
         group_by(Date, Beverage_group) %>% 
         mutate(Pc = round(Val/Tot*100,1)) %>% 
         ungroup()
   })
   
   #  Units lookup  - SSB x mth
   U6 <- reactive({ df_SSB_mth() %>%
         summarise(Unit = max(Unit)) })      
   
   
   #---------------------------
   #---- OUTPUT hchart----
   #---------------------------
   
   output$hcontainer <- renderHighchart({
      
      # 7.1) SSB x finyr Val side-by-side + stacking  
      if(input$choosetype == 'SSB' &  input$choosetime == 'Finyr' & input$AmtPc == "Val" ){
         
         hc <- df_SSB_fy() %>% 
            drop_na() %>% 
            group_by(Beverage_group) %>% 
            arrange(Year) %>% ungroup() %>% 
            hchart(.,
                   type = "bar",
                   hcaes(x = Year,
                         y = Val,
                         group = Beverage_group)) %>%
            hc_xAxis(title = list(text = paste0("Selected beverages"))) %>%
            hc_yAxis(title = list(text = paste0(U5()$Unit))) %>%
            hc_title(text = paste0("Selected beverages - ",input$BevM)) %>%
            hc_add_theme(hc_theme_economist()) %>% 
            hc_colors(abscol) %>% 
            hc_tooltip(crosshairs = TRUE, valueSuffix = paste0(" ",U5()$Unit))
         
         if(input$graphstack2 == "Stacked"){ 
            hc <- hc %>% 
               hc_plotOptions(bar = list(stacking = "normal")) 
            
         }
      }  
      
      
      # 7.2) SSB x finyr 'Pc' side-by-side + stacking  
      if(input$choosetype == 'SSB' &  input$choosetime == 'Finyr' & input$AmtPc == "Pc" ){
         
         hc <- df_SSB_fy() %>% 
            drop_na() %>% 
            group_by(Beverage_group) %>% 
            arrange(Year) %>% ungroup() %>% 
            hchart(.,
                   type = "bar",
                   hcaes(x = Year,
                         y = Pc,
                         group = Beverage_group)) %>%
            hc_xAxis(title = list(text = paste0("Selected beverages"))) %>%
            hc_yAxis(title = list(text= paste0("%"))) %>% 
            hc_title(text = paste0("Selected beverages - ",input$BevM)) %>%
            hc_add_theme(hc_theme_economist()) %>% 
            hc_colors(abscol) %>% 
            hc_tooltip(crosshairs = TRUE, valueSuffix = " %") 
         
         if(input$graphstack2 == "Stacked"){ 
            hc <- hc %>% 
               hc_plotOptions(bar = list(stacking = "normal")) 
            
         }
      }     
      
      
      # 8.1) SSB x mth x normal x "Val"
      
      if(input$choosetype == 'SSB' &  input$choosetime == 'Month' & input$AmtPc == "Val" & input$graphstack2 == "Normal")  {
         
         hc <- df_SSB_mth() %>% 
            drop_na() %>% 
            hchart(.,
                   type = "line",
                   hcaes(x = Date,
                         y = Val,
                         group = Beverage_group)) %>%
            hc_xAxis(type = "datetime", crosshair = TRUE, dateTimeLabelFormats = list(day = '%b, %Y')) %>% 
            hc_xAxis(title = list(text = paste0("Month"))) %>%
            hc_yAxis(title = list(text = paste0(U6()$Unit))) %>%
            hc_title(text = paste0("Selected beverages - ",input$BevM)) %>%
            hc_add_theme(hc_theme_economist()) %>% 
            hc_colors(abscol) %>% 
            hc_tooltip(crosshairs = TRUE, valueSuffix = paste0(" ",U6()$Unit))
      }
      
      
      # 8.2) SSB x mth x normal x "Pc"
      
      if(input$choosetype == 'SSB' &  input$choosetime == 'Month' & input$AmtPc == "Pc" & input$graphstack2 == "Normal")  {
         
         hc <- df_SSB_mth() %>% 
            drop_na() %>% 
            hchart(.,
                   type = "line",
                   hcaes(x = Date,
                         y = Pc,
                         group = Beverage_group)) %>%
            hc_xAxis(type = "datetime", crosshair = TRUE, dateTimeLabelFormats = list(day = '%b, %Y')) %>% 
            hc_xAxis(title = list(text = paste0("Month"))) %>%
            hc_yAxis(title = list(text = paste0("%"))) %>%
            hc_title(text = paste0("Selected beverages - ",input$BevM)) %>%
            hc_add_theme(hc_theme_economist()) %>% 
            hc_colors(abscol) %>% 
            hc_tooltip(crosshairs = TRUE, valueSuffix = " %") 
      }
      
      # 8.3) SSB x mth x stacked Area x "Val"
      
      if(input$choosetype == 'SSB' &  input$choosetime == 'Month' & input$AmtPc == "Val" & input$graphstack2 == "Stacked")  {
         
         hc <- df_SSB_mth() %>% 
            drop_na() %>% 
            hchart(.,
                   type = "area",
                   hcaes(x = Date,
                         y = Val,
                         group = Beverage_group)) %>%
            hc_plotOptions(area = list(stacking = "normal")) %>%  
            hc_xAxis(type = "datetime", crosshair = TRUE, dateTimeLabelFormats = list(day = '%b, %Y')) %>% 
            hc_xAxis(title = list(text = paste0("Month"))) %>%
            hc_yAxis(title = list(text = paste0(U6()$Unit))) %>%
            hc_title(text = paste0("Selected beverages - ",input$BevM)) %>%
            hc_add_theme(hc_theme_economist()) %>% 
            hc_colors(abscol) %>% 
            hc_tooltip(crosshairs = TRUE, valueSuffix = paste0(" ",U6()$Unit))
      }
      
      
      # 8.4) SSB x mth x stacked Area x "Pc"
      
      if(input$choosetype == 'SSB' &  input$choosetime == 'Month' & input$AmtPc == "Pc" & input$graphstack2 == "Stacked")  {
         
         hc <- df_SSB_mth() %>% 
            drop_na() %>% 
            hchart(.,
                   type = "area",
                   hcaes(x = Date,
                         y = Pc,
                         group = Beverage_group)) %>%
            hc_plotOptions(area = list(stacking = "normal")) %>% 
            hc_xAxis(type = "datetime", crosshair = TRUE, dateTimeLabelFormats = list(day = '%b, %Y')) %>% 
            hc_xAxis(title = list(text = paste0("Month"))) %>%
            hc_yAxis(title = list(text = paste0("%"))) %>%
            hc_title(text = paste0("Selected beverages - ",input$BevM)) %>%
            hc_add_theme(hc_theme_economist()) %>% 
            hc_colors(abscol) %>% 
            hc_tooltip(crosshairs = TRUE, valueSuffix = " %") 
      }      
      
      
      # 1) AUSNUT x year   
      if(input$choosetype == 'AUSNUT' &  input$choosetime == 'Finyr' ) {     
         hc <- df_Ausn_fy() %>% 
            group_by(Year) %>% 
            arrange(desc(Val)) %>% ungroup() %>% 
            
            hchart(.,
                   type = "bar",
                   hcaes(x = Label,
                         y = Val,
                         group = Year)) %>%
            hc_xAxis(title = list(text = paste0("Selected food groups"))) %>%
            hc_yAxis(title = list(text = paste0(U()$unit))) %>%
            hc_title(text = paste0("Daily available ", input$A_Nutrient, " per capita - ", U()$unit )) %>%
            hc_add_theme(hc_theme_economist()) %>% 
            hc_colors(abscol) %>% 
            hc_tooltip(crosshairs = TRUE, valueSuffix = paste0(" ",U()$unit))
      }
      
      # 2) AUSNUT x month
      if(input$choosetype == 'AUSNUT' &  input$choosetime == 'Month') {
         hc <- df_Ausn_mth() %>% 
            hchart(.,
                   type = "line",
                   hcaes(x = Date,
                         y = Val,
                         group = Label)) %>%
            hc_add_theme(hc_theme_economist()) %>% 
            hc_xAxis(type = "datetime", crosshair = TRUE, dateTimeLabelFormats = list(day = '%b, %Y')) %>% 
            hc_xAxis(title = list(text = paste0("Month"),  labels = list(enabled = TRUE))) %>%
            hc_yAxis(title = list(text = paste0(input$gkJ))) %>% 
            hc_title(text = paste0("Daily ", input$gkJ, " per capita" )) %>%
            hc_colors(abscol) %>% 
            hc_tooltip(crosshairs = TRUE, valueSuffix = paste0(" ",input$gkJ))
      }      
      
      # 3)  Nutrients x year  
      if(input$choosetype == 'NUT' &  input$choosetime == 'Finyr' & input$graphstack == "Normal") {
         hc <- df_Nut_fy() %>% 
            group_by(Year) %>% 
            arrange(desc(Val)) %>% ungroup() %>% 
            hchart(.,
                   type = "bar",
                   hcaes(x = Nutrient,
                         y = Val,
                         group = Year)) %>%
            hc_xAxis(title = list(text = paste0("Selected nutrient"))) %>%
            hc_yAxis(title = list(text = paste0(U2()$Unit_label))) %>%
            hc_title(text = paste0("Average daily ", U2()$Unit_label, " of selected nutrients per capita:" )) %>%
            hc_subtitle(text = paste0(" ",input$N_Nutrient))%>% 
            hc_add_theme(hc_theme_economist()) %>% 
            hc_colors(abscol) %>% 
            hc_tooltip(crosshairs = TRUE, valueSuffix = paste0(" ",U2()$Unit_label))
      }
      
      # 3.1) Stacked Nutrients x year  
      if(input$choosetype == 'NUT' &  input$choosetime == 'Finyr' & input$graphstack == "Stacked") {
         hc <- df_Nut_fy() %>% 
            group_by(Nutrient) %>% 
            arrange(Year) %>% ungroup() %>% 
            hchart(.,
                   type = "bar",
                   hcaes(x = Year,
                         y = Val,
                         group =  Nutrient)) %>%
            hc_plotOptions(bar = list(stacking = "normal")) %>%
            hc_xAxis(title = list(text = paste0("Year"))) %>%
            hc_yAxis(title = list(text = paste0(U2()$Unit_label))) %>%
            hc_title(text = paste0("Average daily ", U2()$Unit_label, " of selected nutrients per capita:" )) %>%
            hc_subtitle(text = paste0(" ",input$N_Nutrient))%>% 
            hc_add_theme(hc_theme_economist()) %>% 
            hc_colors(abscol) %>% 
            hc_tooltip(crosshairs = TRUE, valueSuffix = paste0(" ",U2()$Unit_label))
      }    
      
      
      # 4) Nutrients x month
      if(input$choosetype == 'NUT' &  input$choosetime == 'Month' ) {
         hc <- df_Nut_mth() %>% 
            hchart(.,
                   type = "line",
                   hcaes(x = Date,
                         y = Val,
                         group = paste0(Nutrient))) %>%
            hc_xAxis(type = "datetime", crosshair = TRUE, dateTimeLabelFormats = list(day = '%b, %Y')) %>% 
            hc_xAxis(title = list(text = paste0("Month"))) %>%
            hc_yAxis(title = list(text = paste0(U2()$Unit_label))) %>%
            hc_title(text = paste0("Average daily ", U2()$Unit_label, " of selected nutrients per capita:" )) %>%
            hc_add_theme(hc_theme_economist()) %>% 
            hc_subtitle(text = paste0(" ",input$N_Nutrient))%>% 
            hc_colors(abscol) %>% 
            hc_tooltip(crosshairs = TRUE, valueSuffix = paste0(" ",(U2()$Unit_label)))
         }      
      
      # 4.1) Stacked Nutrients x month
      if(input$choosetype == 'NUT' &  input$choosetime == 'Month' & input$graphstack == "Stacked") {
         
         hc <- df_Nut_mth() %>%   
            hchart(.,
                   type = "area",
                   hcaes(x = Date,
                         y = Val,
                         group =  Nutrient)) %>%
            hc_plotOptions(area = list(stacking = "normal")) %>%
            hc_xAxis(type = "datetime", crosshair = TRUE, dateTimeLabelFormats = list(day = '%b, %Y')) %>% 
            hc_xAxis(title = list(text = paste0("Month"))) %>%
            hc_yAxis(title = list(text = paste0(U2()$Unit_label))) %>% 
            hc_title(text = paste0("Contribution of selected macronutrients to total energy, ", U2()$Unit_label)) %>%
            hc_add_theme(hc_theme_economist()) %>% 
            hc_colors(abscol) %>% 
            hc_tooltip(crosshairs = TRUE, valueSuffix = paste0(" ",U2()$Unit_label))
      }
      
      # 5)  Barplot of 6 ADG foods x years
      if(input$choosetype == 'ADG' &  input$choosetime == 'Finyr' & input$Subgrp == 'ADG')  {
         
         hc <- df_ADG_fy() %>% 
            group_by(Year, ADG_group) %>% 
            arrange(Year) %>% ungroup() %>% 
            hchart(.,
                   type = "bar",
                   hcaes(x = ADG_group,
                         y = Val,
                         group = Year)) %>%
            hc_xAxis(title = list(text = paste0("ADG food"))) %>%
            hc_yAxis(title = list(text = paste0(U3()$Unit_label))) %>%
            hc_title(text = paste0(" Mean daily ",input$gram_serve, " of ADG food groups - ",  input$Disctype1)) %>%
            hc_add_theme(hc_theme_economist()) %>% 
            hc_colors(abscol) %>% 
            hc_tooltip(crosshairs = TRUE, valueSuffix = paste0(" ",U3()$Unit_label))
      }  
      
      
      # 5.1)  Barplot of ADG subgroups x years  (STACKED)
      if(input$choosetype == 'ADG' &  input$choosetime == 'Finyr' & input$Subgrp == 'ADG_sub' & input$ADGstack == 'Stacked')  {
         
         hc <- df_ADGsubgrp() %>% 
            group_by(Year, ADG_group) %>% 
            arrange(Year) %>% ungroup() %>% 
            hchart(.,
                   type = "bar",
                   hcaes(x = paste0(Year, " ", ADG_group),
                         y = Val,
                         group = ADG_subgroup)) %>%
            hc_plotOptions(bar = list(stacking = "normal")) %>%
            hc_xAxis(title = list(text = paste0("ADG food"))) %>%
            hc_yAxis(title = list(text = paste0(U4()$Unit_label))) %>%
            hc_title(text = " Mean daily serves of ADG food groups - ") %>%
            hc_subtitle(text= paste0(input$Disctype1," foods" )) %>% 
            hc_add_theme(hc_theme_economist()) %>% 
            hc_colors(abscol) %>% 
            hc_tooltip(crosshairs = TRUE, valueSuffix = paste0(" ",U4()$Unit_label))
      }  
      
      
      # 5.2)  Barplot of ADG subgroups x years  (Normal)
      if(input$choosetype == 'ADG' &  input$choosetime == 'Finyr' & input$Subgrp == 'ADG_sub' & input$ADGstack == 'Normal')  {
         
         hc <- df_ADGsubgrp() %>% 
            group_by(Year, ADG_group) %>% 
            arrange(desc(Val)) %>% ungroup() %>% 
            hchart(.,
                   type = "bar",
                   hcaes(x = ADG_subgroup,
                         y = Val,
                         group = Year)) %>%
            hc_xAxis(title = list(text = paste0("ADG food"))) %>%
            hc_yAxis(title = list(text = paste0(U4()$Unit_label))) %>%
            hc_title(text = " Mean daily serves of ADG food groups - ") %>%
            hc_subtitle(text= paste0(input$Disctype1)) %>% 
            hc_add_theme(hc_theme_economist()) %>% 
            hc_colors(abscol) %>% 
            hc_tooltip(crosshairs = TRUE, valueSuffix = paste0(" ",U4()$Unit_label))
      }  
      
      
      
      # 6) Linechart of ADG foods x month
      if(input$choosetype == 'ADG' &  input$choosetime == 'Month')  {
         
         hc <- df_ADG_mth() %>% 
            drop_na() %>% 
            hchart(.,
                   type = "line",
                   hcaes(x = Date,
                         y = Val,
                         group = ADG_group)) %>%
            hc_xAxis(type = "datetime", crosshair = TRUE, dateTimeLabelFormats = list(day = '%b, %Y')) %>% 
            hc_xAxis(title = list(text = paste0("Month"))) %>%
            hc_yAxis(title = list(text = paste0("Serves"))) %>% 
            hc_title(text = paste0(" Mean daily serves of ADG food groups - ", input$Disctype)) %>%
            hc_add_theme(hc_theme_economist()) %>% 
            hc_colors(abscol) %>% 
            hc_tooltip(crosshairs = TRUE, valueSuffix = " Serves")
      }
      
      
      
      hc
      
   })
   
   #--- Set up table objects for DT and Excel from df---  
   dt_Ausn_fy <-  reactive({ df_Ausn_fy() %>% 
         select(Class_level, Label, Year, Val, Unit) %>% 
         arrange(Year) %>% 
         pivot_wider(1:2, names_from = Year, values_from = Val)})
   
   dt_Ausn_mth <- reactive({ df_Ausn_mth() %>% 
         select(Nutrient, Date, Label, Val) %>% 
         arrange(Date) %>% 
         pivot_wider(1:2, names_from = Label, values_from = Val)})
   
   dt_Nut_fy <- reactive({df_Nut_fy() %>% 
         select(Unit, Nutrient, Year, Val) %>% 
         pivot_wider(1:2, names_from = Year, values_from = Val)
   })
   
   dt_Nut_mth <- reactive({df_Nut_mth() %>% 
         select(Unit, Date, Nutrient, Val) %>% 
         pivot_wider(1:2, names_from = Nutrient, values_from = Val)})
   
   dt_ADG_fy <- reactive({ df_ADG_fy() %>% 
         select(Unit, ADG_group, Year, Val) %>% 
         pivot_wider(1:2, names_from = Year, values_from = Val)})   
   
   dt_ADGsubgrp <- reactive({ df_ADGsubgrp() %>% 
         select(Unit, ADG_group, ADG_subgroup, Year, Val) %>% 
         pivot_wider(1:3, names_from = Year, values_from = Val)})   
   
   dt_ADG_mth <- reactive({df_ADG_mth() %>% 
         select(Unit, Date, ADG_group, Val) %>% 
         pivot_wider(1:2, names_from = ADG_group, values_from = Val)})   
   
   
   dt_SSB_fy <- reactive({df_SSB_fy() %>% 
         select(Unit, Beverage_group, Year, Val) %>% 
         pivot_wider(1:2, names_from = Year, values_from = Val)})
   
   dt_SSB_mth<- reactive({df_SSB_mth() %>% 
         drop_na() %>% 
         select(Unit, Date, Beverage_group, Val) %>% 
         pivot_wider(1:2, names_from = Beverage_group, values_from = Val)})   
   
   # Table for DT display
   output$table = DT::renderDataTable({
      
      if(input$choosetype == 'AUSNUT' &  input$choosetime == 'Finyr') {
         tab <- dt_Ausn_fy() }
      
      if(input$choosetype == 'AUSNUT' &  input$choosetime == 'Month') {
         tab <- dt_Ausn_mth()   }
      
      if(input$choosetype == 'NUT' & input$choosetime == 'Finyr') {
         tab <-dt_Nut_fy()  }
      
      if(input$choosetype == 'NUT' & input$choosetime == 'Month') {
         tab <- dt_Nut_mth() }
      
      if(input$choosetype == 'ADG' & input$choosetime == 'Finyr' & input$Subgrp == 'ADG') {
         tab <-dt_ADG_fy() }
      
      if(input$choosetype == 'ADG' &  input$choosetime == 'Finyr' & input$Subgrp == 'ADG_sub') {
         tab <- dt_ADGsubgrp() }
      
      if(input$choosetype == 'ADG' &  input$choosetime == 'Month') {
         tab <-dt_ADG_mth()   } 
      
      if(input$choosetype == 'SSB' &  input$choosetime == 'Finyr') {
         tab <-dt_SSB_fy()   } 
      
      if(input$choosetype == 'SSB' &  input$choosetime == 'Month') {
         tab <-dt_SSB_mth()   }
      
      tab
   })
   
   
   
   # Table for download
   dltab <- reactive ({
      if (input$choosetype == 'AUSNUT' &  input$choosetime == 'Finyr') {T <- dt_Ausn_fy()}
      if(input$choosetype == 'AUSNUT' &  input$choosetime == 'Month') {T <- dt_Ausn_mth()}
      
      if(input$choosetype == 'NUT' &  input$choosetime == 'Finyr') {T <- dt_Nut_fy()}
      if(input$choosetype == 'NUT' &  input$choosetime == 'Month') {T <- dt_Nut_mth()}
      
      if(input$choosetype == 'ADG' &  input$choosetime == 'Finyr') {T <- dt_ADG_fy()}
      if(input$choosetype == 'ADG' &  input$choosetime == 'Month') {T <- dt_ADG_mth()}
      
      
      if(input$choosetype == 'SSB' &  input$choosetime == 'Finyr') {T <- dt_SSB_fy()}
      if(input$choosetype == 'SSB' &  input$choosetime == 'Month') {T <- dt_SSB_mth()}
      
      T
   }) 
   
   
   # Downloadable xlsx --
   output$downloadTb <- downloadHandler(
      filename = function() { paste0("ACSF20-21, ", input$choosetime,", selected groups", ".xlsx") },
      content = function(file) { write_xlsx(dltab(), path = file) }
   ) 
   
}


#====================================
shinyApp(ui, server)
#===================================
