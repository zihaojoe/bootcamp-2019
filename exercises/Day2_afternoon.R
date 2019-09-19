library(here)
library(readr)
generation <- read.csv(here::here("data/ca_energy_generation.csv"), stringsAsFactors=F)
imports <- read.csv(here::here("data/ca_energy_imports.csv"), stringsAsFactors=F)
library(lubridate)
generation$datetime <- as_datetime(generation$datetime)
imports$datetime <- as_datetime(imports$datetime)
class(generation$datetime)

### long/wide transformation
library(reshape2)
long_gen <- melt(generation, id.vars = "datetime",
                 variable.name = "source",
                 value.name = "usage")
head(long_gen)

### merge data
merged_energy <- merge(generation, imports, by = "datetime")
dim(merged_energy)
# long transformation
long_merged_energy <- melt(merged_energy, id.vars = "datetime",
                           variable.name = "source",
                           value.name = "usage")
head(long_merged_energy)

### manipulate data
library(tidyverse)
tmp <- mutate(long_merged_energy, log_usage = log(usage))
head(tmp)
# pipe operator
merged_energy %>% 
    select(contains("hydro")) %>% 
    mutate(total_hydro = rowSums(., na.rm = T)) %>%  # .means use the former dataset
    summarize(mean_hydro = mean(total_hydro, na.rm = T))

### data.table
### data.table has 3 parameters: 1 for columns selected, 2 for things
### to to, 3 for group criteria
library(data.table)
data_file <- here::here("data", "ca_energy_generation.csv")
generation_df <- read.csv(data_file, stringsAsFactors = F)   #dataframe
generation_dt <- fread(data_file)   #datatable

# things to do
generation_dt[,newcol := 3*wind + solar*biogas/2]   # in place
generation_dt[,.(newcol = 3*wind + solar*biogas/2)]   # export data
generation_dt[,newcol := NULL]   # delete the column that was created

generation_dt[,total_hydro := small_hydro + large_hydro]
generation_dt[,.(mean(nuclear), mean(biogas))]
generation_dt[solar == 0, .(datetime, total_thermal = natural_gas + coal)]

# group by
generation_dt[,median(solar), by = hour(datetime)]
generation_dt[solar > 0, max(natural_gas), by = mday(datetime)]

imports_dt <- fread(here::here("data", "ca_energy_imports.csv"))
imports_dt[generation_dt, on = "datetime", imports_gas := imports + i.natural_gas]

imports_dt[generation_dt[hour(datetime) == 2], on = "datetime", imports_gas_2 := imports + i.natural_gas]


