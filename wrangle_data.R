library(ipumsr)
library(tidyverse)

#nhis_00002.xml is data I downloaded from the IPUMS NHIS website. It has data 
#on symptoms of depression, location, and whether above or below the poverty 
#threshold. The following lines convert the DDI into an R object
ddi <- read_ipums_ddi("nhis_00002.xml")
data <- read_ipums_micro(ddi)

#Converts data into a tibble
mental_health <- as_tibble(data)

#Cleaning up data by changing the numerical values of most columns into their 
#corresponding string values and/or NA. I then drop the NA values and create a 
#k6 column, which adds up the six questions that make up the Kessler 6 scale, 
#which "measures nonspecific psychological distress" (https://meps.ipums.org/meps-action/variables/K6SUM#description_section)
clean_data <- mental_health |> 
  mutate(region = case_when(REGION == 1 ~"Northeast", 
                            REGION == 2 ~"Midwest", 
                            REGION == 3 ~"South", 
                            REGION == 4 ~"West"), 
         pooryn = na_if(POORYN, 9), 
         ybarmental = case_when(YBARMENTAL == 1 ~"No", 
                                YBARMENTAL == 2 ~"Yes")) |> 
  mutate(region1 = na_if(region, 8), 
         region = na_if(region1, 9),
         ybarmental1 = na_if(ybarmental, 0), 
         ybarmental2 = na_if(ybarmental1, 7), 
         ybarmental3 = na_if(ybarmental2, 8), 
         ybarmental = na_if(ybarmental3, 9)) |> 
  mutate(asad1 = na_if(ASAD, 6), 
         asad2 = na_if(asad1, 9),
         asad3 = na_if(asad2, 8),
         asad = na_if(asad3, 7),
         anervous1 = na_if(ANERVOUS, 6), 
         anervous2 = na_if(anervous1, 9),
         anervous3 = na_if(anervous2, 8),
         anervous = na_if(anervous3, 7),
         arestless1 = na_if(ARESTLESS, 6),
         arestless2 = na_if(arestless1, 9),
         arestless3 = na_if(arestless2, 8),
         arestless = na_if(arestless3, 7),
         ahopeless1 = na_if(AHOPELESS, 6),
         ahopeless2 = na_if(ahopeless1, 9),
         ahopeless3 = na_if(ahopeless2, 8),
         ahopeless = na_if(ahopeless3, 7),
         aeffort1 = na_if(AEFFORT, 6), 
         aeffort2 = na_if(aeffort1, 9),
         aeffort3 = na_if(aeffort2, 8),
         aeffort = na_if(aeffort3, 7),
         aworthless1 = na_if(AWORTHLESS, 6), 
         aworthless2 = na_if(aworthless1, 9),
         aworthless3 = na_if(aworthless2, 8),
         aworthless = na_if(aworthless3, 7))|> 
  drop_na() |> 
  select(YEAR, region, pooryn, ybarmental, asad, anervous, arestless, ahopeless, aeffort, aworthless) |> 
  filter(YEAR >= 2015 & YEAR <= 2019) |> 
  rowwise() |> 
  summarize(year = YEAR, 
            ybarmental = ybarmental,
            pooryn = pooryn,
            k6 = sum(c_across(c(asad, anervous, arestless, ahopeless, aeffort, aworthless))))
write_rds(clean_data, "clean_data.rds")  
