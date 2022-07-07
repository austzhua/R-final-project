library(rvest)
library(tidyverse)

mental_plot <- read_rds("clean_data.rds")

#Creates a plot that seeks to find whether a person's financial status affected 
#there ability to get treatment, even if they're moderately to severely distressed
#based on the Kessler scale (k6 >= 5 based on https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3370145/)
class(mental_plot$pooryn)
#Additional wrangling to find percentage of yes responses per Kessler score
mental_plot |> 
  mutate(ybary = if_else(ybarmental == "Yes", 1, 0)) |> 
  group_by(k6, pooryn) |> 
  summarize(ybartotal = n(), 
            ybarytotal = sum(ybary), 
            pooryn = pooryn) |> 
  mutate(ybarperc = (ybarytotal/ybartotal)*100) |> 
  ggplot(aes(x = k6, y = ybarperc, color = if_else(pooryn == 1, "Yes", "No"))) + 
    geom_point() + 
    xlim(5, 24) + 
    labs(title = "Percentage of Yes Responses to \"Needed But Couldn't Afford Mental Health Care\"\nfor Kessler Scores of Moderately to Severely Distressed People ", 
         subtitle = "Those at or above the poverty threshold with high Kessler Scores were more likely to not be able to afford\nmental health care than those below the poverty threshold with the same scores", 
         x = "Kessler Psychological Distress Scale Score", 
         y = "% of Yeses to \"Needed But Couldn't Afford Mental Health Care\"", 
         color = "At or Above\nPoverty Threshold", 
         caption = "Source: IPUMS Health Surveys") + 
    theme_minimal()
ggsave("mental_plot2.png")
