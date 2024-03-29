library(rvest)
library(tidyverse)

mental_plot <- read_rds("clean_data.rds")

#Creates a plot that seeks to find whether a person's financial status affected 
#their ability to get treatment, even if they're moderately to severely distressed
#based on the Kessler scale (k6 >= 5 based on https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3370145/)
mental_plot |> 
  ggplot(aes(x = k6, fill = ybarmental)) +
    geom_bar(position = 'dodge') + 
    facet_wrap(~pooryn, 
             labeller = labeller(pooryn = c("1" = "At or above poverty threshold",
                                                  "2" = "Below poverty threshold"))) + 
    xlim(4, 24) + 
    ylim(0, 4000) + 
    labs(title = "Number of People With Signs of Mental Distress Who Got vs Did Not Get treatment", 
       subtitle = "A larger number of people above the poverty threshold had more financial trouble getting treatment", 
       x = "Kessler Psychological Distress Scale Score (0-24)", 
       y = "Count", 
       fill = "Needed But Couldn't\nAfford Mental Health Care", 
       caption = "Source: IPUMS Health Surveys") + 
    theme_minimal()
ggsave("k6plot.png")
