library(tidyverse)
library(janitor)
library(scales)

LData <- read_csv("LD-assay data.csv", na = "-")

LData_long <- LData %>% 
  pivot_longer(cols = WT:`Mut H`) 

LData_long %>% 
  ggplot(aes(x = name,
             y = value+1))+
  geom_boxplot(width = .1,
               position = position_nudge(x= .2),
               outlier.shape = NA,
               fill = "lightblue")+
  geom_half_violin(position = position_nudge(x= -.2),
                   fill = "lightblue")+
  geom_beeswarm(shape = 21,
                fill = "lightblue",
                colour = "black",
                cex = .5)+
  scale_y_log10(labels = label_log()) +
  theme_minimal()+
  labs(x = "Bacterial Strain + plasmid pRB318",
       y = "Log10 Colony Forming Units+1")

# Model====

lsmodel1 <- lm(value+1 ~ name, data = LData_long)

summary(lsmodel1)

car::boxCox(lsmodel1)

lsmodel2 <- lm(log10(value+1) ~ name, data = LData_long)
