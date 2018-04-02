######### Codigo Plotar série mensal (média mensal) utilizando stat_summary do GGPLOT

library(xlsx)
fernanda <- read_excel("C:/Users/RuanPS/Documents/R/exc/www/Sample Fernanda.xlsx", sheet = 1, col_names = FALSE, col_types = NULL, na = "", skip = 10)

fernanda$Month <- as.Date(cut(fernanda$X__1, breaks = "month"))
fernanda$Year <- as.Date(cut(fernanda$X__1, breaks = "year"))

library(ggplot2)
library(scales)
ggplot(data = fernanda, aes(Month, X__2)) +
  stat_summary(fun.y = "mean", geom = "bar") + 
  scale_x_date( labels = date_format("%Y-%m"), date_breaks =  "1 month") 


######## Plotar série anual (média anual) utilizando stat_summary do GGPOLT
ggplot(data = fernanda, aes(Year, X__2)) +
  stat_summary(fun.y = "mean", geom = "bar") + 
  scale_x_date( labels = date_format("%Y"), date_breaks =  "1 year") 
#################################################
library(readxl)
fernanda2 <- read_excel("C:/Users/RuanPS/Documents/R/exc/www/Sample Fernanda.xlsx", sheet = 1, col_names = FALSE, col_types = NULL, na = "", skip = 10)

library(magrittr)
library(dplyr)

#mutate(fernanda2, Month = cut(fernanda2$X__1, breaks = "month"))

############### Plotar série mensal gerando novo dataframe (dplyr)
fernanda2 %>% mutate(
  Month2 = cut(fernanda2$X__1, breaks = "month")
) %>%
  ggplot(mapping = aes(Month2, X__2, fill = "cut")) +
  summarise()

######################### VERSAO ATUALIZADA
  fernanda2$Month2 <- as.Date(cut(fernanda$X__1, breaks = "month"))
  
  fernanda3 <- fernanda2 %>%
    group_by(Month2) %>%
    summarise(mean = mean(X__2))
  
  ggplot(data = fernanda3) + 
    geom_bar(mapping = aes(x = Month2, y = mean), stat = "identity")
  
########################### VERSAO ATUALIZADA ANO
  fernanda2$Year2 <- as.Date(cut(fernanda$X__1, breaks = "year"))
  
  fernanda3 <- fernanda2 %>%
    group_by(Year2) %>%
    summarise(mean = mean(X__2))
  
    ggplot(data = fernanda3) + 
      geom_bar(mapping = aes(x = Year2, y = mean), stat = "identity")
  

################## Plotar série anual gerando novo dataframe (dplyr)
library(magrittr)
library(dplyr)
fernanda2 %>% mutate(
  Year2 = cut(fernanda2$X__1, breaks = "year")
)%>%
  ggplot(mapping=aes(Year2, X__2, fill = "cut"))+
  stat_summary(fun.y = "mean", geom = "bar")