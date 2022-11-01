#pakker
library(PxWebApiData)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(ggsci)

#hente data for innvandring fra ssb api
Data <- ApiData("https://data.ssb.no/api/v0/no/table/05185/", 
                Landbakgrunn=list('agg:Verdensdel2', c("b0", "b11", "b12", "b13", "b14", "b2", "b3", "b4", "b5", "b6", "b8", "b9")), 
                Tid=list('item', c("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022")), 
                Kjonn=FALSE, 
                ContentsCode=TRUE)

Data <- Data[[1]] 

#farger til figurer
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")

#plotte antall utenlandsfødte 
Data %>%
  filter(value > 0) %>% 
  ggplot(aes(år, value, group = landbakgrunn, color = landbakgrunn)) +
  geom_line(size = 1.1) +
  theme_minimal() +
  scale_y_continuous(labels = ~ format(.x, scientific = FALSE)) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
  scale_colour_manual(values=cbPalette)+
  labs(x = "År",
       y = "Utenlandsfødte",
       title = "Utenlandsfødte etter landbakgrunn og år i Norge",
       caption = "Kilde: https://data.ssb.no/api/v0/no/table/05185/ (hentet 26.10.2022)")

#hente data for sysselsetting blant innvandrere fra ssb api
Data2 <- ApiData("https://data.ssb.no/api/v0/no/table/13215/", 
                 Kjonn=list('item', c("0")), 
                 Alder=list('item', c("15-74")), 
                 InnvandrKat=list('item', c("B")), 
                 Landbakgrunn=list('item', c("015a")), 
                 NACE2007=list('agg:NACE260InnvGrupp2', c("SNI-01-03", "SNI-05-09", "SNI-10-33", "SNI-35-39", "SNI-41-43", "SNI-45-47", "SNI-49-53", "SNI-49.3", "SNI-55", "SNI-56", "SNI-58-63", "SNI-64-66", "SNI-68-75", "SNI-77-82", "SNI-78.2", "SNI-81.2", "SNI-84", "SNI-85", "SNI-86-88", "SNI-90-99", "SNI-00")), 
                 Tid=list('item', c("2021")), 
                 ContentsCode=TRUE)

Data2 <- Data2[[1]]

#plotte næringer innvandrere er sysselsatt i
Data2 %>% 
  mutate(næring = gsub(".*\\... ", "", `næring (SN2007)`)) %>%
  ggplot(aes(`næring (SN2007)`, value, fill = næring)) +
  scale_x_discrete(labels = NULL)+
  geom_col() +
  theme_minimal() +
  theme(legend.position="bottom",
        legend.text=element_text(size=8)) +
  scale_fill_manual(values = get_palette(palette = "aaas", k = 21)) +
  labs(x = "Næring",
       y = "Antall sysselsatte",
       title = "Antall sysselsatte blant innvandrere fra EU-land i Øst-Europa etter næring",
       caption = "Kilde: https://data.ssb.no/api/v0/no/table/13215/ (hentet 26.10.2022)")
