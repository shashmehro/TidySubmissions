
library(tidyr)
library(dplyr)
library(ggplot2)
mobile <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/mobile.csv')
landline <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/landline.csv')

ID <- NULL
ID <- paste(mobile$code,"_",mobile$year,sep = "")
mobile$ID <- ID

ID <- NULL
ID <- paste(landline$code,"_",landline$year,sep = "")
landline$ID <- ID

landline.data.consider = landline[,c("ID","landline_subs")]
merged.mobile.landline <- merge(x = mobile,y = landline.data.consider,by = "ID")

grouped.data = merged.mobile.landline %>%
              group_by(continent,year) %>%
              summarise_at(.vars = c("mobile_subs","landline_subs"),
                          .funs = mean,
                          na.rm = TRUE)


ggplot(data = grouped.data,aes(x = year))+
  geom_line(aes(y = mobile_subs, colour = continent,linetype="landline")) + 
  geom_line(aes(y = landline_subs, colour = continent,linetype="mobile"))+
  theme_bw()+
  facet_wrap(~ continent, nrow = 3, scales = "free_y")+
  ylab("Subscriptions")+
  xlab("")+
  ggtitle("When did the world ditch landlines for mobile phones?")

