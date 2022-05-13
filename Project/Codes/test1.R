library("nCov2019")
res <- query()

names(res)

library("dplyr")
library("ggrepel")

x <- res$latest
y <- res$historical

country_list =  x["global"]$country[1:10]

y[country_list]  %>%
  subset( date > as.Date("2020-10-01") ) %>%
  group_by(country) %>%
  arrange(country,date) %>%
  mutate(increase = cases - lag(cases, default =  first(cases))) -> df

ggplot(df, aes(x=date, y=increase, color=country  ))+
  geom_smooth() + 
  geom_label_repel(aes(label = paste(country,increase)), 
                   data = df[df$date == max(df$date), ], hjust = 1) + 
  labs(x=NULL,y=NULL)+ 
  theme_bw() + theme(legend.position = 'none')



library('tidyr')
library('ggrepel')
library('ggplot2')
y <- res$historical
country = "USA"

y[country] -> d
d <- gather(d, curve, count, -date, -country)

ggplot(d, aes(date, count, color = curve)) + geom_point() + geom_line() + 
  labs(x=NULL,y=NULL,title=paste("Trend of cases, recovered and deaths in", country)) +
  scale_color_manual(values=c("#f39c12", "#dd4b39", "#00a65a")) +
  theme_bw() +   
  geom_label_repel(aes(label = paste(curve,count)), 
                   data = d[d$date == max(d$date), ], hjust = 1) + 
  theme(legend.position = "none",
        axis.text = element_text(angle = 15, hjust = 1)) +
  scale_x_date(date_labels = "%Y-%m-%d")



y <- res$historical
d <- y["global"]



dd <- d %>% 
  as_tibble %>%
  filter(cases > 1000000) %>%
  group_by(country) %>%
  mutate(days_since_1m = as.numeric(date - min(date))) %>%
  ungroup 




breaks=c(1000, 10000, 20000, 50000, 500000,500000,5000000,20000000)


p <- ggplot(dd, aes(days_since_1m, cases, color = country)) +
  geom_smooth(method='lm', aes(group=1),
              data = dd, 
              color='grey10', linetype='dashed') +
  geom_line(size = 0.8) +
  geom_point(pch = 21, size = 1) +
  scale_y_log10(expand = expansion(add = c(0,0.1)), 
                breaks = breaks, labels = breaks) +
  scale_x_continuous(expand = expansion(add = c(0,1))) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(3,15,3,3,"mm")
  ) +
  coord_cartesian(clip = "off") +
  geom_shadowtext(aes(label = paste0(" ",country)), hjust=0, vjust = 0, 
                  data = . %>% group_by(country) %>% top_n(1,days_since_1m),
                  bg.color = "white") +
  labs(x = "Number of days since 1,000,000th case", y = "", 
       subtitle = "Total number of cases")
print(p)