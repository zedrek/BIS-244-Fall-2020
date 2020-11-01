## Intended to accompany BIS 244 Video Set 09

library(gapminder)
library(tidyverse)
library(ggrepel)
library(socviz)


## 8.3 Change the Appearance of Plots with Themes

if (!require("ggthemes")) install.packages("ggthemes")
library(ggthemes)

theme_set(theme_economist())
p4 + theme(legend.position="top")

theme_set(theme_wsj())

p4 + theme(plot.title = element_text(size = rel(0.6)),
           legend.title = element_text(size = rel(0.35)),
           plot.caption = element_text(size = rel(0.35)),
           legend.position = "top")

theme_set(theme_minimal())



## ----ch-08-theme-control, fig.cap='Controlling various theme elements directly.', fig.width=5, fig.height=5, out.width = "50%", warning=FALSE, fig.show = "hold"----

p4 + theme(legend.position = "top")

p4 + theme(legend.position = "top",
           plot.title = element_text(size=rel(2),
                                     lineheight=.5,
                                     family="Times",
                                     face="bold.italic",
                                     colour="orange"),
           axis.text.x = element_text(size=rel(1.1),
                                      family="Courier",
                                      face="bold",
                                      color="purple"))




## ----gss-custom-01, fig.cap = "A customized small multiple.", fig.height = 10, fig.width = 4----

yrs <- c(seq(1972, 1988, 4), 1993, seq(1996, 2016, 4))

mean_age <- gss_lon %>%
    filter(age %nin% NA && year %in% yrs) %>%
    group_by(year) %>%
    summarize(xbar = round(mean(age, na.rm = TRUE), 0))
mean_age$y <- 0.3

yr_labs <- data.frame(x = 85, y = 0.8,
                      year = yrs)


p <- ggplot(data = subset(gss_lon, year %in% yrs),
            mapping = aes(x = age))

p1 <- p + geom_density(fill = "gray20", color = FALSE,
                       alpha = 0.9, mapping = aes(y = ..scaled..)) +
    geom_vline(data = subset(mean_age, year %in% yrs),
               aes(xintercept = xbar), color = "white", size = 0.5) +
    geom_text(data = subset(mean_age, year %in% yrs),
              aes(x = xbar, y = y, label = xbar), nudge_x = 7.5,
              color = "white", size = 3.5, hjust = 1) +
    geom_text(data = subset(yr_labs, year %in% yrs),
              aes(x = x, y = y, label = year)) +
    facet_grid(year ~ ., switch = "y")

p1 + theme_minimal() +
    theme(plot.title = element_text(size = 16),
          axis.text.x= element_text(size = 12),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y = element_blank(),
          strip.background = element_blank(),
          strip.text.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(x = "Age",
         y = NULL,
         title = "Age Distribution of\nGSS Respondents")




## ----ridgeplot, fig.cap = "A ridgeplot version of the age distribution plot.", fig.height = 12, fig.width = 3.1----

library(ggridges)

p <- ggplot(data = gss_lon,
            mapping = aes(x = age, y = factor(year, levels = rev(unique(year)),
                                     ordered = TRUE)))

p + geom_density_ridges(alpha = 0.6, fill = "lightblue", scale = 1.5) +
    scale_x_continuous(breaks = c(25, 50, 75)) +
    scale_y_discrete(expand = c(0.01, 0)) + 
    labs(x = "Age", y = NULL,
         title = "Age Distribution of\nGSS Respondents") +
    theme_ridges() +
    theme(title = element_text(size = 16, face = "bold"))


## ----08-refine-plots-13---------------------------------------------
head(fredts)

fredts_m <- fredts %>% select(date, sp500_i, monbase_i) %>%
    gather(key = series, value = score, sp500_i:monbase_i)

head(fredts_m)


## ----08-refine-plots-14---------------------------------------------

p <- ggplot(data = fredts_m,
            mapping = aes(x = date, y = score,
                          group = series,
                          color = series))
p1 <- p + geom_line() + theme(legend.position = "top") +
    labs(x = "Date",
         y = "Index",
         color = "Series")

p <- ggplot(data = fredts,
            mapping = aes(x = date, y = sp500_i - monbase_i))

p2 <- p + geom_line() +
    labs(x = "Date",
         y = "Difference")


## ----fred5, fig.cap="Indexed series with a running difference below, using two separate plots.", fig.width = 8----

cowplot::plot_grid(p1, p2, nrow = 2, rel_heights = c(0.75, 0.25), align = "v")



## ----08-refine-plots-15---------------------------------------------
head(yahoo)


## ----yahoo-02, fig.cap="Redrawing as a connected scatterplot.", fig.width = 5, fig.height = 5----
p <- ggplot(data = yahoo,
            mapping = aes(x = Employees, y = Revenue))
p + geom_path(color = "gray80") +
    geom_text(aes(color = Mayer, label = Year),
              size = 3, fontface = "bold") +
    theme(legend.position = "bottom") +
    labs(color = "Mayer is CEO",
         x = "Employees", y = "Revenue (Millions)",
         title = "Yahoo Employees vs Revenues, 2004-2014") +
    scale_y_continuous(labels = scales::dollar) +
    scale_x_continuous(labels = scales::comma)



## ----yahoo-03, fig.cap="Plotting the ratio of revenue to employees against time.", fig.width = 6, fig.height = 4----

p <- ggplot(data = yahoo,
            mapping = aes(x = Year, y = Revenue/Employees))

p + geom_vline(xintercept = 2012) +
    geom_line(color = "gray60", size = 2) +
    annotate("text", x = 2013, y = 0.44,
             label = " Mayer becomes CEO", size = 2.5) +
    labs(x = "Year\n",
         y = "Revenue/Employees",
         title = "Yahoo Revenue to Employee Ratio, 2004-2014")



## ----08-refine-plots-16---------------------------------------------
head(studebt)


## ----studentpie-02, fig.cap="Faceting the pie charts.", fig.width = 6, fig.height = 4----

p_xlab <- "Amount Owed, in thousands of Dollars"
p_title <- "Outstanding Student Loans"
p_subtitle <- "44 million borrowers owe a total of $1.3 trillion"
p_caption <- "Source: FRB NY"

f_labs <- c(`Borrowers` = "Percent of\nall Borrowers",
            `Balances` = "Percent of\nall Balances")

p <- ggplot(data = studebt,
            mapping = aes(x = Debt, y = pct/100, fill = type))
p + geom_bar(stat = "identity") +
    scale_fill_brewer(type = "qual", palette = "Dark2") +
    scale_y_continuous(labels = scales::percent) +
    guides(fill = FALSE) +
    theme(strip.text.x = element_text(face = "bold")) +
    labs(y = NULL, x = p_xlab,
      caption = p_caption,
      title = p_title,
      subtitle = p_subtitle) +
    facet_grid(~ type, labeller = as_labeller(f_labs)) +
    coord_flip()


## ----studentpie-03, fig.cap="Debt distributions as horizontally segmented bars.", fig.width = 8, fig.height = 3----
library(viridis)

p <- ggplot(studebt, aes(y = pct/100, x = type, fill = Debtrc))
p + geom_bar(stat = "identity", color = "gray80") +
  scale_x_discrete(labels = as_labeller(f_labs)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis(discrete = TRUE) +
  guides(fill = guide_legend(reverse = TRUE,
                             title.position = "top",
                             label.position = "bottom",
                             keywidth = 3,
                             nrow = 1)) +
  labs(x = NULL, y = NULL,
       fill = "Amount Owed, in thousands of dollars",
       caption = p_caption,
       title = p_title,
       subtitle = p_subtitle) +
  theme(legend.position = "top",
        axis.text.y = element_text(face = "bold", hjust = 1, size = 12),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major.y = element_blank()) +
  coord_flip()




## ----08-refine-plots-2, fig.height=12, fig.width=14, out.width="100%"----

pdf(file = "~/Desktop/asa_sections.pdf", height = 8, width = 12)
p0 <- ggplot(data = asasec,
             mapping = aes(x = Year, y = Members, label = Sname, group = Sname)) 
p1 <- p0 + geom_line() 

p1 + facet_wrap( ~ reorder(Sname, -Members), ncol = 11) +
  scale_x_continuous(breaks = c(2006, 2010, 2014)) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = rel(0.65))) +
  labs(x = "")

dev.off()


