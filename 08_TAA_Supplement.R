## Intended to accompany BIS 244 Video Set 07

library(gapminder)
library(here)
library(tidyverse)
library(socviz)


## 5.1 Use Pipes to Summarize Data

rel_by_region <- gss_sm %>%
    group_by(bigregion, religion) %>%
    summarize(N = n()) %>%
    mutate(freq = N / sum(N),
           pct = round((freq*100), 0))

# Is the same as 

temp1 <- group_by(gss_sm, bigregion, religion)
temp2 <- summarize(temp1, N = n())
rel_by_region2 <- mutate(temp2, freq = N / sum(N), pct = round((freq*100), 0))


# Look at gss_sm vs. temp1 to see what group_by does

temp1
gss_sm

# Look at temp2

temp2

# Look at rel_by_reion vs. rel_by_region2

View(rel_by_region)
View(rel_by_region2)

# Yet a 3rd way to do the same thing:

rel_by_region3 <- mutate(summarize(group_by(gss_sm, bigregion, religion),N = n()),
                         freq = N / sum(N), pct = round((freq*100), 0))


# Back to our data

rel_by_region


# Do the pct values sum to 100?

rel_by_region %>% group_by(bigregion) %>%
    summarize(total = sum(pct))      


# Plotting our summarisation

p <- ggplot(rel_by_region, aes(x = bigregion, y = pct, fill = religion))
p + geom_col(position = "dodge2") +
    labs(x = "Region",y = "Percent", fill = "Religion") +
    theme(legend.position = "top")      

# BTW, "dodge2" is a newer way with thinner columns of doing

p + geom_col(position = "dodge") +
  labs(x = "Region",y = "Percent", fill = "Religion") +
  theme(legend.position = "top")      



# Using facet_grid() to put graphs for each region side-by-side

p <- ggplot(rel_by_region, aes(x = religion, y = pct, fill = religion))
p + geom_col(position = "dodge") +
  labs(x = NULL, y = "Percent", fill = "Religion") +
  guides(fill = FALSE) + 
  facet_grid(~ bigregion)      

# ... and flippling the x- and y- axes....

p + geom_col(position = "dodge") +
    labs(x = NULL, y = "Percent", fill = "Religion") +
    guides(fill = FALSE) + 
    coord_flip() + 
    facet_grid(~ bigregion)   


## 5.2 Continuous Variables by Group or Category

# Let's look at organdata

View(organdata)

@ ANother way of getting a quick view of the first 6 columns

organdata %>% select(1:6) %>% sample_n(size = 10)      


# A Bad graph
p <- ggplot(data = organdata,
            mapping = aes(x = year, y = donors))
p + geom_point()      

# Very similar to the problem we saw in section 4.2, which we fixed by grouping by country...

p <- ggplot(data = gapminder,
            mapping = aes(x = year,
                          y = gdpPercap))
p + geom_line()       

# We take a similar approach here
p <- ggplot(data = organdata,
            mapping = aes(x = year, y = donors))
p + geom_line(aes(group = country)) + 
  facet_wrap(~ country)      


# Let's summarize this one one graph using geom_boxplot()
p <- ggplot(data = organdata,
            mapping = aes(x = country, y = donors))
p + geom_boxplot()      

# What is being summarized in each "box"?

# Flipping the axes to make the names more readable...
p + geom_boxplot() + coord_flip()      


# ... and reordering the country listings by "donors" mean values
p <- ggplot(data = organdata,
            mapping = aes(x = reorder(country, donors, na.rm=TRUE),
                          y = donors))
p + geom_boxplot() +
    labs(x=NULL) +
    coord_flip()      

# Could also do reorder() by median values...

p <- ggplot(data = organdata,
            mapping = aes(x = reorder(country, donors, median,na.rm=TRUE),
                          y = donors))
p + geom_boxplot() +
  labs(x=NULL) +
  coord_flip()      



# Easy to add colors using "fill" parameter
p <- ggplot(data = organdata,
            mapping = aes(x = reorder(country, donors, na.rm=TRUE),
                          y = donors, fill = world))
p + geom_boxplot() + labs(x=NULL) +
    coord_flip() + theme(legend.position = "top")      


# Same thing, but with geom_point() or geom_jitter() instead of goem_boxplot()
p <- ggplot(data = organdata,
            mapping = aes(x = reorder(country, donors, na.rm=TRUE),
                          y = donors, color = world))
p + geom_point() + labs(x=NULL) +
    coord_flip() + theme(legend.position = "top")

p + geom_jitter() + labs(x=NULL) +
    coord_flip() + theme(legend.position = "top")      

# And can control the amount of jitter

p + geom_jitter(position = position_jitter(width=0.15)) +
  labs(x=NULL) + coord_flip() + theme(legend.position = "top")      

# When would using individual points like this be infeasible?


## ----summarize------------------------------------------------------
by_country <- organdata %>% group_by(consent_law, country) %>%
    summarize(donors_mean= mean(donors, na.rm = TRUE),
              donors_sd = sd(donors, na.rm = TRUE),
              gdp_mean = mean(gdp, na.rm = TRUE),
              health_mean = mean(health, na.rm = TRUE),
              roads_mean = mean(roads, na.rm = TRUE),
              cerebvas_mean = mean(cerebvas, na.rm = TRUE))

by_country


## ----better_summarize-----------------------------------------------
by_country <- organdata %>% 
  group_by(consent_law, country) %>%
    summarize_if(is.numeric, 
                 list(~ mean(., na.rm = TRUE), 
                      ~ sd(., na.rm = TRUE))) %>%
    ungroup()

by_country


# Regardless of how we get there, can use mean and sd with geom_point()
p <- ggplot(data = by_country,
            mapping = aes(x = donors_mean, 
                          y = reorder(country, donors_mean),
                          color = consent_law))
p + geom_point(size=3) +
    labs(x = "Donor Procurement Rate",
         y = "", color = "Consent Law") +
    theme(legend.position="top")      


# ... or can use facet_wrap to get separate graphs for informed and presumed
p <- ggplot(data = by_country,
            mapping = aes(x = donors_mean,
                          y = reorder(country, donors_mean)))

p + geom_point(size=3) +
    facet_wrap(~ consent_law, scales = "free_y", ncol = 1) +
    labs(x= "Donor Procurement Rate",
         y= "")       


# And can explicitly use mean and SD with geom_pointrange()
p <- ggplot(data = by_country, mapping = aes(x = reorder(country,
              donors_mean), y = donors_mean))

p + geom_pointrange(mapping = aes(ymin = donors_mean - donors_sd,
       ymax = donors_mean + donors_sd)) +
     labs(x= "", y= "Donor Procurement Rate") + coord_flip()      


## ----05-tables-and-labels-15----------------------------------------
p <- ggplot(data = by_country,
            mapping = aes(x = roads_mean, y = donors_mean))
p + geom_point() + geom_text(mapping = aes(label = country))
      


## ----05-tables-and-labels-16----------------------------------------
p <- ggplot(data = by_country,
            mapping = aes(x = roads_mean, y = donors_mean))

p + geom_point() + geom_text(mapping = aes(label = country), hjust = 0)
      


## ----05-tables-and-labels-17----------------------------------------
library(ggrepel)      


## ----05-tables-and-labels-18----------------------------------------
elections_historic %>% select(2:7)       


## ----05-tables-and-labels-19, layout = 'l-screen-inset', fig.height=10, fig.width=12----
p_title <- "Presidential Elections: Popular & Electoral College Margins"
p_subtitle <- "1824-2016"
p_caption <- "Data for 2016 are provisional."
x_label <- "Winner's share of Popular Vote"
y_label <- "Winner's share of Electoral College Votes"

p <- ggplot(elections_historic, aes(x = popular_pct, y = ec_pct,
                                    label = winner_label))

p + geom_hline(yintercept = 0.5, size = 1.4, color = "gray80") +
    geom_vline(xintercept = 0.5, size = 1.4, color = "gray80") +
    geom_point() +
    geom_text_repel() +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = x_label, y = y_label, title = p_title, subtitle = p_subtitle,
         caption = p_caption)      


## ----05-tables-and-labels-20----------------------------------------
p <- ggplot(data = by_country,
            mapping = aes(x = gdp_mean, y = health_mean))

p + geom_point() +
    geom_text_repel(data = subset(by_country, gdp_mean > 25000),
                    mapping = aes(label = country))

p <- ggplot(data = by_country,
            mapping = aes(x = gdp_mean, y = health_mean))

p + geom_point() +
    geom_text_repel(data = subset(by_country,
                                  gdp_mean > 25000 | health_mean < 1500 |
                                  country %in% "Belgium"),
                    mapping = aes(label = country))      


## ----05-tables-and-labels-21----------------------------------------
organdata$ind <- organdata$ccode %in% c("Ita", "Spa") &
                    organdata$year > 1998

p <- ggplot(data = organdata,
            mapping = aes(x = roads,
                          y = donors, color = ind))
p + geom_point() +
    geom_text_repel(data = subset(organdata, ind),
                    mapping = aes(label = ccode)) +
    guides(label = FALSE, color = FALSE)      


## ----05-tables-and-labels-22----------------------------------------
p <- ggplot(data = organdata, mapping = aes(x = roads, y = donors))
p + geom_point() + annotate(geom = "text", x = 91, y = 33,
                            label = "A surprisingly high \n recovery rate.",
                            hjust = 0)
      


## ----05-tables-and-labels-23----------------------------------------
p <- ggplot(data = organdata,
            mapping = aes(x = roads, y = donors))
p + geom_point() +
    annotate(geom = "rect", xmin = 125, xmax = 155,
             ymin = 30, ymax = 35, fill = "red", alpha = 0.2) + 
    annotate(geom = "text", x = 157, y = 33,
             label = "A surprisingly high \n recovery rate.", hjust = 0)      


## ----05-tables-and-labels-24----------------------------------------

p <- ggplot(data = organdata,
            mapping = aes(x = roads,
                          y = donors,
                          color = world))
p + geom_point()


## ----05-tables-and-labels-25----------------------------------------

p <- ggplot(data = organdata,
            mapping = aes(x = roads,
                          y = donors,
                          color = world))
p + geom_point() +
    scale_x_log10() +
    scale_y_continuous(breaks = c(5, 15, 25),
                       labels = c("Five", "Fifteen", "Twenty Five"))


## ----05-tables-and-labels-26----------------------------------------

p <- ggplot(data = organdata,
            mapping = aes(x = roads,
                          y = donors,
                          color = world))
p + geom_point() +
    scale_color_discrete(labels =
                             c("Corporatist", "Liberal",
                               "Social Democratic", "Unclassified")) +
    labs(x = "Road Deaths",
         y = "Donor Procurement",
        color = "Welfare State")



## ----05-tables-and-labels-27----------------------------------------

p <- ggplot(data = organdata,
            mapping = aes(x = roads,
                          y = donors,
                          color = world))
p + geom_point() +
    labs(x = "Road Deaths",
         y = "Donor Procurement") +
    guides(color = FALSE)


