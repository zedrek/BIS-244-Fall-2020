## Intended to accompany BIS 244 Video Set 11

library(gapminder)
library(tidyverse)
library(ggrepel)
library(socviz)


## 7.1 Map U.S. State-Level Data
election %>% select(state, total_vote, r_points, pct_trump, party, census) %>%
    sample_n(5)

election <- as.data.frame(election)

# Making and Using Dem and Rep colors

# Hex color codes for Dem Blue and Rep Red
party_colors <- c("#2E74C0", "#CB454A") 

p0 <- ggplot(data = subset(election, st %nin% "DC"),
             mapping = aes(x = r_points,
                           y = reorder(state, r_points),
                           color = party))

p1 <- p0 + geom_vline(xintercept = 0, color = "gray30") +
    geom_point(size = 2)

p2 <- p1 + scale_color_manual(values = party_colors)

p3 <- p2 + scale_x_continuous(breaks = c(-30, -20, -10, 0, 10, 20, 30, 40),
                              labels = c("30\n (Clinton)", "20", "10", "0",
                                         "10", "20", "30", "40\n(Trump)"))

p3 + facet_wrap(~ census, ncol=1, scales="free_y") +
    guides(color=FALSE) + labs(x = "Point Margin", y = "") +
    theme(axis.text=element_text(size=8))

# Use ggforce to resize the facets proportional to the
# number of states in each region.
if (!require("ggforce")) install.packages("ggforce")
library(ggforce)
 
p3 + facet_col(~ census, scales="free_y", space = "free") +
 guides(color=FALSE) + labs(x = "Point Margin", y = NULL) +
 theme(axis.text=element_text(size=8))



# Let's create map data frame
install.packages(c("maps", "mapdata"))
library(maps)
us_states <- map_data("state")
head(us_states)
dim(us_states)


# Plotting map data

p <- ggplot(data = us_states,
            mapping = aes(x = long, y = lat,
                          group = group))

p + geom_polygon(fill = "white", color = "black")

# Coloring the states

p <- ggplot(data = us_states,
            aes(x = long, y = lat,
                group = group, fill = region))

p + geom_polygon(color = "gray90", size = 0.1) + guides(fill = FALSE)

# Improving the projection

p <- ggplot(data = us_states,
            mapping = aes(x = long, y = lat,
                          group = group, fill = region))

p + geom_polygon(color = "gray90", size = 0.1) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    guides(fill = FALSE)

# Creating a column to merge election data and map data on, then doing left_join()

election$region <- tolower(election$state)
us_states_elec <- left_join(us_states, election)

# Plotting the joined data set

p <- ggplot(data = us_states_elec,
            aes(x = long, y = lat,
                group = group, fill = party))

p + geom_polygon(color = "gray90", size = 0.1) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) 

# Make a theme for producing our map in the format we want

theme_map <- function(base_size=9, base_family="") {
    require(grid)
    theme_bw(base_size=base_size, base_family=base_family) %+replace%
        theme(axis.line=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank(),
              axis.title=element_blank(),
              panel.background=element_blank(),
              panel.border=element_blank(),
              panel.grid=element_blank(),
              panel.spacing=unit(0, "lines"),
              plot.background=element_blank(),
              legend.justification = c(0,0),
              legend.position = c(0,0)
              )
}

# Redoing the plot with our theme

p0 <- ggplot(data = us_states_elec,
             mapping = aes(x = long, y = lat,
                           group = group, fill = party))
p1 <- p0 + geom_polygon(color = "gray90", size = 0.1) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) 
p2 <- p1 + scale_fill_manual(values = party_colors) +
    labs(title = "Election Results 2016", fill = NULL)
p2 + theme_map() 

# Two versions of Percent Trump by State

p0 <- ggplot(data = us_states_elec,
             mapping = aes(x = long, y = lat, group = group, fill = pct_trump))

p1 <- p0 + geom_polygon(color = "gray90", size = 0.1) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) 

p1 + labs(title = "Trump vote") + theme_map() + labs(fill = "Percent")

p2 <- p1 + scale_fill_gradient(low = "white", high = "#CB454A") +
        labs(title = "Trump vote") 
p2 + theme_map() + labs(fill = "Percent")


# Two views of Trump vs Clinton share: a white midpoint, and a Purple America version.

p0 <- ggplot(data = us_states_elec,
             mapping = aes(x = long, y = lat, group = group, fill = d_points))

p1 <- p0 + geom_polygon(color = "gray90", size = 0.1) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) 

p2 <- p1 + scale_fill_gradient2() + labs(title = "Winning margins") 
p2 + theme_map() + labs(fill = "Percent")

p3 <- p1 + scale_fill_gradient2(low = "red", mid = scales::muted("purple"),
                                high = "blue", breaks = c(-25, 0, 25, 50, 75)) +
    labs(title = "Winning margins") 
p3 + theme_map() + labs(fill = "Percent")


# A Purple America version of Trump vs Clinton that excludes results from Washington, DC.

p0 <- ggplot(data = subset(us_states_elec,
                           region %nin% "district of columbia"),
             aes(x = long, y = lat, group = group, fill = d_points))

p1 <- p0 + geom_polygon(color = "gray90", size = 0.1) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) 

p2 <- p1 + scale_fill_gradient2(low = "red",
                                mid = scales::muted("purple"),
                                high = "blue") +
    labs(title = "Winning margins") 
p2 + theme_map() + labs(fill = "Percent")

## 7.2 America's Ur-choropleths

# Using county-based maps
county_map %>% sample_n(5)

county_data %>%
    select(id, name, state, pop_dens, pct_black) %>%
    sample_n(5)

county_full <- left_join(county_map, county_data, by = "id")


# US population density by county

p <- ggplot(data = county_full,
            mapping = aes(x = long, y = lat,
                          fill = pop_dens, 
                          group = group))

p1 <- p + geom_polygon(color = "gray90", size = 0.05) + coord_equal()

p2 <- p1 + scale_fill_brewer(palette="Blues",
                             labels = c("0-10", "10-50", "50-100", "100-500",
                                        "500-1,000", "1,000-5,000", ">5,000"))

p2 + labs(fill = "Population per\nsquare mile") +
    theme_map() +
    guides(fill = guide_legend(nrow = 1)) + 
    theme(legend.position = "bottom")


# Percent Black population by county

p <- ggplot(data = county_full,
            mapping = aes(x = long, y = lat, fill = pct_black, 
                          group = group))
p1 <- p + geom_polygon(color = "gray90", size = 0.05) + coord_equal()
p2 <- p1 + scale_fill_brewer(palette="Greens")

p2 + labs(fill = "US Population, Percent Black") +
    guides(fill = guide_legend(nrow = 1)) + 
    theme_map() + theme(legend.position = "bottom")



# Example of how population density and pct black drive graphs

orange_pal <- RColorBrewer::brewer.pal(n = 6, name = "Oranges")
orange_pal

orange_rev <- rev(orange_pal)
orange_rev

# Gun-related suicides by county; Reverse-coded population density by county

gun_p <- ggplot(data = county_full,
            mapping = aes(x = long, y = lat,
                          fill = su_gun6, 
                          group = group))

gun_p1 <- gun_p + geom_polygon(color = "gray90", size = 0.05) + coord_equal()

gun_p2 <- gun_p1 + scale_fill_manual(values = orange_pal)

gun_p2 + labs(title = "Gun-Related Suicides, 1999-2015",
              fill = "Rate per 100,000 pop.") +
    theme_map() +
    theme(legend.position = "bottom")


pop_p <- ggplot(data = county_full,
            mapping = aes(x = long, y = lat,
                          fill = pop_dens6, 
                          group = group))

pop_p1 <- pop_p + geom_polygon(color = "gray90", size = 0.05) + coord_equal()

pop_p2 <- pop_p1 + scale_fill_manual(values = orange_rev)

pop_p2 + labs(title = "Reverse-coded Population Density",
              fill = "People per square mile") +
    theme_map() +
    theme(legend.position = "bottom")


## 7.3 Statebins
if (!require("statebins")) install.packages("statebins")
library(statebins)

p <- ggplot(data = us_states_elec,
            mapping = aes(state = state, fill = party))

p + geom_statebins() +
    scale_fill_manual(values = party_colors) +
    labs(title = "Election Results 2016", fill = NULL) +
    theme_map() 


## 7.4 Small-Multiple Maps

opiates

opiates$region <- tolower(opiates$state)
opiates_map <- left_join(us_states, opiates)

# A small multiple map. States in grey reported too few deaths for a reliable population estimate in that year

library(viridis)

p0 <- ggplot(data = subset(opiates_map, year > 1999),
             mapping = aes(x = long, y = lat,
                 group = group,
                 fill = adjusted))

p1 <- p0 + geom_polygon(color = "gray90", size = 0.05) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) 

p2 <- p1 + scale_fill_viridis_c(option = "plasma")

p2 + theme_map() + facet_wrap(~ year, ncol = 3) +
    theme(legend.position = "bottom",
          strip.background = element_blank()) +
    labs(fill = "Death rate per 100,000 population ",
         title = "Opiate Related Deaths by State, 2000-2014")  

## 7.5 Is Your Data Really Spatial


p <- ggplot(data = opiates,
            mapping = aes(x = year, y = adjusted,
                          group = state))
p + geom_line(color = "gray70") 


# Faceting by Census Division
p0 <- ggplot(data = drop_na(opiates, division_name),
            mapping = aes(x = year, y = adjusted))
            
p1 <- p0 + geom_line(color = "gray70", 
              mapping = aes(group = state)) 

p2 <- p1 + geom_smooth(mapping = aes(group = division_name),
                       se = FALSE)
p3 <- p2 + geom_text_repel(data = subset(opiates,
                                         year == max(year) & abbr !="DC"),
                     mapping = aes(x = year, y = adjusted, label = abbr),
                     size = 1.8, segment.color = NA, nudge_x = 30) +
         coord_cartesian(c(min(opiates$year), 
                    max(opiates$year)))

p3 + labs(x = "", y = "Rate per 100,000 population",
       title = "State-Level Opiate Death Rates by Census Division, 1999-2014") +
    facet_wrap(~ reorder(division_name, -adjusted, na.rm = TRUE), nrow  = 3)


