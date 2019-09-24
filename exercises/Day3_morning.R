source(here::here("data/day3_objects.R"))

# Scatterplot
ggplot(data = gapminder07) + 
    geom_point(mapping = aes(x = gdpPercap, y = lifeExp))

# Set labels
ggplot(gapminder07) + 
    geom_point(aes(x = gdpPercap, y = lifeExp)) + 
    labs(title = "Relationship between life expectancy and GDP per capita in 2007", 
         x = "GDP per capita", y = "Life expectancy")
ggplot(gapminder07) + 
    geom_point(aes(x = log(pop), y = log(gdpPercap))) + 
    labs(title = "Relationship between GDP per capita and population in 2007", x = "Logged GDP per capita", y = "Logged life expectancy")

# data with pipe operator
long_gen %>% 
    group_by(datetime) %>% 
    summarise(output=sum(output)) %>% 
    ggplot() + 
    geom_col(aes(x=datetime, y=output)) + 
    labs(title="Total energy generated, by hour", x="Hour", y="Output (MW)")

long_gen %>% 
    filter(source=="large_hydro"|source=="small_hydro") %>%
    group_by(datetime) %>% 
    summarise(output=sum(output)) %>% 
    ggplot() + 
    geom_col(aes(x=datetime, y=output)) + 
    labs(title="Total energy generated, by hour", x="Hour", y="Output (MW)")

imports %>%
    ggplot() + 
    geom_line(aes(x=datetime, y=imports), size=1.2, col="red") + 
    labs(title="Energy imports over time", x="Hour", y="Amount imported (MW)")

generation %>% 
    ggplot() + 
    geom_area(aes(x=datetime, y=wind), fill="darkblue") + 
    labs(title="Hourly wind power generation, Sept 3-9", x="Hour", y="Output (MW)")

generation %>% 
    ggplot() + 
    geom_line(aes(x=datetime, y=large_hydro), col="turquoise3") + 
    geom_smooth(aes(x=datetime, y=large_hydro)) + 

long_merged_energy %>% 
    group_by(source) %>% 
    summarise(output=sum(output)) %>%
    ggplot() + 
    geom_col(aes(x=source, y=output), fill="darkred") + 
    geom_hline(aes(yintercept=mean(output))) + 
    geom_hline(aes(yintercept=median(output)), col = 'purple') +
    labs(title="Total output per energy source over Sept 3-9", y="Output (MW)", x="Source")

# Visualizing grouped data
# Colors and fill
long_merged_energy %>%
    ggplot() + 
    geom_line(aes(x=datetime, y=output, group=source, col=source)) + 
    labs(title="Output by energy source over time", subtitle="Hourly data from September 3-9, 2018", x="Hour", y="Output (MW)")

long_merged_energy %>%
    filter(source=="wind"|source=="solar"|source=="geothermal") %>% 
    ggplot() + 
    geom_line(aes(x=datetime, y=output, group=source, col=source), size=1.5) +  # group????????????
    labs(title="Wind vs. Solar vs. Geothermal generation", subtitle="Hourly data from September 3-9, 2018", x="Hour", y="Output (MW)")