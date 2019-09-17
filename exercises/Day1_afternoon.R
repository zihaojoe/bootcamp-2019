gapminder <- read.csv(here::here("data", "gapminder5.csv"))
gapminder$country <- as.character(gapminder$country)
gapminder$continent <- as.character(gapminder$continent)
str(gapminder)
### LOOP
# create a vector of values that you want to repeat the function for
obs <- 1:nrow(gapminder)

# initialize the for loop with `for (i in vector)` 
for (i in obs) { # the function to repeat is enclosed in braces {}
    gapminder[i, "gdp"] <- gapminder[i, "pop"] * gapminder[i, "gdpPercap"]
}

# natural log of GDP per capita and population
obs <- 1:nrow(gapminder)
for (i in obs) { 
    gapminder[i, "log_gdpPercap"] <- log(gapminder[i, "gdpPercap"])
    gapminder[i, "log_pop"] <- log(gapminder[i, "pop"])
}

# has life expentancy increased over time?
years <- unique(gapminder$year)

for (i in years) {
    mean_le <- mean(gapminder$lifeExp[gapminder$year == i], 
                    na.rm = T)
    print(paste0(i, ": ", mean_le))
}   
# mean life expectancy by continent
conts <- unique(gapminder$continent)

for (i in conts) {
    mean_le <- mean(gapminder$lifeExp[gapminder$continent == i], 
                    na.rm = T)
    print(paste0(i, ": ", mean_le))
}
    
# the nested for loop
## What is the mean life expectancy for each continent for each year
for (i in conts) {
    print(paste0("Continent: ", i))
    for (j in years) {
        mean_le <- mean(gapminder$lifeExp[gapminder$continent == i & 
                                              gapminder$year == j], 
                        na.rm = T)
        print(paste0(j, ": ", mean_le))
    }
}

## standard deviation (sd) for life expectancy for each continent for each year
for (i in conts) {
    print(paste0("Continent: ", i))
    for (j in years) {
        mean_le <- mean(gapminder$lifeExp[gapminder$continent == i & 
                                              gapminder$year == j], 
                        na.rm = T)
        print(paste0(j, ": ", mean_le))
    }
}

### APPLY FUNCTION
# want to find the mean for each stat in gapminder
vars <- gapminder[, c("lifeExp", "pop", "gdpPercap")]   # 1 = row or 2 = column
apply(vars, 2, mean)

### while
#the standard deviation for life expectancy for each year between 1987 and 2002 (inclusive)
i <-  1987 # define the interator

while (i <= 2002) {
    sd_le <- sd(gapminder$lifeExp[gapminder$year == i])
    print(paste0(i, ": ", sd_le)
    )
    i <- i + 1 # increase the iterator by the interval between years
}

### if/else
# continents have a mean life expectancy greater than 70 years
threshold <- 70

for (i in unique(gapminder$continent)) {
    tmp <- mean(gapminder$lifeExp[gapminder$continent==i])
    
    if (tmp < threshold) {
        print(paste("Mean Life Expectancy in", i, "is less than", threshold))
    } else {
        print(paste("Mean Life Expectancy in", i, "is greater than", threshold))
    }
}

# mean population for years greater than or equal to 1987
for (i in years) {
    if (i >= 1987) {
        mean_pop <- mean(gapminder$pop[gapminder$year == i])
        print(paste0(i, ": ", mean_pop))
    } else {
        print("Sorry, year is less than 1987")
    }
}

### CREATE YOUR OWN FUNCTION
# Write a function that reports the mean, median, minimum, and maximum for life expectancy for a continent in gapminder
report_stats <-
    function(df, variable, continent){
        var <- df[[variable]][df$continent == continent]
        min_le <- min(var)
        max_le <- min(var)
        mean_le <- mean(var)
        median_le <- median(var)
        cat("Continent:", continent,
            "\nMinimum Life expectancy:", min_le,
            "\nMaximum Life expectancy:", max_le,
            "\nMean Life expectancy:", mean_le,
            "\nMedian Life expectancy:", median_le)
    }
        
report_stats(gapminder, "lifeExp", "Asia")
