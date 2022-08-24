# Creating-a-tidy-data-
# Creating a tidy data 
### make sure to install all the packages below. 
pacman::p_load(datasets, pacman, psych, rio, tidyverse, tsibble, lubridate, XML)

### using the sunsport dataset
?sunspots
view(sunspots)
head(sunspots)
plot(sunspots)



# TIME SERIES DATA #########################################

# About the "sunspots" dataset
?sunspots       # Get info on "sunspots" dataset
sunspots        # See full dataset in time series format
head(sunspots)  # Show first few observations
plot(sunspots)  # Plot data using base graphics

# Tidy the data
tidy_ts <- sunspots %>%  
  as_tsibble() %>%        # Convert to timeseries tibble
  mutate(                 # Create new variables
    year = year(index),   # Create a year column
    month = month(index)  # Create a month column
  ) %>%
  select(                 # Select, reorder, rename vars
    date = index,         # Rename "index" to "date"
    year,                 # Use "year" as second variable
    month,                # Use "month" as third variable
    spots = value         # Rename "value" as "spots"
  ) %>%
  print()                 # Show data

# Graph the tidy data by decade
tidy_ts %>%
  index_by(                            # By decade
    decade = ~ floor_date(tidy_ts$date, years(10))
  ) %>%
  summarise(mean_s = mean(spots)) %>%  # Mean for decade
  ggplot(aes(decade, mean_s)) +        # Plot means
  geom_point() +                       # Scatterplot
  geom_smooth() +                      # Smoother
  ylab("Sunspots: Mean by Decade")     # Label
 


# Parse data
tidy_xml <- xmlParse("XMLdata.xml")

# Convert data to tidy format
tidy_xml <- XML:::xmlAttrsToDataFrame(getNodeSet(tidy_xml,
                                                 path='//Row')) %>%                # Read data
  as_tibble() %>%                    
  mutate_all(as.character) %>%     
  `colnames<-` (.[1,]) %>%            
  slice(-1) %>%                      
  mutate(birthdate = mdy(birthdate))   

### showing the data 
tidy_xml

view(tidy_xml)
attach(tidy_xml)

### graphing the birthdays 
tidy_xml %>% ggplot(mapping = aes(x = birthdate)) + geom_histogram()


### Compound values 
names <- c("Doris Vorome", "Patrick Mensah", "Selorm Patricia", "Elorm Mensah", 
           "Samuel Mensah" , "Joyce Mensah")

### Tidying the names 
data <- names %>% enframe() %>% separate(value, c("First", "Last"))%>% print()
  
                  
