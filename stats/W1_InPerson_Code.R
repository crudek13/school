#install and load packages
install.packages("nycflights13")
library(nycflights13)

#flights dataset
  #4 other datasets in the nycflights package
nycflights13::flights
  
  
#which flight had the longest arrival delay
flights %>%
  arrange(desc(arr_delay))


#avg arrival delay of AA
#whenever you have a missing value and use a summarize function its always going to return "NA"
flights %>%
  filter(carrier == "AA") %>%
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE))


#all of the airlines avg arrival delays
flights %>%
  group_by(carrier) %>%
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE)) %>%
  # can use desc() or just add a negative in front
  arrange(-avg_delay)

#whats in the dataset
?flights

#TIP: ctrl shift M to add a pipe

#which destination does each carrier go to the most
flights %>%
  count(carrier, dest) %>%
  arrange(-n) %>%
  group_by(carrier) %>%
  #different ways to do this
  filter(n == max(n))
  #top_n(n=1, wt=n)


top_dest <- flights %>%
  group_by(carrier, dest) %>%
  summarize(num_flights = n()) %>%
  filter(num_flights == max(num_flights))


### JOINING DATA 

view(airlines)
view(airports)

top_dest %>%
  left_join(airlines, by = "carrier")

#learn how to do join with unmatching column names
?left_join

top_dest %>%
  left_join(airlines, by = "carrier") %>%
  left_join(airports, by = c("dest"="faa"),  suffix=c("_carrier", "_airport")) %>%
  ungroup() %>%
  select(Carrier = name_carrier, Airport = name_airport, 'Number of Flights' =  num_flights)




