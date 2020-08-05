---
title: Last Christmas I gave you my heart
description: TLDR; There is no such thing as a summer classic - but plenty of christmas classics!
toc: true
branch: master
badges: false
comments: true
author: Mikkel Freltoft Krogsholm
categories: [spotify, charts, christmas, summer, R, jupyter]
---

This blogpost looks at music. 

I have downloaded most of the chart data from [spotifycharts](https://spotifycharts.com/) and stored them in a SQLite database. 

Specifically I will look at "seasonal classics". It turns out that there are no summer classics - only christmas classics. It looks like Summer Hits are temporary and fleeing where Christmas songs are more permanent. 

### Setup

First I load the needed libraries and created a connection to my local database with music.


```R
suppressPackageStartupMessages(library(tidyverse))
library(RSQLite)

options(repr.plot.width=15, repr.plot.height=7.5, scipen = 999)

con <- dbConnect(RSQLite::SQLite(), "spotify.db")
```

### Collect the data

I will look a three countries: Germany (de), Denmark (dk) and Great Britain (gb). And then create a plot that shows how many Christmas songs are the same year on year, but summer songs are not.


```R
# Make a reference to the table in the DB
tblref <- tbl(con, "top200")
```


```R
# Collect the data
dfhere <- tblref %>%
    filter(region %in% c("de", "dk", "gb")) %>% # filter relevant countries
    mutate(m = str_sub(date, 6, 7), # create a month column
           y = str_sub(date, 1, 4), # create a year column
           ym = paste(y,m)) %>% # create a yearmonth  column
    group_by(region, y, m, ym, `Track Name`, Artist, URL) %>% # Group by region, track and year month
    summarise(streams = sum(Streams, na.rm = TRUE)) %>% # sum up the streams for a track per year month 
    ungroup() %>% # ungroup
    collect() # Import the data into R's memory

# Lets have a look at the first few lines
head(dfhere)
```


<table>
<caption>A tibble: 6 × 8</caption>
<thead>
	<tr><th scope=col>region</th><th scope=col>y</th><th scope=col>m</th><th scope=col>ym</th><th scope=col>Track Name</th><th scope=col>Artist</th><th scope=col>URL</th><th scope=col>streams</th></tr>
	<tr><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;int&gt;</th></tr>
</thead>
<tbody>
	<tr><td>de</td><td>2017</td><td>01</td><td>2017 01</td><td>'Till I Collapse</td><td>Eminem       </td><td>https://open.spotify.com/track/6yr8GiTHWvFfi4o6Q5ebdT</td><td> 950929</td></tr>
	<tr><td>de</td><td>2017</td><td>01</td><td>2017 01</td><td>10 Jahre        </td><td>Gzuz         </td><td>https://open.spotify.com/track/3KbCjPkewCmJt09FYtyVK5</td><td>2921941</td></tr>
	<tr><td>de</td><td>2017</td><td>01</td><td>2017 01</td><td>24 Karat        </td><td>Kollegah     </td><td>https://open.spotify.com/track/77NydtMB8mylSXby3y5aDG</td><td>  24446</td></tr>
	<tr><td>de</td><td>2017</td><td>01</td><td>2017 01</td><td>24K Magic       </td><td>Bruno Mars   </td><td>https://open.spotify.com/track/6b8Be6ljOzmkOmFslEb23P</td><td>2067541</td></tr>
	<tr><td>de</td><td>2017</td><td>01</td><td>2017 01</td><td>7 Years         </td><td>Lukas Graham </td><td>https://open.spotify.com/track/7129iqBafaphfc3WPCGC0L</td><td> 905040</td></tr>
	<tr><td>de</td><td>2017</td><td>01</td><td>2017 01</td><td>80 Millionen    </td><td>Max Giesinger</td><td>https://open.spotify.com/track/14SZQ3k55sfzGLuv9WZ4Xe</td><td> 154202</td></tr>
</tbody>
</table>



With the monthly data now in memory I will pick the top 100 tracks per month


```R
top <- dfhere %>%
    group_by(region, y, m, ym) %>%
    top_n(n = 100, wt = streams) %>%
    ungroup()

# yms <- top$ym %>% unique() %>% sort()
# top$ym <- factor(top$ym, levels = yms)

head(top)
```


<table>
<caption>A tibble: 6 × 8</caption>
<thead>
	<tr><th scope=col>region</th><th scope=col>y</th><th scope=col>m</th><th scope=col>ym</th><th scope=col>Track Name</th><th scope=col>Artist</th><th scope=col>URL</th><th scope=col>streams</th></tr>
	<tr><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;int&gt;</th></tr>
</thead>
<tbody>
	<tr><td>de</td><td>2017</td><td>01</td><td>2017 01</td><td>10 Jahre    </td><td>Gzuz       </td><td>https://open.spotify.com/track/3KbCjPkewCmJt09FYtyVK5</td><td>2921941</td></tr>
	<tr><td>de</td><td>2017</td><td>01</td><td>2017 01</td><td>24K Magic   </td><td>Bruno Mars </td><td>https://open.spotify.com/track/6b8Be6ljOzmkOmFslEb23P</td><td>2067541</td></tr>
	<tr><td>de</td><td>2017</td><td>01</td><td>2017 01</td><td>Ahnma       </td><td>Beginner   </td><td>https://open.spotify.com/track/5CXjevTAQIYO1Z0yyiOOno</td><td>1788179</td></tr>
	<tr><td>de</td><td>2017</td><td>01</td><td>2017 01</td><td>Alarm       </td><td>Anne-Marie </td><td>https://open.spotify.com/track/0OwX5aROoW1Iip8FV51Efg</td><td>1351960</td></tr>
	<tr><td>de</td><td>2017</td><td>01</td><td>2017 01</td><td>All Night   </td><td>The Vamps  </td><td>https://open.spotify.com/track/1yNyoWWWikbLhwIGWjZuDW</td><td>2698589</td></tr>
	<tr><td>de</td><td>2017</td><td>01</td><td>2017 01</td><td>All Time Low</td><td>Jon Bellion</td><td>https://open.spotify.com/track/1CnPYaKxTVb4LWOtiGOm0m</td><td>2352737</td></tr>
</tbody>
</table>



Now we need to divide the data into different data frames. We need these to create the visualization.


```R
# First I create a table with repeaters, i.e. with songs that appear in the same month across the years more than once:
repeaters <- top %>%
    group_by(region) %>%
    count(m, `Track Name`, Artist, URL) %>%
    filter(n > 1) %>%
    select(region, URL) %>%
    ungroup()

# Then I filter the data to only contain data for the repeathers
toprep <- top %>%
    inner_join(repeaters, by = c("region", "URL"))

# Then I create a table with the songs that have max hits in december
xmasmax <- toprep %>%
    group_by(region, URL) %>%
    filter(streams == max(streams),
           m == 12) %>%
    select(region, URL) %>%
    ungroup()

# And use this table to create a data frame for repeater songs where
# the most streams are not in december
noxmas <- toprep %>% anti_join(xmasmax, by = c("region", "URL")) %>% distinct()

# And a dataframe where most streams are in december
xmas <- toprep %>% inner_join(xmasmax, by = c("region", "URL")) %>% distinct()

head(xmas)
```


<table>
<caption>A tibble: 6 × 8</caption>
<thead>
	<tr><th scope=col>region</th><th scope=col>y</th><th scope=col>m</th><th scope=col>ym</th><th scope=col>Track Name</th><th scope=col>Artist</th><th scope=col>URL</th><th scope=col>streams</th></tr>
	<tr><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;int&gt;</th></tr>
</thead>
<tbody>
	<tr><td>de</td><td>2017</td><td>12</td><td>2017 12</td><td>All I Want for Christmas Is You    </td><td>Mariah Carey  </td><td>https://open.spotify.com/track/0bYg9bo50gSsH3LtXe2SQn</td><td>8895808</td></tr>
	<tr><td>de</td><td>2017</td><td>12</td><td>2017 12</td><td>Christmas Lights                   </td><td>Coldplay      </td><td>https://open.spotify.com/track/4fzyvSu73BhGvi96p2zwjL</td><td>2321093</td></tr>
	<tr><td>de</td><td>2017</td><td>12</td><td>2017 12</td><td>Christmas Time                     </td><td>Bryan Adams   </td><td>https://open.spotify.com/track/0hnErtVKN1JyvfBNfpWDpO</td><td>2906518</td></tr>
	<tr><td>de</td><td>2017</td><td>12</td><td>2017 12</td><td>Feliz Navidad                      </td><td>José Feliciano</td><td>https://open.spotify.com/track/7taXf5odg9xCAZERYfyOkS</td><td>2498182</td></tr>
	<tr><td>de</td><td>2017</td><td>12</td><td>2017 12</td><td>Hallelujah                         </td><td>Pentatonix    </td><td>https://open.spotify.com/track/550rQQCGkrTzvp4SfpOPzx</td><td>3738649</td></tr>
	<tr><td>de</td><td>2017</td><td>12</td><td>2017 12</td><td>Happy Xmas (War Is Over) - 2010 Mix</td><td>John Lennon   </td><td>https://open.spotify.com/track/3zJw3rugfpVrmBeDDnUYzy</td><td>4203958</td></tr>
</tbody>
</table>



### Visualize the data

#### Year on year repeaters
Let's first look at the songs that repeat year on year. As you can see the songs normally have a more or less sharp rise and then a long slow decline. It is this decline that makes it possible for the song to appear in the same months year on year, although on a very different scale. 


```R
# I need a little helper to create pretty x-axis on the charts
yms <- top$ym %>% unique() %>% sort()
yms_labs <- str_c(str_sub(yms, -2,-1), " '", str_sub(yms, 3, 4))
yms_df <- top %>% distinct(ym)
```


```R
ggplot() + 
    # Then I plot the non-xmas data in green
    geom_line(data = noxmas, aes(ym, streams, group = URL), show.legend = FALSE, color = "darkgreen", 
              linetype = "dashed", alpha = .4) + 
    geom_point(data = noxmas, aes(ym, streams, group = URL), show.legend = FALSE, color = "darkgreen", 
               alpha = .4) + 
    
    # And some chart styling
    scale_x_discrete(labels = yms_labs, limits = yms) + 
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45)) +
    facet_wrap(facets = ~ region, ncol = 1, scales = "free_y") +
    labs(x = "", y = "Streams per month")
```


![]({ site.baseurl }}/images/lastchristmas/output_14_0.png)


#### Christmas repeaters
It looks a bit different if we plot the Christmas songs. It becomes very obvious that the Christmas repeaters are limited to Christmas. They also do not follow the same path as the other songs (sharp increase, long slow decrease). They come to life pretty high on the charts in december (some in november) and then they quickly die out and wait for Christmas to come again. I have marked decemberr with a red dashed line.


```R
ggplot() + 

    # First I create a vertical red dashed line for every december month
    geom_vline(xintercept = c("2017 12", "2018 12", "2019 12"), linetype = "dashed", color = "red") +
    
    # And the christmas data in blue
    geom_line(data = xmas, aes(ym, streams, group = URL), show.legend = FALSE, color = "blue", 
              alpha = .5) + 
    geom_point(data = xmas, aes(ym, streams, group = URL), show.legend = FALSE, color = "blue", 
               alpha = .9) + 
    
    # And some chart styling
    scale_x_discrete(labels = yms_labs, limits = yms) + 
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45)) +
    facet_wrap(facets = ~ region, ncol = 1, scales = "free_y") +
    labs(x = "", y = "Streams per month")
```


![]({ site.baseurl }}/images/lastchristmas/output_16_0.png)


#### Plotted together
When plotted together the difference becomes obvious.


```R
ggplot() + 
    # First I create a vertical red dashed line for every december month
    geom_vline(xintercept = c("2017 12", "2018 12", "2019 12"), linetype = "dashed", color = "red") +
    
    # Then I plot the non-xmas data in green
    geom_line(data = noxmas, aes(ym, streams, group = URL), show.legend = FALSE, color = "darkgreen", 
              linetype = "dashed", alpha = .4) + 
    geom_point(data = noxmas, aes(ym, streams, group = URL), show.legend = FALSE, color = "darkgreen", 
               alpha = .4) + 
    
    # And the christmas data in blue
    geom_line(data = xmas, aes(ym, streams, group = URL), show.legend = FALSE, color = "blue", 
              alpha = .5) + 
    geom_point(data = xmas, aes(ym, streams, group = URL), show.legend = FALSE, color = "blue", 
               alpha = .9) + 
    
    # And some chart styling
    scale_x_discrete(labels = yms_labs, limits = yms) + 
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45)) +
    facet_wrap(facets = ~ region, ncol = 1, scales = "free_y") +
    labs(x = "", y = "Streams per month")
```


![]({ site.baseurl }}/images/lastchristmas/output_18_0.png)


### Interpretation

#### Non Christmas
We see a few non-christmas songs that make it more than a year - ie they appear in the same month i more than one year. But, they show a much different trend than the christmas songs. They have a pattern of popularity and then a fade out. 

#### Christmas
With the Christmas songs we see a whole different pattern. First we see more of them, ie there are more survivors year on year. Second the pattern is quite different; the songs lie dormant pretty much all year and then suddenly they explode in december. Some even get an early listen in november, where the must christmas enthusiastic people get started. 

## Conlusion
There is no such thing as a summer classic - but plenty of christmas classics!
