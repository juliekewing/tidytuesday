# Setup

library(tidyverse)
library(tidytext)

# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2020-09-29')
##tuesdata <- tidytuesdayR::tt_load(2020, week = 40)

beyonce_lyrics <- tuesdata$beyonce_lyrics
tswift_lyrics <- tuesdata$taylor_swift_lyrics
sales <- tuesdata$sales
charts <- tuesdata$charts


#Data wrangling

#Order albums by release
x <- c("Taylor Swift ", "Fearless", "Speak Now ", "Red", "1989",
       "reputation ", "Lover ", "folklore ")


###take the full string of lyrics, separate it into one row per word, remove stop words
tidy_lyrics <- tswift_lyrics %>%
  unnest_tokens(word, Lyrics) %>% 
  anti_join(stop_words, by=c("word"="word"))


##let's see what words changed in usage over time

##
ts_tf_idf <- tidy_lyrics %>%
  count(Album, word, sort = TRUE) %>%
  bind_tf_idf(word, Album, n) %>% 
  group_by(word) %>%
  mutate(albums_used = n(), total_freq = sum(n)) %>%
  ungroup() %>%
  filter(albums_used == 8)


distinct_per_album <- tidy_lyrics %>%
  count(Album, word, sort = TRUE) %>%
  bind_tf_idf(word, Album, n) %>% 
  group_by(word) %>%
  mutate(albums_used = n(), total_freq = sum(n)) %>%
  ungroup() %>%
  group_by(Album) %>%
  mutate(most_distinct_value = max(tf_idf)) %>%
  filter(tf_idf == most_distinct_value)



lyrics_album <- ts_tf_idf %>%
  mutate(Album = factor(Album, levels = x)) %>% 
  mutate(album_order = as.integer(Album))




#Calculate term frequency-inverse document frequency 
#(h/t to Rosie Baillie for her awesome text analysis!)
ts_tf_idf <- tidy_lyrics %>%
  count(Album, word, sort = TRUE) %>%
  bind_tf_idf(word, Album, n) %>%
  arrange(-tf_idf) %>%
  group_by(Album) %>%
  top_n(10) %>% 
  ungroup %>% 
  mutate(counter = 1) %>% 
  group_by(Album) %>% 
  mutate(ticker = cumsum(counter)) %>% 
  arrange(Album, n) %>% 
  filter(ticker <= 10) %>%
  ungroup


#Order albums by release
x <- c("Taylor Swift ", "Fearless", "Speak Now ", "Red", "1989",
       "reputation ", "Lover ", "folklore ")

ts_tf_idf <- ts_tf_idf %>% 
  mutate(Album = factor(Album, levels = x)) %>% 
  mutate(album_order = as.integer(Ablum))
      
      , "Fearless", "Speak Now ", "Red", "1989",
       "reputation ", "Lover ", "folklore "
      
      
      " ~ 1 + text_adj,
  ))

#Set theme
font_family <- 'Century Gothic'
background <- "#0D1821"
text_colour <- "white"
axis_colour <- "#595959"
plot_colour <- "white"
theme_style <- theme(text = element_text(family = font_family ),
                     rect = element_rect(fill = background),
                     plot.background = element_rect(fill = background, color = NA),
                     plot.title = element_markdown(family = font_family, vjust = 3.5, hjust = 0.5, size = 18, colour = text_colour),
                     plot.subtitle = element_text(family = font_family, vjust = 3, hjust = 0.5, size = 14, colour = text_colour),
                     plot.caption = element_markdown(family = font_family, hjust = 0.5, size = 10, colour = text_colour),
                     plot.margin = unit(c(1.5, 1, 1.5, 1), "cm"),
                     panel.background = element_rect(fill = background, color = NA),
                     panel.border = element_blank(),
                     panel.grid.major.y = element_line(colour = axis_colour, size = .1),
                     panel.grid.major.x = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     axis.title.x = element_blank(),
                     axis.text.x = element_text(family = font_family, size = 6, colour= text_colour),
                     axis.title.y = element_blank(),
                     axis.text.y = element_text(family = font_family, size = 6, colour= text_colour),
                     axis.ticks = element_blank(),
                     axis.line.y = element_line(colour = axis_colour, size = .05),
                     axis.line.x = element_blank(),
                     legend.position="none",
                     strip.text.x = element_text(size = 12, face = 'bold', colour= text_colour),
                     strip.placement = "outside",
                     strip.background = element_blank())

theme_set(theme_classic() + theme_style)

#Create colour palette
cols = c("#EAB4EE", "#FDE2AF", "#FCFDD3", "#BEEADE", "#CAE4FC", "#CBC4F3", "#F3B8D0", "#F9CCC7")

#Create variables needed for notes
stem_placement <- 0.06
note_width <- 0.2
note_height <- 0.02
text_height <- 0.03

#Plot data
ggplot(ts_tf_idf) +
  geom_segment(aes(x = ticker + stem_placement, 
                   xend = ticker + stem_placement, 
                   y = tf_idf, 
                   yend = tf_idf + note_height), 
               size = .2, 
               color=plot_colour) +
  geom_curve(aes(x = ticker + stem_placement, 
                 xend =  ticker + note_width,
                 y = tf_idf + note_height,
                 yend = tf_idf + note_height),
             size = .2, 
             color = plot_colour, 
             curvature = 0.4) +
  geom_hline(aes(yintercept = 0), 
             colour = axis_colour, 
             size = .05) +
  geom_point(aes(ticker, 
                 y=tf_idf, 
                 colour = Album), 
             size=2.5) +
  geom_text(data = ts_tf_idf,
            aes(y = tf_idf + text_height,
                x = ticker,
                label = paste(ticker, ".", word)),
            family = font_family,
            size = 2,
            hjust = "left",
            color = text_colour) +
  scale_y_continuous(limits = c(0, 0.08, 0.015),
                     sec.axis = dup_axis(name = NULL, labels = NULL)) +
  scale_x_discrete(labels = word) +
  scale_colour_manual(name = "Album", values = cols) +
  facet_wrap(~Album, ncol=1, strip.position = "top") +
  labs(title = "**Taylor Swift** | Top Ten Words Per Album",
       subtitle = "Albums ordered by release",
       caption = "<br/><br/>**Data Source:** Rosie Baillie & Dr. Sara Stoudt<br/>**Visualisation:** @JaredBraggins")
