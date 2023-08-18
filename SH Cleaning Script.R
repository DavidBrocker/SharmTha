# Load in packages
library(janitor)
library(forcats)
library(readxl)
library(openxlsx)
library(stringr)
library(dplyr,warn.conflicts = F)
library(ggplot2)
library(tidytext)

# Read in File
sh <- read_excel(file.choose())

# Clean
sh_cln <- 
  sh |>
  clean_names() |> 
  select(subreddit,title,year_3,description_4) |> 
  rename(
    year = year_3,
    description = description_4
  ) 

# Theme by Subreddit Across Years
sh_cln |> 
  group_by(subreddit,year,description) |> 
  count() |> 
  ungroup() |>
  filter(description != "NA") |> 
  ggplot(aes(year,n, group = subreddit, color = subreddit)) +
  geom_point() + 
  geom_line() +
  labs(
    x = "\nYear",
    y = "Frequency of Posts\n",
    color = "Subreddit\n"
  ) +
  theme_minimal() +
  facet_wrap(~description)

# Sentiment Analysis
sh_cln |> 
  unnest_tokens(word,title) |> 
  anti_join(stop_words) |> 
  group_by(word,subreddit) |> 
  count(sort = T) |> 
  ungroup() |> 
  group_by(subreddit) |> 
  top_n(n = 20, wt = word) |> 
  ggplot(aes(fct_reorder(word,n),n,fill = subreddit)) + 
  geom_bar(stat="identity") + 
  coord_flip() + 
  facet_wrap(~subreddit) +
  theme_minimal()

sh_cln |> 
  filter(str_detect(title, "deep")) |> 
  group_by(subreddit) |> 
  count() |> 
  ggplot(aes(subreddit,n,fill = subreddit)) +
  geom_bar(stat = "identity") +
  coord_flip() + 
  theme_minimal() +
  labs(
    x = "Subreddit\n",
    y = "Frequency",
    title = "Posts Containing 'Clean'"
  ) + 
  theme(
    plot.title.position = "plot"
  )


library(rvest)

url <- "https://www.recoveryanswers.org/addiction-ary/"

addictionary <- 
  url |> 
  read_html() |> 
  html_nodes("h5") |> 
  html_text(trim = TRUE) |> 
  str_to_lower()



test_sent <- 
  data.frame(
  addictionary,
  lexicon = "addiction"
) |> 
  unnest_tokens(word,addictionary)


sh_cln |> 
  unnest_tokens(word,title) |> View()
  anti_join(stop_words) |> 
  inner_join(test_sent,multiple = "all") |> 
  group_by(subreddit,lexicon) |> 
  count(sort = T) |> 
  ungroup() |> 
  ggplot(aes(subreddit,n, fill = subreddit)) + 
  geom_bar(stat = "identity") + 
  theme_minimal() + 
  coord_flip() + 
  labs(
    x = "\nSubreddit",
    y = "\nFrequency\n"
  ) +
  scale_fill_manual(values = wesanderson::wes_palette(9))
