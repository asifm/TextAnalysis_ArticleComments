library(tidyverse)
library(readxl)
wsj <- read_xlsx("Data/CommentsWSJ.xlsx", sheet = "cleaned")
nyt <- read_xlsx("Data/CommentsNYT.xlsx", sheet = "cleaned")

wsj |>
  mutate(post_id = paste0("wsj", row_number())) |>
  select(post_id, post_text) |>
  write_csv("Data/wsj_posts.csv")

nyt |>
  mutate(post_id = paste0("nyt", row_number())) |>
  select(post_id, post_text) |>
  write_csv("Data/nyt_posts.csv")

wsj_posts <- read_csv("Data/wsj_posts.csv")
nyt_posts <- read_csv("Data/nyt_posts.csv")

posts <- bind_rows(wsj_posts, nyt_posts)

posts |>
  write_csv("Data/posts.csv")
