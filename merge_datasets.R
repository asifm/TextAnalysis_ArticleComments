library(tidyverse)
library(readxl)

nyt <- read_xlsx("Data/CommentsNYT.xlsx", sheet = "cleaned")
wsj <- read_xlsx("Data/CommentsWSJ.xlsx", sheet = "cleaned")

nyt_posts <- nyt |>
  rename(
    post_text = commentBody, post_type = commentType,
    article_url = address, article_title = title,
    upvotes = recommendations, reply_count = replyCount,
  ) |>
  mutate(
    post_id = paste0("nyt", row_number()),
    post_type = ifelse(post_type == "userReply", "reply", post_type),
    post_date = as_date(createDate),
    publication = "NYT"
  ) |>
  select(
    post_text, post_id, post_type, post_length, post_date,
    upvotes, reply_count, article_title, article_url
  )

wsj_posts <- wsj |>
  rename(
    post_text = content, post_type = type,
    article_url = address, article_title = title,
    upvotes = rank.ranks_up, reply_count = replies_count
  ) |>
  mutate(
    post_id = paste0("wsj", row_number()),
    post_date = as_date(written_at),
    publication = "WSJ"
  ) |>
  select(
    post_text, post_id, post_type, post_length, post_date,
    upvotes, reply_count, article_title, article_url
  )

posts <- bind_rows(wsj_posts, nyt_posts)

write_csv(posts, "Data/posts.csv")
