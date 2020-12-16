library(tidyverse)

df <- read_delim("data/bmat_results_tl.txt", delim = " ", col_names = c("Domicile", "Course", "Year", "Candidate", letters[1:8]), skip = 1) %>%
  unite("x", a:h, sep = " ") %>%
  mutate(x = str_remove_all(x, " NA")) %>%
  mutate(b = str_extract(x, "[0-9](.[0-9]|) [0-9](.[0-9]|)")) %>%
  separate(b, into = c("BMAT_Section_1", "BMAT_Section_2"), sep = " ", convert = TRUE) %>%
  mutate(Offer = ifelse(str_detect(x, "Y "), TRUE, FALSE)) %>%
  mutate(x = (str_remove(x, " ?(Y|) [0-9](.[0-9]|) [0-9](.[0-9]|)"))) %>%
  rename("College" = x) %>%
  mutate(BMAT_all = BMAT_Section_1 + BMAT_Section_2) %>%
  #group_by(College, Year) %>%
  #mutate(offer_fraction = sum(Offer)/n()) %>%
  #ungroup %>%
  select(-Domicile, -Course) %>%
  filter(College != "Hughes Hall") 

df %>%
  write_tsv("data/bmat_df.txt")

df <- df %>%
  mutate(dataset = ifelse(Year %in% c("2017", "2018"), "test", "train")) %>%
  mutate(Score = (BMAT_all - mean(BMAT_all)) / sd(BMAT_all)) %>%
  mutate(College_ID = as.numeric(as.factor(College))) %>%
  ungroup() %>%
  select(-Candidate, -College, -dataset)

filter(df, Year %in% c(2017, 2018)) %>%
  write_tsv("data/train.txt")

filter(df, Year == 2019) %>%
  write_tsv("data/test.txt")
