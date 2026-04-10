# AC Survey (R Version)

library(tidyverse)

# a few formats:  with numeric responses vs choice text
#                 also split multianswer fields into columns or not
# I think I'm going to try to wrangle unsplit choice first
choice = "/Users/ishmael/Desktop/Projects/AC Survey/AC_RProject/Qualtrics_choice.csv"
nums = "/Users/ishmael/Desktop/Projects/AC Survey/AC_RProject/Qualtrics_numeric.csv"
split_choice = "/Users/ishmael/Desktop/Projects/AC Survey/AC_RProject/Qualtrics_split_choice.csv"
split_nums = "/Users/ishmael/Desktop/Projects/AC Survey/AC_RProject/Qualtrics_split_numeric.csv"

df = read_csv(file = choice)
colnames(df)
df <- df %>% 
  select("ResponseId", "Status", "Progress", "Duration (in seconds)",
         "LocationLatitude", "LocationLongitude", "Intro", contains("_"))
df <- slice(df, -c(1,2,3)) # drop my preview submission and garbage header-likes
colnames(df)
unique(df$Status)
# df[df$Status == "Spam",]
df <- df %>% 
  filter(Status != "Spam")

#TODO Drop Status == "Spam" response
#TODO Rename Duration column
# Examine durations of survey and graph vs if they finished or not ("Progress" or "Finished")
# Compare formats to "choice"
# POTENTIAL FUTURE ADDON: Determine country of origin by latitude/longitude data?
#TODO Make dictionary with response number and entry key
gamenames = c("Assassin's Creed (2007)",
              "Assassin's Creed II (2009)", 
              "Assassin's Creed: Brotherhood (2010)",
              "Assassin's Creed: Revelations (2011)",
              "Assassin's Creed III (2012)",
              "Assassin's Creed IV: Black Flag (2013)",
              "Assassin's Creed: Rogue (2014)",
              "Assassin's Creed: Unity (2014)",
              "Assassin's Creed: Syndicate (2015)",
              "Assassin's Creed: Origins (2017)",
              "Assassin's Creed: Odyssey (2018)",
              "Assassin's Creed: Valhalla (2020)")

# So 12 games and the second part of each question was:
#   _1 = Story
#   _2 = Gameplay
#   _3 = Enjoyment/Overall

# Exact text of second question
# "Reviews of the games generally seem to boil down to feedback on the Story and Gameplay,
# which are often different. You'll also of course be asked for an overall rating for the game, 
# how much you personally enjoyed playing it overall (not necessarily how much you'd recommend
# it to others!) Please rate on a classic 1-10 scale.







#LETS COMPARE THE MULTICOLUMN SPLIT OUTPUT
df2 = read_csv(file = split_nums)
df2
colnames(df2)
df2 <- df2 %>% 
  select("ResponseId", "Status", "Progress", "Duration (in seconds)",
         "LocationLatitude", "LocationLongitude", contains("_"))
df2 <- slice(df2, -c(1,2,3)) 
# seems to split intro into intro_1 _2 etc with 0 for no and 1 for yes (played it)
colnames(df2)
df2 <- rename(df2, "Duration" = "Duration (in seconds)")


#regular nums probably best for the questions i want to answer
df_n <- read_csv(file = nums)
titles <- df_n[1,] # saved list of the QUESTION TEXT and also full text for 1_3 etc.
df_n <- df_n %>% 
  select("ResponseId", "Status", "Progress", "Duration (in seconds)",
         "LocationLatitude", "LocationLongitude", "Intro", contains("_")) %>% 
  slice(-c(1,2,3))
colnames(df_n)
played <- df_n["Intro"]


# let's think about what we want to answer. first, a chart of how many people played each game


# let's try to get proportions of people who played each game from played
# I think I can get this from the NUMS version, but the split output above maybe possible
str_detect(played, "1")
str_count(played[[1]], "1")
typeof(played[1,1])
played[[1]]
for (i in 1:12){
  
}
for (i in as.character(1:12)){
  played_counts <- mutate(played, )
}
#IDK WHAT I AM DOING AT ALL

# let's try to expand the data into a full matrix first (ABANDONED)
strsplit(played[[1,1]], ",")
head(played)
played[[1,1]]

# nope, let's take original and convert to vector then matrix
played <- unlist(played)
played <- str_split_fixed(played, ",", 12)

play_mat <- data.frame(matrix(nrow = 12, ncol = 2))
play_mat[,1] <- 1:12
for (i in 1:12){
  for (j in 1:dim(played)[1]){
    #if ()
  }
}
dim(played)[1]


typeof(played)
















# ugh. chatgpt time
# Sample data
responses <- c("1", "1,2,5,6", "4,5,12", "2,3,7,8,11")

# Step 2: Split each response into a vector of integers
response_list <- strsplit(responses, ",")

# Step 3: Create a matrix to store counts
num_questions <- 12
response_matrix <- matrix(0, nrow = length(played), ncol = 12)


# Step 4: Iterate through each response and update counts
for (i in seq_along(response_list)) {
  answered_questions <- as.integer(response_list[[i]])
  response_matrix[i, answered_questions] <- 1
}

# Step 5: Calculate proportions for each question
proportions <- colMeans(response_matrix)

# Display proportions
result <- data.frame(Question = 1:num_questions, Proportion = proportions)
print(result)









