library(dplyr)
library(tidyr)
library(readr)
library(sjmisc)
library(ggplot2)

#################################################################################################

#Change csv columns
df <- read_csv("g:/data.csv", col_names = TRUE)
head(df)
colnames(df)

names <- colnames(df) %>% 
  strsplit("\t") %>% 
  unlist()
names

df <- df %>% 
  separate(col = colnames(df), 
           sep = '\t', 
           into = names)
head(df)
tail(df)


#Frequency Count 
for (i in c(1:10, 11, 13)) {
  cat(colnames(df)[i], "frequency: \n")
  print(table(df[, i]))
}


#Change numbers to answers
df$Q1 <- rec(df$Q1, rec = "1=stronglydisagree;2=disagree;3=agree;4=stronglyagree;0=noanswer")
df$Q2 <- rec(df$Q2, rec = "1=stronglydisagree;2=disagree;3=agree;4=stronglyagree;0=noanswer")
df$Q3 <- rec(df$Q3, rec = "1=stronglydisagree;2=disagree;3=agree;4=stronglyagree;0=noanswer")
df$Q4 <- rec(df$Q4, rec = "1=stronglydisagree;2=disagree;3=agree;4=stronglyagree;0=noanswer")
df$Q5 <- rec(df$Q5, rec = "1=stronglydisagree;2=disagree;3=agree;4=stronglyagree;0=noanswer")
df$Q6 <- rec(df$Q6, rec = "1=stronglydisagree;2=disagree;3=agree;4=stronglyagree;0=noanswer")
df$Q7 <- rec(df$Q7, rec = "1=stronglydisagree;2=disagree;3=agree;4=stronglyagree;0=noanswer")
df$Q8 <- rec(df$Q8, rec = "1=stronglydisagree;2=disagree;3=agree;4=stronglyagree;0=noanswer")
df$Q9 <- rec(df$Q9, rec = "1=stronglydisagree;2=disagree;3=agree;4=stronglyagree;0=noanswer")
df$Q10 <- rec(df$Q10, rec = "1=stronglydisagree;2=disagree;3=agree;4=stronglyagree;0=noanswer")
df$gender <- rec(df$gender, rec = "1=male;2=female;3=other;0=noanswer")
df$source <- rec(df$source, rec = "1=frontpage;2=googlesearch;3=other")
head(df)
tail(df)
write_csv(df, "g:/dataNew.csv")


##########################################################################################

#Question 1
#Frequency Count and percentage on Q1
df$Q1 <- factor(df$Q1, levels = c("stronglydisagree",
                                  "disagree",
                                  "agree",
                                  "stronglyagree",
                                  "noanswer"))
freq <- table(df$Q1)
print(freq)


#By different gender
Q1gender <- df[df$gender %in% c("male", "female"), c("Q1", "gender")]
head(Q1gender)

Q1gender <- Q1gender %>% 
  group_by(gender) %>% 
  count(Q1) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup() %>% 
  mutate(prop_perc = paste0(round(prop * 100, 2), "%"))
Q1gender$Q1 <- factor(Q1gender$Q1, 
                      levels = c("stronglydisagree",
                                 "disagree",
                                 "agree",
                                 "stronglyagree",
                                 "noanswer"))
Q1gender$gender <- factor(Q1gender$gender, 
                          levels = c("male", "female"))
Q1gender


#Relation plot
ggplot(Q1gender) + 
  geom_bar(aes(x = forcats::fct_rev(Q1), y = n, fill = Q1), 
           stat = "identity") +
  geom_text(aes(x = Q1, y = n, label = n), size = 3.5) + 
  facet_wrap(~gender, ncol = 1) + 
  xlab("Answers of Q1") + 
  ylab("Number") + 
  coord_flip()


#Relation plot on percentage
ggplot(Q1gender) + 
  geom_bar(aes(x = forcats::fct_rev(Q1), y = prop, fill = Q1), 
           stat = "identity") +
  geom_text(aes(x = Q1, y = prop, label = prop_perc), size = 3.5) + 
  facet_wrap(~gender, ncol = 1) + 
  xlab("Answers of Q1") + 
  ylab("Proportion") + 
  coord_flip()


#Question 2
#Frequency Count and percentage on Q2
df$Q2 <- factor(df$Q2, levels = c("stronglydisagree",
                                  "disagree",
                                  "agree",
                                  "stronglyagree",
                                  "noanswer"))
freq <- table(df$Q2)
print(freq)


#By different gender
Q2gender <- df[df$gender %in% c("male", "female"), c("Q2", "gender")]
head(Q2gender)

Q2gender <- Q2gender %>% 
  group_by(gender) %>% 
  count(Q2) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup() %>% 
  mutate(prop_perc = paste0(round(prop * 100, 2), "%"))
Q2gender$Q2 <- factor(Q2gender$Q2, 
                      levels = c("stronglydisagree",
                                 "disagree",
                                 "agree",
                                 "stronglyagree",
                                 "noanswer"))
Q2gender$gender <- factor(Q2gender$gender, 
                          levels = c("male", "female"))
Q2gender


#Relation plot on percentage
ggplot(Q2gender) + 
  geom_bar(aes(x = forcats::fct_rev(Q2), y = prop, fill = Q2), 
           stat = "identity") +
  geom_text(aes(x = Q2, y = prop, label = prop_perc), size = 3.5) + 
  facet_wrap(~gender, ncol = 1) + 
  xlab("Answers of Q2") + 
  ylab("Proportion") + 
  coord_flip()


#Question 3
#Frequency Count and percentage on Q3
df$Q3 <- factor(df$Q3, levels = c("stronglydisagree",
                                  "disagree",
                                  "agree",
                                  "stronglyagree",
                                  "noanswer"))
freq <- table(df$Q3)
print(freq)


#By different gender
Q3gender <- df[df$gender %in% c("male", "female"), c("Q3", "gender")]
head(Q3gender)

Q3gender <- Q3gender %>% 
  group_by(gender) %>% 
  count(Q3) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup() %>% 
  mutate(prop_perc = paste0(round(prop * 100, 2), "%"))
Q3gender$Q3 <- factor(Q3gender$Q3, 
                      levels = c("stronglydisagree",
                                 "disagree",
                                 "agree",
                                 "stronglyagree",
                                 "noanswer"))
Q3gender$gender <- factor(Q3gender$gender, 
                          levels = c("male", "female"))
Q3gender


#Relation plot on percentage
ggplot(Q3gender) + 
  geom_bar(aes(x = forcats::fct_rev(Q3), y = prop, fill = Q3), 
           stat = "identity") +
  geom_text(aes(x = Q3, y = prop, label = prop_perc), size = 3.5) + 
  facet_wrap(~gender, ncol = 1) + 
  xlab("Answers of Q3") + 
  ylab("Proportion") + 
  coord_flip()


#Question 4
#Frequency Count and percentage on Q4
df$Q4 <- factor(df$Q4, levels = c("stronglydisagree",
                                  "disagree",
                                  "agree",
                                  "stronglyagree",
                                  "noanswer"))
freq <- table(df$Q4)
print(freq)


#By different gender
Q4gender <- df[df$gender %in% c("male", "female"), c("Q4", "gender")]
head(Q4gender)

Q4gender <- Q4gender %>% 
  group_by(gender) %>% 
  count(Q4) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup() %>% 
  mutate(prop_perc = paste0(round(prop * 100, 2), "%"))
Q4gender$Q4 <- factor(Q4gender$Q4, 
                      levels = c("stronglydisagree",
                                 "disagree",
                                 "agree",
                                 "stronglyagree",
                                 "noanswer"))
Q4gender$gender <- factor(Q4gender$gender, 
                          levels = c("male", "female"))
Q4gender


#Relation plot on percentage
ggplot(Q4gender) + 
  geom_bar(aes(x = forcats::fct_rev(Q4), y = prop, fill = Q4), 
           stat = "identity") +
  geom_text(aes(x = Q4, y = prop, label = prop_perc), size = 3.5) + 
  facet_wrap(~gender, ncol = 1) + 
  xlab("Answers of Q4") + 
  ylab("Proportion") + 
  coord_flip()


#Question 5
#Frequency Count and percentage on Q5
df$Q5 <- factor(df$Q5, levels = c("stronglydisagree",
                                  "disagree",
                                  "agree",
                                  "stronglyagree",
                                  "noanswer"))
freq <- table(df$Q5)
print(freq)


#By different gender
Q5gender <- df[df$gender %in% c("male", "female"), c("Q5", "gender")]
head(Q5gender)

Q5gender <- Q5gender %>% 
  group_by(gender) %>% 
  count(Q5) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup() %>% 
  mutate(prop_perc = paste0(round(prop * 100, 2), "%"))
Q5gender$Q5 <- factor(Q5gender$Q5, 
                      levels = c("stronglydisagree",
                                 "disagree",
                                 "agree",
                                 "stronglyagree",
                                 "noanswer"))
Q5gender$gender <- factor(Q5gender$gender, 
                          levels = c("male", "female"))
Q5gender


#Relation plot on percentage
ggplot(Q5gender) + 
  geom_bar(aes(x = forcats::fct_rev(Q5), y = prop, fill = Q5), 
           stat = "identity") +
  geom_text(aes(x = Q5, y = prop, label = prop_perc), size = 3.5) + 
  facet_wrap(~gender, ncol = 1) + 
  xlab("Answers of Q5") + 
  ylab("Proportion") + 
  coord_flip()


#Question 6
#Frequency Count and percentage on Q6
df$Q6 <- factor(df$Q6, levels = c("stronglydisagree",
                                  "disagree",
                                  "agree",
                                  "stronglyagree",
                                  "noanswer"))
freq <- table(df$Q6)
print(freq)


#By different gender
Q6gender <- df[df$gender %in% c("male", "female"), c("Q6", "gender")]
head(Q6gender)

Q6gender <- Q6gender %>% 
  group_by(gender) %>% 
  count(Q6) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup() %>% 
  mutate(prop_perc = paste0(round(prop * 100, 2), "%"))
Q6gender$Q6 <- factor(Q6gender$Q6, 
                      levels = c("stronglydisagree",
                                 "disagree",
                                 "agree",
                                 "stronglyagree",
                                 "noanswer"))
Q6gender$gender <- factor(Q6gender$gender, 
                          levels = c("male", "female"))
Q6gender


#Relation plot on percentage
ggplot(Q6gender) + 
  geom_bar(aes(x = forcats::fct_rev(Q6), y = prop, fill = Q6), 
           stat = "identity") +
  geom_text(aes(x = Q6, y = prop, label = prop_perc), size = 3.5) + 
  facet_wrap(~gender, ncol = 1) + 
  xlab("Answers of Q6") + 
  ylab("Proportion") + 
  coord_flip()


#Question 7
#Frequency Count and percentage on Q7
df$Q7 <- factor(df$Q7, levels = c("stronglydisagree",
                                  "disagree",
                                  "agree",
                                  "stronglyagree",
                                  "noanswer"))
freq <- table(df$Q7)
print(freq)


#By different gender
Q7gender <- df[df$gender %in% c("male", "female"), c("Q7", "gender")]
head(Q7gender)

Q7gender <- Q7gender %>% 
  group_by(gender) %>% 
  count(Q7) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup() %>% 
  mutate(prop_perc = paste0(round(prop * 100, 2), "%"))
Q7gender$Q7 <- factor(Q7gender$Q7, 
                      levels = c("stronglydisagree",
                                 "disagree",
                                 "agree",
                                 "stronglyagree",
                                 "noanswer"))
Q7gender$gender <- factor(Q7gender$gender, 
                          levels = c("male", "female"))
Q7gender


#Relation plot on percentage
ggplot(Q7gender) + 
  geom_bar(aes(x = forcats::fct_rev(Q7), y = prop, fill = Q7), 
           stat = "identity") +
  geom_text(aes(x = Q7, y = prop, label = prop_perc), size = 3.5) + 
  facet_wrap(~gender, ncol = 1) + 
  xlab("Answers of Q7") + 
  ylab("Proportion") + 
  coord_flip()


#Question 8
#Frequency Count and percentage on Q8
df$Q8 <- factor(df$Q8, levels = c("stronglydisagree",
                                  "disagree",
                                  "agree",
                                  "stronglyagree",
                                  "noanswer"))
freq <- table(df$Q8)
print(freq)


#By different gender
Q8gender <- df[df$gender %in% c("male", "female"), c("Q8", "gender")]
head(Q8gender)

Q8gender <- Q8gender %>% 
  group_by(gender) %>% 
  count(Q8) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup() %>% 
  mutate(prop_perc = paste0(round(prop * 100, 2), "%"))
Q8gender$Q8 <- factor(Q8gender$Q8, 
                      levels = c("stronglydisagree",
                                 "disagree",
                                 "agree",
                                 "stronglyagree",
                                 "noanswer"))
Q8gender$gender <- factor(Q8gender$gender, 
                          levels = c("male", "female"))
Q8gender


#Relation plot on percentage
ggplot(Q8gender) + 
  geom_bar(aes(x = forcats::fct_rev(Q8), y = prop, fill = Q8), 
           stat = "identity") +
  geom_text(aes(x = Q8, y = prop, label = prop_perc), size = 3.5) + 
  facet_wrap(~gender, ncol = 1) + 
  xlab("Answers of Q8") + 
  ylab("Proportion") + 
  coord_flip()


#Question 9
#Frequency Count and percentage on Q9
df$Q9 <- factor(df$Q9, levels = c("stronglydisagree",
                                  "disagree",
                                  "agree",
                                  "stronglyagree",
                                  "noanswer"))
freq <- table(df$Q9)
print(freq)


#By different gender
Q9gender <- df[df$gender %in% c("male", "female"), c("Q9", "gender")]
head(Q9gender)

Q9gender <- Q9gender %>% 
  group_by(gender) %>% 
  count(Q9) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup() %>% 
  mutate(prop_perc = paste0(round(prop * 100, 2), "%"))
Q9gender$Q9 <- factor(Q9gender$Q9, 
                      levels = c("stronglydisagree",
                                 "disagree",
                                 "agree",
                                 "stronglyagree",
                                 "noanswer"))
Q9gender$gender <- factor(Q9gender$gender, 
                          levels = c("male", "female"))
Q9gender


#Relation plot on percentage
ggplot(Q9gender) + 
  geom_bar(aes(x = forcats::fct_rev(Q9), y = prop, fill = Q9), 
           stat = "identity") +
  geom_text(aes(x = Q9, y = prop, label = prop_perc), size = 3.5) + 
  facet_wrap(~gender, ncol = 1) + 
  xlab("Answers of Q9") + 
  ylab("Proportion") + 
  coord_flip()


#Question 10
#Frequency Count and percentage on Q10
df$Q10 <- factor(df$Q10, levels = c("stronglydisagree",
                                  "disagree",
                                  "agree",
                                  "stronglyagree",
                                  "noanswer"))
freq <- table(df$Q10)
print(freq)


#By different gender
Q10gender <- df[df$gender %in% c("male", "female"), c("Q10", "gender")]
head(Q10gender)

Q10gender <- Q10gender %>% 
  group_by(gender) %>% 
  count(Q10) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup() %>% 
  mutate(prop_perc = paste0(round(prop * 100, 2), "%"))
Q10gender$Q10 <- factor(Q10gender$Q10, 
                      levels = c("stronglydisagree",
                                 "disagree",
                                 "agree",
                                 "stronglyagree",
                                 "noanswer"))
Q10gender$gender <- factor(Q10gender$gender, 
                          levels = c("male", "female"))
Q10gender


#Relation plot on percentage
ggplot(Q10gender) + 
  geom_bar(aes(x = forcats::fct_rev(Q10), y = prop, fill = Q10), 
           stat = "identity") +
  geom_text(aes(x = Q10, y = prop, label = prop_perc), size = 3.5) + 
  facet_wrap(~gender, ncol = 1) + 
  xlab("Answers of Q10") + 
  ylab("Proportion") + 
  coord_flip()



