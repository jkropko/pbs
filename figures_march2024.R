library(tidyverse)
source("DataCleaning.R")

# Table 1: Institutions x Value for Public Dollars 

value <- data %>%
  select(response_id, starts_with("valuefordollar")) %>%
  pivot_longer(starts_with("valuefordollar"),
               names_to = "institution",
               values_to = "response") %>%
  group_by(institution, response) %>%
  summarize(count=n()) %>%
  filter(!is.na(response)) %>%
  group_by(institution) %>%
  mutate(total = sum(count),
         percent = round(100*count/total, 1),
         disp = paste(percent, "%", sep=""),
         institution = fct_recode(institution, 
                                  "Congress"="valuefordollar_congress",
                                  "Courts of Law"="valuefordollar_courtoflaw",
                                  "Current President"="valuefordollar_currentpresident",
                                  "Law Enforcement"="valuefordollar_lawenforcementpolice",
                                  "Local Government"="valuefordollar_localgovernment",
                                  "Military"="valuefordollar_military",          
                                  "NASA"="valuefordollar_nasa",
                                  "NPR"="valuefordollar_npr",
                                  "PBS"="valuefordollar_pbs",
                                  "Postal Service"="valuefordollar_postalservice",       
                                  "Public Schools"="valuefordollar_publicschools",
                                  "Smithsonian"="valuefordollar_smithsonian",         
                                  "State Government"="valuefordollar_stategovernment"),
         institution = fct_relevel(institution, "PBS")) 

pdf("figures/figure1.pdf", width=10, height=8)
g <- ggplot(value, aes(x=count, y=response, fill=institution)) +
  geom_col() +
  facet_wrap(~institution) +
  guides(fill=FALSE) +
  theme(text=element_text(family="serif")) +
  geom_text(aes(label = disp), hjust=-.1, size=3) +
  xlab("Count") +
  ylab("Response") +
  xlim(c(0, 900))
g
dev.off()

### Missing values (negligable?)
value <- data %>%
  select(response_id, starts_with("valuefordollar")) %>%
  pivot_longer(starts_with("valuefordollar"),
               names_to = "institution",
               values_to = "response") %>%
  group_by(institution, response) %>%
  summarize(count=n()) %>%
  filter(is.na(response))

#Table 2: Aspects of PBS contributing to audiences’ trust (N=1533)
contribute <- data %>%
  select(response_id, starts_with("contributetrustpbs")) %>%
  select(-contains("other")) %>%
  pivot_longer(starts_with("contributetrustpbs"),
               names_to = "reason",
               values_to = "response") %>%
  group_by(reason, response) %>%
  summarize(count=n()) %>%
  filter(!is.na(response)) %>%
  group_by(reason) %>%
  mutate(total = sum(count),
         percent = round(100*count/total, 1),
         disp = paste(percent, "%", sep=""),
         reason = fct_recode(reason, 
                                  "Adult Programs"="contributetrustpbs_adultprograms",
                                  "Arts and Culture"="contributetrustpbs_artsandculture",
                                  "Children's Programs"="contributetrustpbs_childrenprograms",
                                  "Entertainment"="contributetrustpbs_entertainment",
                                  "Funding Model"="contributetrustpbs_funding",
                                  "Kids' Games"="contributetrustpbs_kidsgames",          
                                  "Local News"="contributetrustpbs_localnews",
                                  "Local Station"="contributetrustpbs_localstation",
                                  "National News"="contributetrustpbs_nationalnews",
                                  "PBS as an Organization"="contributetrustpbs_organisation"))
contrib_order <- contribute %>%
  filter(response=="Trust a lot") %>%
  arrange(-percent) 
contrib_order <- as.character(contrib_order$reason)

contribute <- contribute %>%
  mutate(reason = fct_relevel(reason, contrib_order))

pdf("figures/figure2.pdf", width=10, height=8)
g <- ggplot(contribute, aes(x=count, y=response, fill=reason)) +
  geom_col() +
  facet_wrap(~reason) +
  guides(fill=FALSE) +
  theme(text=element_text(family="serif")) +
  geom_text(aes(label = disp), hjust=-.1, size=3) +
  xlab("Count") +
  ylab("Response") +
  xlim(c(0, 1000))
g
dev.off()

### Missing values (negligable except for 43 missing on kids games)
contribute <- data %>%
  select(response_id, starts_with("contributetrustpbs")) %>%
  select(-contains("other")) %>%
  pivot_longer(starts_with("contributetrustpbs"),
               names_to = "reason",
               values_to = "response") %>%
  group_by(reason, response) %>%
  summarize(count=n()) %>%
  filter(is.na(response))

# Table 3: PBS as minor or major soource of news by respondents’ political leaning (N=1533, missing values=4) 
politics <- data %>%
  group_by(subscription_pbs, politics) %>%
  summarize(count=n()) %>%
  group_by(politics) %>%
  mutate(total = sum(count),
         percent = count/total,
         disp = paste(round(100*percent, 1), "%", sep=""),
         pol_disp = paste(politics, "\n (N = ", total, ")", sep="")) %>%
  filter(subscription_pbs=="Minor source") %>%
  mutate(pol_disp = fct_relevel(as.factor(pol_disp), 
                                "Extremely liberal\n (N = 183)",
                                "Moderately liberal\n (N = 217)",
                                "Slightly liberal\n (N = 131)",
                                "Neither liberal nor conservative\n (N = 325)",
                                "Slightly conservative\n (N = 156)",
                                "Moderately conservative\n (N = 225)",
                                "Extremely conservative\n (N = 296)"))

pdf("figures/figure3.pdf", width=10, height=5)
g <- ggplot(politics, aes(x=pol_disp, y=100*percent)) +
  geom_col(fill="blue") +
  theme(text=element_text(family="serif")) +
  geom_text(aes(label = disp), vjust=-.5, size=3) +
  geom_hline(yintercept=50, linetype='dashed', col = 'black') +
  xlab("Political Ideology") +
  ylab("Percent Citing PBS as a Major Source of Political and Election News") +
  scale_y_continuous(breaks=c(0, 10, 20, 30, 40, 50, 60)) +
  guides(fill=FALSE)
g
dev.off()

### Missing values (none??)
politics <- data %>%
  group_by(subscription_pbs, politics) %>%
  summarize(count=n())

# Figure 1: Perception of bias in PBS news
tab <- summarytools::freq(data$levelofbias_pbsnews, order = "level", 
                   report.nas=FALSE,
                   cumul=FALSE,
                   totals=FALSE)
tab
print(tab, method="render")

# Chart 4: Children’s programming vs political leaning (N=1533, missing values=206)
kids <- data %>%
  group_by(contributetrustpbs_childrenprograms, politics) %>%
  summarize(count=n()) %>%
  group_by(politics) %>%
  filter(!is.na(contributetrustpbs_childrenprograms)) %>%
  mutate(total = sum(count),
         percent = count/total,
         disp = paste(round(100*percent, 1), "%", sep="")) 

pdf("figures/figure4.pdf", width=10, height=8)
g <- ggplot(kids, aes(x=count, y=contributetrustpbs_childrenprograms, fill=politics)) +
  geom_col() +
  facet_wrap(~politics) +
  guides(fill=FALSE) +
  theme(text=element_text(family="serif")) +
  geom_text(aes(label = disp), hjust=-.1, size=3) +
  xlab("Count") +
  ylab("Response") +
  xlim(c(0, 150))
g
dev.off()

#Would it be possible to get a frequency table/chart/figure of levels of levels of trust in PBS kids (Q32_6)?
kidstv <- data %>%
  group_by(contributetrustpbs_childrenprograms) %>%
  summarize(count = n()) %>%
  na.omit()

pdf("figures/figure5.pdf", width=8, height=6)
g <- ggplot(kidstv, aes(x=contributetrustpbs_childrenprograms, y=count)) +
  geom_col(fill="blue") +
  theme(text=element_text(family="serif")) +
  geom_text(aes(label = count), hjust=-.5, size=3) +
  xlab("Level of trust in children’s educational programming on PBS") +
  ylab("Count") +
  guides(fill=FALSE) +
  coord_flip() +
  ylim(c(0,650))
g
dev.off()

#Would it be possible to get a frequency table/chart/figure on how much people watch PBS kids (Q29_3)
kidstv <- data %>%
  group_by(consumepbs_chidlrenprograms) %>%
  summarize(count = n()) %>%
  na.omit() %>%
  mutate(consumepbs_chidlrenprograms = fct_relevel(consumepbs_chidlrenprograms,
                                                   "None at all",
                                                   "A little",
                                                   "A moderate amount",
                                                   "A lot",
                                                   "A great deal"))

pdf("figures/figure6.pdf", width=8, height=6)
g <- ggplot(kidstv, aes(x=consumepbs_chidlrenprograms, y=count)) +
  geom_col(fill="blue") +
  theme(text=element_text(family="serif")) +
  geom_text(aes(label = count), hjust=-.5, size=3) +
  xlab("How much children's programming on PBS do you consume?") +
  ylab("Count") +
  guides(fill=FALSE) +
  coord_flip() +
  ylim(c(0,500))
g
dev.off()

#Would it be possible to get a frequency table/chart/figure on our recoded open ended questions (Q_38)
open <- data %>%
  group_by(open_whyyoutrust) %>%
  summarize(count = n()) %>%
  na.omit() %>%
  filter(open_whyyoutrust != "NA") %>%
  mutate(open_whyyoutrust = fct_relevel(open_whyyoutrust,
                                        "A specific show or program",
                                        "Structure of the network",
                                        "Range and diversity of programming",
                                        "Children's and educational programming",
                                        "Nostalgia, familiarity, and comfort",
                                        "High quality programming",
                                        "Public funding",
                                        "Other", 
                                        "News exhibits political moderation, unbiasedness, or accuracy" ))

pdf("figures/figure7.pdf", width=8, height=6)
g <- ggplot(open, aes(x=open_whyyoutrust, y=count)) +
  geom_col(fill="blue") +
  theme(text=element_text(family="serif")) +
  geom_text(aes(label = count), hjust=-.5, size=3) +
  xlab("Why do you trust PBS? (Open-ended, manually categorized)") +
  ylab("Count") +
  guides(fill=FALSE) +
  coord_flip() +
  ylim(c(0,500))
g
dev.off()

#Percentage of people who considered PBS a major source of news (Q.18.8)

data %>%
  group_by(subscription_pbs) %>%
  summarize(count = n())

#Percentage of people who said they consumed PBS children’s educational programming (a little, a moderate amount, a lot, a great deal) (Q. 29.3)

data %>%
  group_by(consumepbs_chidlrenprograms) %>%
  summarize(count = n())

#Percentage of people who said they consumed PBS Kids Games (a little, a moderate amount, a lot, a great deal) (Q 29.4)

data %>%
  group_by(consumepbs_kidsgames) %>%
  summarize(count = n())
