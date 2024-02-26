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
  xlim(c(0, 110))
g
dev.off()

