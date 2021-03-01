# import libraries
library(tidyverse)
library(Hmisc)
library(ggthemes)
library(stringr)

# read the cleansed data
# PLEASE NOTE, THAT MY PERSONAL CSV FILE IS NOT INCLUDED IN THE REPO; PLEASE
# USE YOUR OWN!
li_cons <- read_csv("./01-data/cleansed_Connections.csv")

li_cons %>% 
  sample_n(2)

# Overall count of connections
li_cons %>% 
  dim()

# Current date and earliest date
li_cons$connectedOn %>% 
  max()

li_cons$connectedOn %>% 
  min()

# basic statistics

li_cons %>% 
  summary()

li_cons %>% 
  describe()

# some useful constants
c_col <- c("#264653", "#2a9d8f", "#e9c46a", "#f4a261", "#e76f51")
theme_set(
  theme(
    text = element_text(size = 18,  family="Optima"),
    plot.title = element_text(
      size = 28,
      hjust = 0,
      margin = margin(t = 10, r = 0, b = 10, l = 0, unit = "pt"),
      color = c_col[1],
      face = "bold"
    ),
    axis.title = element_text(
      size = 18,
      margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"),
      color = c_col[1],
      face = "bold"
    )
  )
)

# clean some company spellings

li_cons %>% 
  group_by(Company) %>% 
  summarise(
    n = n()
  ) %>% 
  arrange(Company) %>% 
  pull(Company)

li_cons$comp_clean <- li_cons$Company

li_cons <- li_cons %>% 
  mutate(
    comp_clean = if_else(str_detect(Company, "^Allianz"), "Allianz", comp_clean),
    comp_clean = if_else(str_detect(Company, "^Capgemini"), "Capgemini", comp_clean),
    comp_clean = if_else(str_detect(Company, "^metafinanz"), "Allianz - metafinanz", comp_clean),
    comp_clean = if_else(str_detect(Company, "^Siemens"), "Siemens", comp_clean),
    comp_clean = if_else(str_detect(Company, "^BMW"), "BMW", comp_clean),
    comp_clean = if_else(str_detect(Company, "^Bayer"), "Bayer", comp_clean),
    comp_clean = if_else(str_detect(Company, "^Daimler"), "Daimler", comp_clean),
    comp_clean = if_else(str_detect(Company, "^Mercedes"), "Daimler", comp_clean),
    comp_clean = if_else(str_detect(Company, "Hochschule"), "University", comp_clean),
    comp_clean = if_else(str_detect(Company, "University"), "University", comp_clean),
    comp_clean = if_else(str_detect(Company, "University"), "University", comp_clean),
    comp_clean = if_else(str_detect(Company, "Universität"), "University", comp_clean),
    comp_clean = if_else(str_detect(Company, "^Lana"), "Lana Labs", comp_clean),
    comp_clean = if_else(str_detect(Company, "^OPITZ"), "Opitz Consulting", comp_clean),
    comp_clean = if_else(str_detect(Company, "^SAP"), "SAP", comp_clean),
    comp_clean = if_else(str_detect(Company, "^UBS"), "UBS", comp_clean),
    comp_clean = if_else(str_detect(Company, "^UiPath"), "UiPath", comp_clean)
  )

li_cons %>% 
  group_by(comp_clean) %>% 
  summarise(
    n = n()
  ) %>% 
  arrange(comp_clean) %>% 
  pull(comp_clean)

# clean some position spellings

li_cons %>% 
  group_by(Position) %>% 
  summarise(
    n = n()
  ) %>% 
  arrange(Position) %>% 
  pull(Position)

li_cons$pos_clean <- li_cons$Position

li_cons <- li_cons %>% 
  mutate(
    pos_clean = if_else(str_detect(Position, "^Abteilungsdirektor"), "Abteilungsleiter", pos_clean),
    pos_clean = if_else(str_detect(Position, "^Account"), "Account Manager", pos_clean),
    pos_clean = if_else(str_detect(Position, "^Agile"), "Agile", pos_clean),
    pos_clean = if_else(str_detect(Position, "CTO"), "CTO", pos_clean),
    pos_clean = if_else(str_detect(Position, "^Business Area Lead"), "Abteilungsleiter", pos_clean),
    pos_clean = if_else(str_detect(Position, "^CEO"), "CEO", pos_clean),
    pos_clean = if_else(str_detect(Position, "^Chief"), "CXO", pos_clean),
    pos_clean = if_else(str_detect(Position, "^Co-Founder"), "Founder", pos_clean),
    pos_clean = if_else(str_detect(Position, "^Founder"), "Founder", pos_clean),
    pos_clean = if_else(str_detect(Position, "^Consultant"), "Consultant", pos_clean),
    pos_clean = if_else(str_detect(Position, "^Director"), "Director", pos_clean),
    pos_clean = if_else(str_detect(Position, "Vice President"), "Vice President", pos_clean),
    pos_clean = if_else(str_detect(Position, "VP"), "Vice President", pos_clean),
    pos_clean = if_else(str_detect(Position, "Professor"), "Professor", pos_clean),
    pos_clean = if_else(str_detect(Position, "^Head of"), "Head of", pos_clean),
    pos_clean = if_else(str_detect(Position, "^Lead"), "Leader", pos_clean),
    pos_clean = if_else(str_detect(Position, "^Leiter"), "Leader", pos_clean),
    pos_clean = if_else(str_detect(Position, "^Manager"), "Manager", pos_clean),
    pos_clean = if_else(str_detect(Position, "^Principal"), "Principal", pos_clean),
    pos_clean = if_else(str_detect(Position, "^Product Manager"), "Product Manager", pos_clean),
    pos_clean = if_else(str_detect(Position, "^Project Manager"), "Project Manager", pos_clean),
    pos_clean = if_else(str_detect(Position, "^Projektleiter"), "Project Manager", pos_clean)
  )

li_cons %>% 
  group_by(pos_clean) %>% 
  summarise(
    n = n()
  ) %>% 
  arrange(pos_clean) %>% 
  pull(pos_clean)

# connectedOn: show a bar chart with frequencies by year
li_cons %>% 
  ggplot() +
  geom_bar(
    aes(as.factor(connectedOnYear)),
    fill = c_col[1], color = "Black"
  ) +
  labs(
    title = "Number of new connections per year",
    subtitle = "I startet using LinkedIn in 2012, and it was the most active year concerning new connections",
    x = "Years active on LinkedIn",
    y = "Number of new connections"
  ) 

ggsave('./03-graphs/01 - connections per year.png', width = 16, height = 9)

# connectedOn: show a bar chart with frequencies by year and quarter
li_cons %>% 
  ggplot() +
  geom_bar(
    aes(connectedOnQuarterStr),
    fill = c_col[1], color = "Black"
  ) +
  labs(
    title = "Number of new connections per year and quarter",
    subtitle = "In the first month I connected to almost 150 contacts",
    x = "Years and quarters active on LinkedIn",
    y = "Number of new connections"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12))

ggsave('./03-graphs/02 - connections per year-quarter.png', width = 16, height = 9)


# connectedOn: show a bar chart with frequencies by year and quarter
li_cons %>% 
  filter(connectedOnYearMonth != "2012-03") %>% 
  ggplot() +
  geom_bar(
    aes(connectedOnQuarterStr),
    fill = c_col[1], color = "Black"
  ) +
  labs(
    title = "Number of new connections per year and quarter (without March 2012)",
    subtitle = "Disregarding the initial month, there are connection peaks in Q2-2013, Q3-2019, and Q4-2020",
    x = "Years and quarters active on LinkedIn",
    y = "Number of new connections"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12))

ggsave('./03-graphs/03 - connections per year-quarter without march 12.png', width = 16, height = 9)

# connectedOn: show a line chart cumulative
li_cons %>% 
  #filter(connectedOnYearMonth != "2012-03") %>% 
  group_by(connectedOnQuarterStr) %>% 
  summarise(
    n=n()
  ) %>% 
  ggplot(
    aes(connectedOnQuarterStr, cumsum(n)),
    fill = c_col[1], color = "Black"
  ) +
  geom_line(group = "") +
  geom_point() +
  labs(
    title = "Number of new connections for every year and quarter",
    x = "year and quarter",
    y = "count"
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 750)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8))

sggsave('./03-graphs/04 - connections per year-quarter cumulative.png')

# companies: show frequencies of top-most companies
li_cons %>% 
  group_by(comp_clean) %>% 
  summarise(
    n = n()
  ) %>% 
  drop_na() %>% 
  filter(n > 3) %>% 
  arrange(-n)  %>% 
  ggplot() +
  geom_col(
    aes(n, comp_clean),
    fill = c_col[2], color = "Black"
  ) +
  labs(
    title = "Number of connections per Company (with at least 4 connections)",
    subtitle = "Naturally, I have the most connections working for my employers, customers, and business partners",
    y = "Company name (limited to companies with at least 4 connections)",
    x = "Number of connections"
  ) 

ggsave('./03-graphs/05 - connections per top companies.png', width = 16, height = 9)

# positions: show frequencies of top-most positions

# some reording based on frequency
levels4sorting <- li_cons %>% 
  group_by(pos_clean) %>% 
  summarise(
    n = n()
  ) %>% 
  arrange(n) %>% 
  drop_na() %>% 
  filter(n > 3) %>% 
  pull(pos_clean)

li_cons_topPositions <- li_cons %>% 
  filter(pos_clean %in% levels4sorting) 

li_cons_topPositions$pos_clean <- factor(li_cons_topPositions$pos_clean, levels = levels4sorting)  

li_cons_topPositions %>% 
  group_by(pos_clean) %>% 
  summarise(
    n = n()
  ) %>% 
  ggplot() +
  geom_col(
    aes(n, pos_clean),
    fill = c_col[3], color = "Black"
  ) +
  labs(
    title = "Number of connections per position (with at least 4 positions)",
    subtitle = "I just realized I am connected to 28 professors!",
    y = "Position name (limited to titles with at least 4 connections)",
    x = "Number of connections"
  ) 

ggsave('./03-graphs/06 - connections per top companies.png', width = 16, height = 9)

# show history of new connections and companies

# list of top companies, with at least 4 connections
li_top_companies <- li_cons %>% 
  group_by(comp_clean) %>% 
  summarise(
    n = n()
  ) %>% 
  arrange(n) %>% 
  drop_na() %>% 
  filter(n > 5) %>% 
  pull(comp_clean)

li_top_companies

li_cons <- li_cons %>% 
  mutate(
    is_top_company = ifelse(comp_clean %in% li_top_companies, TRUE, FALSE),
    top_company = ifelse(comp_clean %in% li_top_companies, comp_clean, "_other")
  )

li_cons %>% 
  #filter(is_top_company) %>% 
  ggplot() + 
  geom_jitter(
    aes(connectedOnYear, top_company, color = top_company),
    size = 3,
    height = 0.2
  ) +
  labs(
    title = "Number of connections per company (with at least 6 connections per company)",
    subtitle = "Company information reflect my connections' actual employer and not the employer at the time when we connected",
    y = "Company name (limited to companies with at least 6 connections)",
    x = "Years active on LinkedIn"
  ) +
  theme(legend.position = "none")

ggsave('./03-graphs/07 - top-companies by year.png', width = 16, height = 9)

# show history of new connections and positions

# list of top positions, with at least 4 connections
li_top_pos <- li_cons %>% 
  group_by(pos_clean) %>% 
  summarise(
    n = n()
  ) %>% 
  arrange(n) %>% 
  drop_na() %>% 
  filter(n > 9) %>% 
  pull(pos_clean)

li_top_pos

li_cons <- li_cons %>% 
  mutate(
    is_top_pos = ifelse(pos_clean %in% li_top_pos, TRUE, FALSE),
    top_pos = ifelse(pos_clean %in% li_top_pos, pos_clean, "_other")
  )

li_cons %>% 
  ggplot() + 
  geom_jitter(
    aes(connectedOnYear, top_pos, color = top_pos),
    height = 0.2
  ) +
  labs(
    title = "Number of connections per position (with at least 10 connections per position)",
    subtitle = "Position information reflect my connections' actual positions and not the positions at the time when we connected",
    y = "Position (limited to positions with at least 10 connections)",
    x = "Years active on LinkedIn"
  ) +
  theme(legend.position = "none")

ggsave('./03-graphs/08 - top-positions by year.png', width = 16, height = 9)

# show relationship of top positions and top companies

li_cons %>% 
  ggplot() +
  geom_jitter(
    aes(top_company, top_pos, color = connectedOnYear),
    height = 0.2, width = 0.2
  ) +
  labs(
    title = "Relationship between top positions and top companies",
    subtitle = "Many of my Capgemini collegues made it to VP, Director, or Head of something great. Congrats!",
    y = "Position (limited to positions with at least 10 connections)",
    x = "Company name (limited to companies with at least 6 connections)"
  ) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6, size = 12))

ggsave('./03-graphs/09 - top-positions by top companies.png', width = 16, height = 9)

# connectedOn: show a bar chart with frequencies by year and quarter
li_cons %>% 
  filter(connectedOnYearMonth != "2012-03") %>% 
  filter(top_company != "_other") %>% 
  ggplot() +
  geom_bar(
    aes(connectedOnQuarterStr),
    fill = c_col[1], color = "Black"
  ) +
  labs(
    title = "Number of new connections by year and quarter for top companies and without March 2012",
    x = "year and quarter",
    y = "count"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6)) + 
  facet_wrap(~ top_company)

ggsave('./03-graphs/10 - top-companies by quarters.png')


