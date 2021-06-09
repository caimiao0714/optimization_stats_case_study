pacman::p_load(dplyr, ggplot2, data.table, tidyr, ggthemes, viridis, showtext)

d = data.table::fread("data/lk_risks.csv") %>% 
  mutate(path = paste(`Node A`, `Node Z`, sep = "-") %>% 
           gsub("node ", "", .)) %>% 
  select(path, starts_with("risk_")) %>% 
  mutate_each(function(x)dense_rank(x), -path) %>% 
  arrange(desc(risk_logit)) %>% 
  mutate(path = factor(path, levels = .$path)) %>% 
  pivot_longer(cols = risk_logit:risk_DL, 
               names_to = "stats model", 
               values_to = "risk rank") %>% 
  mutate(`stats model` = gsub("risk_", "", `stats model`)) %>% 
  mutate(`stats model` = case_when(`stats model` == "logit" ~ "Logistic",
                                   `stats model` == "poisson" ~ "Poisson",
                                   `stats model` == "xgboost" ~ "XGBoost",
                                   `stats model` == "DL" ~ "Deep Learning") %>% 
           factor(levels = c("Logistic", "Poisson", "XGBoost", "Deep Learning")))

#title = "Concordance of four models for evaluating the risk of crash",
#caption = "Darker color indicates higher rank of crash risk")
# white border outside of boxes
p = d %>% 
  ggplot(aes(x = path, y = `stats model`, fill = `risk rank`)) + 
  scale_fill_viridis(option = "magma", 
                     direction = -1,
                     breaks = seq(1, 21, 4), 
                     guide = guide_legend(reverse = F)) + 
  geom_tile(color = "white") + 
  xlab("Path: node A - node Z") + 
  theme_tufte(ticks = F) + 
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(hjust = 1),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(margin = margin(t = -2, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(margin = margin(t = 0, r = -3, b = 0, l = 0),
                                   hjust = 0)) 
 
p

ggsave("Figures/concordance_4_models_white_border_1.pdf", p, width = 10*0.8, height = 4*0.8)

