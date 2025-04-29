# load data
load(file.path(here::here(), "Lab-2", "Data_Images", "esa.salmon.rda"))

esu = unique(esa.salmon$esu_dps)

esa.salmon %>%
  filter(esu_dps == esu[11]) %>%
  
  ggplot(aes(x = spawningyear, y = log(value), color = majorpopgroup)) +
  geom_line() +
  facet_wrap(~esapopname) +
  theme_minimal()

data = esa.salmon %>%
  filter(esu_dps == esu[11]) %>%
  mutate(log_spawner = log(value)) %>%
  select(esapopname, spawningyear, log_spawner) %>%
  pivot_wider(names_from = "esapopname", values_from = "log_spawner") %>% 
  column_to_rownames(var = "spawningyear") %>%
  as.matrix() %>%
  t()

data[is.na(data)] = NA # transform NaNs in NAs

rownames(data) = c("Clearwater River", "Lochsa/Selway", "Clearwater/Lolo",
                   "Grande Ronde River", "Joseph Creek", "Asotin Creek",
                   "Tucannon River", "Little Salmon River", "Fork Salmon River",
                   "Fork Salmon/Secesh", "Salmon/Panther")

mod_list = list(
  U = "unequal",
  R = "diagonal and equal",
  Q = "equalvarcov"
)

library(MARSS)
fit = MARSS(data, model = mod_list, control = list(maxit = 10000))

autoplot(fit, plot.type = "fitted.ytT")

states = fit$states

# Replace NAs with estimated states
data[is.na(data)] = states[is.na(data)]

# Plot completed data
data %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  pivot_longer(cols = -rowname,
               names_to = "year",
               values_to = "value") %>%
  rename(region = rowname) %>%
  
  ggplot(aes(x = as.numeric(year), y = value)) +
  geom_line() +
  facet_wrap(~region) +
  theme_minimal() +
  xlab("Year") +
  ylab("Spawner abundance")
  



