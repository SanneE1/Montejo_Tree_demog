# Climate data

# Montly climate
clim_month <- full_join(read.csv("data/ECAnD_DailyTemp_Salamanca.txt", na.strings = "-9999"), 
                  read.csv("data/ECAnD_Precip_Salamanca.txt", na.strings = "-9999")) %>%
  rename(temp = TG,
         precip = RR) %>%
  mutate(year = as.numeric(substr(DATE, 1, 4)),
         month = as.numeric(substr(DATE, 5, 6)),
         day = as.numeric(substr(DATE, 7,8))) %>%
  group_by(year, month) %>%
  summarise(av_temp = mean(temp, na.rm = T)/10,           #recorder in 0.1 degrees celcius
            av_precip = mean(precip, na.rm = T)/10) %>%   # recorded in 0.1 mm
  ungroup %>% group_by(month) %>%
  mutate(temp = scale(av_temp),
         precip = scale(av_precip)) %>%
  ungroup %>% rowwise() %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "/"),format = "%Y/%m/%d"),
         time = case_when(between(date, as.Date("2005-01-01"), as.Date("2014-12-31")) ~ "2005-2015",
                          between(date, as.Date("2015-01-01"), as.Date("2021-12-31")) ~ "2015-2021",
         TRUE ~ NA)) %>%
  filter(!is.na(time))

clim_month %>% group_by(time) %>%
  summarise(temp = mean(temp, na.rm = T),
           precip = mean(precip, na.rm = T))



# Plot monthly climate sequences. both average values, and anomalies

ggplot(clim_month) +
  geom_line(aes(x = date, y = av_precip), size = 1, colour = "blue") + 
  theme_bw() + theme(text = element_text(size = 16))

ggplot(clim_month) +
  geom_line(aes(x = date, y = av_temp), size = 1, colour = "red") + 
  theme_bw() + theme(text = element_text(size = 16))

ggplot(clim_month) +
  geom_line(aes(x = date, y = precip), size = 1, colour = "blue") + 
  geom_hline(aes(yintercept = 0)) +
  geom_smooth(aes(x = date, y = precip), method = "lm", se = FALSE, colour = "blue", size = 1.5) +
  theme_bw() + theme(text = element_text(size = 16))

ggplot(clim_month) +
  geom_line(aes(x = date, y = temp), size = 1, colour = "red") + 
  geom_hline(aes(yintercept = 0)) +
  geom_smooth(aes(x = date, y = temp), method = "lm", se = FALSE, colour = "red", size = 1.5) +
  theme_bw() + theme(text = element_text(size = 16))




ggplot(clim_month) +
  geom_boxplot(aes(x = time, y = precip), fill = "blue", alpha = 0.5) +
  theme_bw() + theme(text = element_text(size = 16))

ggplot(clim_month) +
  geom_boxplot(aes(x = time, y = temp), fill = "red", alpha = 0.5) +
  theme_bw() + theme(text = element_text(size = 16))





