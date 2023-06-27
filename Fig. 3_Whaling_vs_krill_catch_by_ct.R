# Whale catches and krill catches by country

source("Util_WKP.R")



pal_ct <- c("Norway" = "firebrick3",
            "Japan" = "darkred",
            "United Kingdom" = "blue2",
            "USSR" = "gold2",
            "Argentina" = "goldenrod3",
            "South Africa" = "darkgreen",
            "Netherlands" = "royalblue3",
            "Panama" = "navy",
            "Germany" = "black",
            "USA" = "dodgerblue4",
            "Denmark" = "red",
            "South Korea" = "deepskyblue3",
            "China" = "darkgoldenrod1",
            "Ukraine" = "yellow1",
            "Poland" = "red",
            "Chile" = "royalblue4",
            "Vanuatu" = "goldenrod1",
            "Russian Federation" = "blue",
            "Uruguay" = "darkorange4",
            "Other nations combined" = "grey60"
            
            )


# Summary datasets ----
whaling_summary_by_nation <- IWC_SO_whaling_full_df %>%
  group_by(Nt) %>%
  count()


whaling_summary_by_nation <- IWC_SO_whaling_full_df %>% 
  group_by(Nt) %>% 
  count() %>% 
  mutate(ct_full = case_when(Nt == "No" ~ "Norway",
                             Nt == "Jp" ~ "Japan",
                             Nt == "UK" ~ "United Kingdom",
                             Nt == "UR" ~ "USSR",
                             Nt == "Ar" ~ "Argentina",
                             Nt == "SA" ~ "South Africa",
                             Nt == "Ne" ~ "Netherlands",
                             Nt == "Pa" ~ "Panama",
                             Nt == "Ge" ~ "Germany",
                             Nt == "US" ~ "USA",
                             Nt == "Dm" ~ "Denmark"
                             ),
    Total_catch_prop = n/sum(whaling_summary_by_nation$n)) %>% 
  arrange(-Total_catch_prop)


wn_other_n = sum(whaling_summary_by_nation$n) - sum(whaling_summary_by_nation$n[1:4])  # for all whales killed by "other nation" (not top 4)  
wn_other_prop = sum(whaling_summary_by_nation$Total_catch_prop) - sum(whaling_summary_by_nation$Total_catch_prop[1:4])  # for all whales killed by "other nation" (not top 4)  


df2 = data.frame(Nt = "Ot", n = wn_other_n, ct_full = "Other nations combined", 
                 Total_catch_prop = wn_other_prop)
whaling_summary_by_nation <- whaling_summary_by_nation %>% 
  rows_insert(df2)


#View(whaling_summary_by_nation)


# Whaling simple bar plots ----

whaling_by_nation_num <- ggplot(filter(whaling_summary_by_nation, 
                                       ct_full %in% c("Norway", "Japan", "United Kingdom", "USSR", "Other nations combined")),
                                 aes(x= fct_reorder(ct_full, n, .desc = TRUE), 
                                     y = n, 
                                     fill = ct_full)) +
  geom_bar(stat="identity") +
  labs( x = "",
        y = "Total whales harvested") +
  scale_y_continuous(labels = scales::comma) +  
  scale_fill_manual(values = pal_ct) +
  theme_classic(base_size = 24) +
  theme(legend.position = "none",
       axis.text.x = element_text(angle = 35, vjust = 1, hjust=1)
        )

whaling_by_nation_num

#ggsave("whaling_by_nation_num.pdf")

# dev.copy2pdf(file="Whaling by nation.pdf", 
#              width=4, height=5)




whaling_by_nation_prop <- ggplot(whaling_summary_by_nation,
                                    aes(x= fct_reorder(ct_full, Total_catch_prop, .desc = TRUE), 
                                        y = Total_catch_prop, 
                                        fill = ct_full)) +
  geom_bar(stat="identity") +
  labs( x = "",
    y = "Proportion of total catch (all time)") +
  scale_y_continuous(labels = scales::comma) +  
  scale_fill_manual(values = pal_ct) +
  theme_classic() +
  theme(legend.position = "none",
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

whaling_by_nation_prop




# Whaling plots by year----

whaling_summ_by_nation_yr <- IWC_SO_whaling_full_df %>% 
  group_by(Nt, Year) %>% 
  count() %>% 
  mutate(ct_full = case_when(Nt == "No" ~ "Norway",
                             Nt == "Jp" ~ "Japan",
                             Nt == "UK" ~ "United Kingdom",
                             Nt == "UR" ~ "USSR",
                             Nt == "Ar" ~ "Argentina",
                             Nt == "SA" ~ "South Africa",
                             Nt == "Ne" ~ "Netherlands",
                             Nt == "Pa" ~ "Panama",
                             Nt == "Ge" ~ "Germany",
                             Nt == "US" ~ "USA",
                             Nt == "Dm" ~ "Denmark"
  ))


View(whaling_summ_by_nation_yr)



whaling_summ_by_nation_yr_other <- whaling_summ_by_nation_yr %>% 
  filter(!ct_full %in% c("Norway", "Japan", "United Kingdom", "USSR")) %>% 
  group_by(Year) %>% 
  summarise(n = sum(n)) %>% 
  mutate(Nt = "Other",
         ct_full = "Other nations combined")

whaling_summ_by_nation_yr_comb <- rbind(whaling_summ_by_nation_yr_other,whaling_summ_by_nation_yr)


# Check the levels of ct_full
levels(whaling_summ_by_nation_yr_comb$ct_full)

# Add the missing levels if they are not present
levels(whaling_summ_by_nation_yr_comb$ct_full) <- c(levels(whaling_summ_by_nation_yr_comb$ct_full), "Norway", "United Kingdom", "USSR", "Other nations combined")

# Reorder the levels of ct_full
whaling_summ_by_nation_yr_cut <- whaling_summ_by_nation_yr_comb %>% 
  filter(ct_full %in% c("Norway", "Japan", "United Kingdom", "USSR", "Other nations combined")) %>%
  mutate(ct_full = fct_relevel(ct_full, c("Japan", "Norway", "United Kingdom", "USSR", "Other nations combined")))





whaling_by_nation_yr <- ggplot(whaling_summ_by_nation_yr_cut,
              aes(x = Year, y = n, fill = ct_full)) +
  geom_area() +
  labs(
       fill = "Nation",
       x = "Year",
       y = "Total catch") +
  scale_fill_manual(values = pal_ct) +
  theme_classic(base_size = 16) +
  scale_x_continuous(limits = c(1900, 2020), breaks = seq(1900, 2020, by = 20))
whaling_by_nation_yr

#ggsave("whaling_by_nation_yr.pdf")






# Krill fishing plots ----

krill_fishery_landings <- read_csv("CCAMLR_Statistical_Bulletin_Volume_34/catch_and_effort/combined.csv") %>% 
  filter(effort_or_catch == "catch",
         taxon_code == "KRI")


# Krill fishing simple bar plots ----

krill_fishing_by_nation <- krill_fishery_landings %>% 
  group_by(vessel_nationality) %>% 
  mutate(vessel_nationality = factor(vessel_nationality)) %>% 
  summarize(Total_wt_tonne = sum(greenweight_caught_tonne),
            Total_catch_prop = Total_wt_tonne/10072547) %>% 
  arrange(-Total_wt_tonne)
krill_fishing_by_nation$vessel_nationality <- recode(krill_fishing_by_nation$vessel_nationality,
                                                     `United States of America` = "USA",
                                                     `Korea, Republic of` = "South Korea",
                                                     `German Democratic Republic` = "Germany")


sum(krill_fishing_by_nation$Total_wt_tonne) - sum(krill_fishing_by_nation$Total_wt_tonne[1:4])  # for all krill harvested by "other nations" (not top 4)  




kf_other_n = sum(krill_fishing_by_nation$Total_wt_tonne) - sum(krill_fishing_by_nation$Total_wt_tonne[1:4])  # for all krill harvested by "other nations" (not top 4)  
kf_other_prop = sum(krill_fishing_by_nation$Total_catch_prop) - sum(krill_fishing_by_nation$Total_catch_prop[1:4])  # for all krill harvested by "other nations" (not top 4)  


# 
# df_k = data.frame(vessel_nationality = factor("Other nations combined"), 
#                   Total_wt_tonne=kf_other_n, 
#                   Total_catch_prop  = kf_other_prop)
# 
# krill_fishing_by_nation <- krill_fishing_by_nation %>% 
#   rows_insert(df_k)


# create a new row
df_k <- data.frame(vessel_nationality = "Other nations combined", Total_wt_tonne = kf_other_n, Total_catch_prop = kf_other_prop)

# convert vessel_nationality to factor with the same levels as krill_fishing_by_nation
df_k$vessel_nationality <- factor(df_k$vessel_nationality)

#Merge together
krill_fishing_by_nation <- rbind(krill_fishing_by_nation,df_k)




#View(krill_fishing_by_nation)





krill_fishing_by_nation_num_AT <- ggplot(filter(krill_fishing_by_nation, 
                                                vessel_nationality %in% c("Norway", "Japan", "South Korea", "USSR", "Other nations combined")),
                                aes(x= fct_relevel(vessel_nationality, c("USSR", "Norway", "Japan", "South Korea", "Other nations combined")), 
                                    y = Total_wt_tonne/1e6, 
                                    fill = vessel_nationality)) +
  geom_bar(stat="identity") +
  labs( x = "",
        y = "Total krill catch all time (Mt)") +
  scale_y_continuous(labels = scales::comma) + 
  scale_fill_manual(values = pal_ct) +
  theme_classic(base_size = 24) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 35, vjust = 1, hjust=1))

krill_fishing_by_nation_num_AT

#ggsave("krill_fishing_by_nation_num_AT.pdf")

# dev.copy2pdf(file="Krill catch all time.pdf", 
#              width=4, height=5)






krill_fishing_by_nation_21c <- krill_fishery_landings %>% 
  filter(year >1999) %>% 
  group_by(vessel_nationality) %>% 
  mutate(vessel_nationality = factor(vessel_nationality)) %>% 
  summarize(Total_wt_tonne = sum(greenweight_caught_tonne),
            Total_catch_prop = Total_wt_tonne/10072547) %>% 
  arrange(-Total_wt_tonne)
krill_fishing_by_nation_21c$vessel_nationality <- recode(krill_fishing_by_nation_21c$vessel_nationality,
                                                     `United States of America` = "USA",
                                                     `Korea, Republic of` = "South Korea",
                                                     `German Democratic Republic` = "Germany")

krill_fishing_by_nation_num_21c <- ggplot(krill_fishing_by_nation_21c,
                                         aes(x= fct_reorder(vessel_nationality, Total_wt_tonne, .desc = TRUE), 
                                             y = Total_wt_tonne/1e6, 
                                             fill = vessel_nationality)) +
  geom_bar(stat="identity") +
  labs( x = "",
        y = "Total krill catch since 2000 (Mt)") +
  scale_y_continuous(labels = scales::comma) +  
  scale_fill_manual(values = pal_ct) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

krill_fishing_by_nation_num_21c


# dev.copy2pdf(file="Krill catch 21st century.pdf", 
#              width=7, height=5)






# Krill fishing plots by year----

krill_fishing_by_nation_yr <- krill_fishery_landings %>% 
  group_by(vessel_nationality) %>% 
  mutate(vessel_nationality = factor(vessel_nationality)) %>% 
  summarize(Total_wt_tonne = sum(greenweight_caught_tonne),
            Total_catch_prop = Total_wt_tonne/10072547) %>% 
  arrange(-Total_wt_tonne)
krill_fishing_by_nation$vessel_nationality <- recode(krill_fishing_by_nation$vessel_nationality,
                                                     `United States of America` = "USA",
                                                     `Korea, Republic of` = "South Korea",
                                                     `German Democratic Republic` = "Germany")


krill_fishing_by_nation_yr <- krill_fishery_landings %>% 
  group_by(vessel_nationality, year) %>% 
  mutate(vessel_nationality = factor(vessel_nationality)) %>% 
  summarize(Total_wt_tonne = sum(greenweight_caught_tonne),
            #Total_catch_prop = Total_wt_tonne/10072547
            ) %>% 
  arrange(-Total_wt_tonne)
krill_fishing_by_nation_yr$vessel_nationality <- recode(krill_fishing_by_nation_yr$vessel_nationality,
                                                     `United States of America` = "USA",
                                                     `Korea, Republic of` = "South Korea",
                                                     `German Democratic Republic` = "Germany")



krill_fishing_by_nation_yr_other <- krill_fishing_by_nation_yr %>% 
  filter(!vessel_nationality %in% c("USSR", "Norway", "Japan", "South Korea")) %>% 
  group_by(year) %>% 
  summarise(Total_wt_tonne = sum(Total_wt_tonne)) %>% 
  mutate(vessel_nationality = "Other nations combined")

krill_fishing_by_nation_yr_comb <- krill_fishing_by_nation_yr %>% 
  filter(vessel_nationality %in% c("USSR", "Norway", "Japan", "South Korea")) %>% 
  rbind(krill_fishing_by_nation_yr_other) 


# Reorder levels of vessel_nationality
krill_fishing_by_nation_yr_comb$vessel_nationality <- 
  fct_relevel(krill_fishing_by_nation_yr_comb$vessel_nationality, 
              "Japan", "Norway", "South Korea", "USSR", "Other nations combined")


krill_fishing_by_nation_yr_plot <- ggplot(krill_fishing_by_nation_yr_comb,
                               aes(x = year, 
                                   y = Total_wt_tonne/1e6, 
                                   fill = vessel_nationality)) +
  geom_area() +
  labs(
    fill = "Nation",
    x = "Year",
    y = "Total catch (Mt)") +
  scale_fill_manual(values = pal_ct) +
  theme_classic(base_size = 16) +
  scale_x_continuous(limits = c(1970, 2020), breaks = seq(1970, 2020, by = 10))


krill_fishing_by_nation_yr_plot

# ggsave("krill_fishing_by_nation_yr_plot.pdf")








# Whaling and krill fishing on the same timeline----

library(ggpattern)
library(magick)

whaling_kf_by_nation_yr <- ggplot() +
  
  geom_area(data = whaling_summ_by_nation_yr_cut,
            aes(x = Year, y = n, fill = ct_full)) +
  geom_area(data = krill_fishing_by_nation_yr_comb,
            aes(x = year, y = Total_wt_tonne/15, fill = vessel_nationality),
            alpha = 0.78
  ) +
  # geom_area_pattern(data = krill_fishing_by_nation_yr_comb,
  #           aes(x = year, y = Total_wt_tonne/15, fill = vessel_nationality),
  #           alpha = 0.85,
  #           pattern = "stripe", pattern_alpha = 0.5, pattern_scale = .2
  #           ) +
  
  
  scale_fill_manual(values = pal_ct) +
  scale_x_continuous(limits = c(1900, 2020), breaks = seq(1900, 2020, by = 20)) +
  labs(fill = "Nation", x = "Year") +
  theme_classic(base_size = 18) +
  scale_y_continuous(name = "Whale catch (individuals)", limits = c(0, 40000),
                     labels = scales::comma,
                     sec.axis = sec_axis(~. * 15, name = "Krill catch (tonnes)",
                                         breaks = seq(0, 1e6, by = 2e5),
                                         labels = scales::comma
                     )
  )


whaling_kf_by_nation_yr

ggsave("whaling_kf_by_nation_yr.pdf")




  




