#Code for Figure 2 - Whale krill consumption and human seafood consumption over the 20th century


source("Util_WKP.R")



Exploit_time_plot  <- Exploit_time_extended %>%  
  filter(Year > 1800,
         Hemisphere != "NA",
         Family %in% c("Eschrichtiidae","Balaenopteridae", "Balaenidae", "Physeteridae")) %>% 
  ggplot() + 
  geom_line(aes(Year, cumul_harvest, color = abbr_binom(`Scientific name`))) +
  scale_color_manual(values = pal) +
  scale_y_continuous(label=scales::comma) +
  facet_grid(Hemisphere~.) +
  labs(y = "Total individuals harvested",
       color = "Species") +
  guides(color=guide_legend(ncol=2)) +
  theme_bw(base_size = 18) +
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 6, face = "italic"),
        legend.key.size = unit(0.25, "cm"),
        legend.position = c(0.3, 0.85),
        legend.box.background = element_rect(colour = "black"))
Exploit_time_plot

dev.copy2pdf(file = "Rorqual harvest time.pdf", width=4, height=4)

#ggsave(file = "Rorqual harvest time.pdf", width=4, height=4, dpi=100)


Exploit_mass_time_plot  <- Exploit_time_extended %>%  
  filter(Year > 1800,
         Hemisphere != "NA",
         Family %in% c("Eschrichtiidae","Balaenopteridae", "Balaenidae", "Physeteridae")) %>% 
  ggplot() + 
  geom_line(aes(Year, cumul_mass_harvest_t/1e6, 
                color = abbr_binom(`Scientific name`))) +
  scale_color_manual(values = pal) +
  scale_y_continuous(label=scales::comma) +
  facet_grid(Hemisphere~.) +
  labs(y = "Cumulative harvested mass (Mt)",
       color = "Species") +
  guides(color=guide_legend(ncol=2)) +
  theme_bw(base_size = 10) +
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 6, face = "italic"),
        legend.key.size = unit(0.25, "cm"),
        legend.position = c(0.32, 0.85),
        legend.box.background = element_rect(colour = "black"))
Exploit_mass_time_plot

dev.copy2pdf(file = "Rorqual mass harvest time.pdf", width=4, height=4)




# Data digitized via https://automeris.io/WebPlotDigitizer/ from Christensen 2006, Figure 100: https://open.library.ubc.ca/soa/cIRcle/collections/facultyresearchandpublications/52383/items/1.0074757

GW_tot_mass <- readxl::read_excel("Mar_mamm_harvest_Christensen2006.xlsx", sheet = 4)




# Whaling catch v Marine fisheries catch, data from Pauly and Zeller 2016 https://www.nature.com/articles/ncomms10244----
global_fish_catch <- read_csv("Global_fish_catch.csv")



#Read in and edit/clean data from LB Christensen 2006----

whale_pops_time_LBC <- readxl::read_excel("Whale_pops_time_Christensen 2006.xlsx", sheet = 1) %>% 
  mutate(
    Year = round(Year, digits = 0),
    SpeciesCode = case_when(
      CommonName == "Blue Whale" ~ "bw",
      CommonName == "Fin Whale" ~ "bp",
      CommonName == "Humpback Whale" ~ "mn",
      CommonName == "Antarctic Minke Whale" ~ "bb", 
      CommonName == "Bryde's Whale" ~ "be",
      CommonName == "Sei Whale" ~ "bs",
      CommonName == "Bowhead Whale" ~ "bm",
      CommonName == "Sperm Whale" ~ "sw",
      CommonName == "Common Minke Whale" ~ "ba",
      CommonName == "North Pacific Right Whale" ~ "ej",
      CommonName == "North Atlantic Right Whale" ~ "eg",
      CommonName == "Southern Right Whale" ~ "ea"
      ),
    Species = case_when(
      SpeciesCode == "bw" ~ "Balaenoptera musculus",
      SpeciesCode == "bp" ~ "Balaenoptera physalus",
      SpeciesCode == "mn" ~ "Megaptera novaeangliae",
      SpeciesCode == "bb" ~ "Balaenoptera bonaerensis", 
      SpeciesCode == "be" ~ "Balaenoptera brydei",
      SpeciesCode == "bs" ~ "Balaenoptera borealis",
      SpeciesCode == "bm" ~ "Balaena mysticetus",
      SpeciesCode == "eg" ~ "Eubalaena glacialis",
      SpeciesCode == "ej" ~ "Eubalaena japonica",
      SpeciesCode == "ea" ~ "Eubalaena australis",
      SpeciesCode == "sw" ~ "Physeter catodon", 
      SpeciesCode == "ba" ~ "Balaenoptera acutorostrata"
      ),
    Avg_mass_t = case_when(Species == "Balaenoptera bonaerensis" ~ 6.56620,
                           Species == "Balaenoptera borealis" ~ 16.81080,
                           Species == "Balaenoptera physalus" ~ 55.59015,
                           Species == "Balaenoptera musculus" ~ 102.73640,
                           Species == "Megaptera novaeangliae" ~ 30.40820),
    Pop_mass_t = Pop_size*Avg_mass_t,
    MedPreyPerYr_t = as.numeric(MedPreyPerDay_t)*Pop_size*100 # Assuming a 100-day feeding season
    )
    



PreyCons_Allwhales_LBC  <- whale_pops_time_LBC %>% 
  filter(Year < 2002) %>% 
  group_by(Year, Species, Region) %>% 
  summarize(Tot_prey_cons = sum(MedPreyPerYr_t),
            # Tot_N_recycled = sum(N_RecycledPerYr_t),
            # Tot_P_recycled = sum(P_RecycledPerYr_t)
            ) %>%   
  drop_na(Tot_prey_cons)



Total_biomass_LBC <- whale_pops_time_LBC %>% 
  filter(Region == "Southern Hemisphere",
    Species %in% c("Balaenoptera bonaerensis", "Balaenoptera borealis",
                        "Balaenoptera physalus", "Balaenoptera musculus",
                        "Megaptera novaeangliae")
    ) %>% 
  group_by(Year) %>% 
  summarise(Total_mass_t = sum(Pop_mass_t))



#Plot data from LB Christensen 2006---- 

PreyCons_v_MarineFishCatch_LBC <-   ggplot() + 
  geom_area(data = filter(PreyCons_Allwhales_LBC, Year > 1900, 
                          Region == "Southern Hemisphere",
                          !Species %in% c("Balaenoptera borealis", "Balaenoptera brydei", 
                                          "Eubalaena australis")), 
            aes(Year, Tot_prey_cons/1e6, fill = abbr_binom(Species)),
            alpha = 0.8) +
  # geom_line(data = SOKrillCons_Allwhales, 
  #           aes(Year, Tot_krill_cons_EnConv/1e6), color = "darkblue", linetype = "dashed") +
  geom_line(data = Total_biomass_LBC, 
            aes(Year, Total_mass_t/1e6),  
            color = "black", size = 1.05, linetype = "dashed") +
  geom_line(data = global_fish_catch, 
            aes(Year, Reconstructed_catch_t/1e6),  
            color = "darkblue", size = 1.05) +
  geom_segment(x = 1911.5, y = 0, xend = 1911.5, yend = 109) +
  #geom_vline(xintercept = 1911.5) +
  scale_y_continuous(name = bquote(Estimated~italic(E.~superba)~consumption~(Mt~yr^1)),
                     sec.axis = sec_axis(~ .*1, 
                                         name=bquote(Global~marine~fisheries~catch~(Mt~yr^1)),
                                         label=scales::comma,
                                         breaks = c(0,50,100,150,200,250,300, 350)),
                     label=scales::comma,
                     limits = c(0,360), breaks = c(0,50,100,150,200,250,300, 350)) +
  scale_x_continuous(limits = c(1895, 2010), breaks = c(1900,1920,1940,1960,1980,2000)) +
  scale_fill_manual(values = pal) +
  labs(fill = "Species",
       size = "") +
  guides(color=guide_legend(ncol=2)) +
  theme_classic(base_size = 22) +
  theme(
    # axis.title.x = element_text(size=16),
    # axis.text.x = element_text(size=12),
    # axis.title.y = element_text(size=16),
    axis.title.y.right = element_text(color = "darkblue"),
    #axis.text.y = element_text(size=12),
    axis.text.y.right = element_text(color = "darkblue"),
    legend.text = element_text(face = "italic")
  )
PreyCons_v_MarineFishCatch_LBC


ggsave("PreyCons_v_MarineFishCatch_LBC_forWKP.pdf")
#dev.copy2pdf(file = "Whale prey vs human prey consLBC.pdf", width=7.5, height=4)




PreyCons_prop_LBC <-   ggplot() + 
  geom_area(data = filter(PreyCons_Allwhales_LBC, Year > 1900, 
                          Region == "Southern Hemisphere",
                          !Species %in% c("Balaenoptera brydei", "Eubalaena australis")),
            aes(Year, Tot_prey_cons/1e6, fill = abbr_binom(Species)),
            alpha = 0.8, position = "fill") +
  # scale_y_continuous(name = "E. superba consumption (proportional)",
  #                    limits = c(0,1)) +
  scale_x_continuous(limits = c(1900, 2000), breaks = c(1900,1920,1940,1960,1980,2000)) +
  scale_fill_manual(values = pal) +
  labs(y = bquote(Proportional~italic(E.~superba)~consumption),
       fill = "Species",
       size = "") +
  guides(color=guide_legend(ncol=2)) +
  theme_bw(base_size = 10) +
  theme(
    axis.title.x = element_text(size=12),
    axis.text.x = element_text(size=10),
    axis.title.y = element_text(color = "darkblue", size=12),
    axis.text.y = element_text(color = "darkblue", size=10),
    legend.text = element_text(face = "italic")
  )
PreyCons_prop_LBC



dev.copy2pdf(file = "Proportional Ant krill consumptionLBC.pdf", width=7, height=4)





















# Nutrients recycled by whales vs. used by humans too prelim to use most likely ----

# numbers from Smil, V. 2011. Nitrogen cycle and world food production. World Agriculture 
Early_N_prod <- tibble(Year = c(1900, 1910, 1920, 1930, 1940, 1950),
                       N_yr_t = c(100000, 700000, 1000000, 2100000, 3200000, 4800000))


Human_N_production <- read_csv("nitrogen-fertilizer-production.csv") %>% 
  #filter(Year > 1799) %>% 
  filter(Entity != "World", Code != "NA") %>% 
  group_by(Year) %>% 
  summarise(N_yr_t = sum(`Nitrogen fertilizer production (FAO (2017))`)) %>% 
  bind_rows(Early_N_prod) %>% 
  arrange(Year)



Human_P_production <- read_csv("phosphate-fertilizer-production.csv") %>% 
  #filter(Year > 1799) %>% 
  filter(Entity != "World", Code != "NA") %>% 
  group_by(Year) %>% 
  summarise(P_yr_t = sum(`Phosphate Production (FAO (2017))`)) %>% 
  bind_rows(read_csv("Early_P_prod.csv"))  %>% 
  arrange(Year)



Exploit_time_nutrients <- Exploit_time_extended %>%  
  filter(Year > 1800,
         Hemisphere != "NA",
         Family %in% c("Eschrichtiidae","Balaenopteridae", "Balaenidae", "Physeteridae"))




WhaleNRe_v_HumanNProd <-   ggplot() + 
  geom_line(data = Human_N_production, 
            aes(Year, N_yr_t/1e6), 
            color = "darkred") +

  geom_line(data = filter(PreyCons_Allwhales, Year > 1900),
            aes(Year, Tot_N_recycled/1e6), color = "darkblue") +

  scale_y_continuous(name = "N recycled by whales globally (Mt/yr)",
                     sec.axis = sec_axis(~ .*1, name="Synthetic N fertilizer produced globally (Mt/yr)",
                                         label=scales::comma),
                     label=scales::comma,
                     limits = c(0,110)) +
  scale_x_continuous(limits = c(1895, 2010), breaks = c(1900,1920,1940,1960,1980,2000)) +
  guides(color=guide_legend(ncol=2)) +
  theme_bw(base_size = 10) +
  theme(
    axis.title.x = element_text(size=12),
    axis.text.x = element_text(size=10),
    axis.title.y = element_text(color = "darkblue", size=12),
    axis.title.y.right = element_text(color = "darkred", size=12),
    axis.text.y = element_text(color = "darkblue", size=10),
    axis.text.y.right = element_text(color = "darkred", size=10),
  )
WhaleNRe_v_HumanNProd


dev.copy2pdf(file = "Whale vs human N produced.pdf", width=5, height=4)








WhalePRe_v_HumanPProd <-   ggplot() + 
  geom_line(data = Human_P_production, 
            aes(Year, P_yr_t/1e6), 
            color = "darkred") +
  
  geom_line(data = filter(PreyCons_Allwhales, Year > 1900),
            aes(Year, Tot_N_recycled/1e6), color = "darkblue") +
  
  scale_y_continuous(name = "P recycled by whales globally (Mt/yr)",
                     sec.axis = sec_axis(~ .*1, name="P fertilizer produced globally (Mt/yr)",
                                         label=scales::comma),
                     label=scales::comma,
                     limits = c(0,50)) +
  scale_x_continuous(limits = c(1895, 2010), breaks = c(1900,1920,1940,1960,1980,2000)) +
  guides(color=guide_legend(ncol=2)) +
  theme_bw(base_size = 10) +
  theme(
    axis.title.x = element_text(size=12),
    axis.text.x = element_text(size=10),
    axis.title.y = element_text(color = "darkblue", size=12),
    axis.title.y.right = element_text(color = "darkred", size=12),
    axis.text.y = element_text(color = "darkblue", size=10),
    axis.text.y.right = element_text(color = "darkred", size=10),
  )
WhalePRe_v_HumanPProd


dev.copy2pdf(file = "Whale vs human P produced.pdf", width=5, height=4)



 