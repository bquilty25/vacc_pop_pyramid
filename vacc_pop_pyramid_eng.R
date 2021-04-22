#Load required packages and fonts (Windows)
require(pacman)
pacman::p_load(tidyverse,readxl,lemon,scales,extrafont,RCurl)
loadfonts(device = "win")

#Data from Fig 58: https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/979631/Weekly_Flu_and_COVID-19_report_w16.pdf
vacc_dat <- tribble(~age,~sex,~uptake,
                    "80+","Male",0.947,
                    "75-79","Male",0.948,
                    "70-74","Male",0.935,
                    "65-69","Male",0.904,
                    "60-64","Male",0.868,
                    "55-59","Male",0.835,
                    "50-54","Male",0.795,
                    "40-49","Male",0.384,
                    "30-39","Male",0.179,
                    "20-29","Male",0.134,
                    "<20","Male",0.013,
                    "80+","Female",0.949,
                    "75-79","Female",0.95,
                    "70-74","Female",0.941,
                    "65-69","Female",0.917,
                    "60-64","Female",0.895,
                    "55-59","Female",0.875,
                    "50-54","Female",0.856,
                    "40-49","Female",0.524,
                    "30-39","Female",0.30,
                    "20-29","Female",0.223,
                    "<20","Female",0.019)

#Download and process English age data
download.file("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland/mid2019april2020localauthoritydistrictcodes/ukmidyearestimates20192020ladcodes.xls","temp.xls",mode="wb")

pop_dat_male <- readxl::read_excel("temp.xls",
                                   sheet=7,skip=4) %>% 
  slice(4) %>% 
  pivot_longer(`0`:`90+`) %>% 
  select(-c(Code,Name,Geography1,`All ages`)) %>% 
  mutate(sex="Male",
         name=extract_numeric(name)) %>% 
  mutate(age=cut(name,
                 breaks=c(-Inf,20,30,40,50,55,60,65,70,75,80,Inf),
                 labels=c("<20", "20-29", "30-39", "40-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+"))) %>% 
  group_by(age,sex) %>% 
  summarise(pop=sum(value)) %>% 
  mutate(sex=str_to_title(sex))

pop_dat_female <- readxl::read_excel("temp.xls",sheet=8,skip=4) %>% 
  slice(4) %>% 
  pivot_longer(`0`:`90+`) %>% 
  select(-c(Code,Name,Geography1,`All ages`)) %>% 
  mutate(sex="Female",
         name=extract_numeric(name)) %>% 
  mutate(age=cut(name,
                 breaks=c(-Inf,20,30,40,50,55,60,65,70,75,80,Inf),
                 labels=c("<20", "20-29", "30-39", "40-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+"))) %>% 
  group_by(age,sex) %>% 
  summarise(pop=sum(value)) %>% 
  mutate(sex=str_to_title(sex))

pop_dat <- bind_rows(pop_dat_male,pop_dat_female)

#Join to vaccine data
plot_dat <- left_join(vacc_dat,pop_dat,by=c("age","sex")) %>% 
  mutate(`At least 1 dose`=pop*uptake,
         Unvaccinated=pop*(1-uptake)) %>% 
  pivot_longer(c(`At least 1 dose`,Unvaccinated)) %>% 
  mutate(name=fct_rev(name))

#Plot
ggplot(data=plot_dat,
       mapping = aes(x = ifelse(sex == "Male", yes = -value, no = value), 
               y = age, fill = sex,alpha=name),position="stack")+
  geom_col()+
  labs(x="",
       y="Age group",
       title="Vaccine uptake in England by age and sex",
       subtitle="Sources: Vaccine data PHE (April 2021), Age data ONS (April 2020) | Plot by @BQuilty")+
  scale_alpha_manual(name="",values=c(0.4,1))+
  scale_fill_brewer(name="",type="qual",palette = "Set1")+
  scale_x_symmetric(breaks=seq(-7.5e6,7.5e6,2.5e6),labels=c("7.5M","5M","2.5M","0","2.5M","5M","7.5M")) + 
  theme_minimal()+
  theme(legend.position = "bottom",
        text=element_text(family="Roboto"))+
  guides(fill = guide_legend(reverse=T))

#Save plot
ggsave("pop_vacc.png",units="mm",dpi=600,width=200,height=112.5)
