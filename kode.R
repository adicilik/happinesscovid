library(dplyr)
library(ggplot2)
library(stargazer)

sptk21 <- read.csv("sptk2021.csv")

pandemic <- data.frame(happy = sptk21$R1501-sptk21$R1502,
                       prov = sptk21$R101_NAMA,
                       urban = sptk21$R105,
                       sex = sptk21$B4K4,
                       age = sptk21$B4K5,
                       marital = sptk21$B4K6,
                       education = sptk21$B4K7,
                       length_stay = sptk21$LAMA_TINGG,
                       employment = sptk21$R601A,
                       income = sptk21$R603)
str(pandemic)

pandemic <- pandemic %>% 
  mutate(urban = recode(urban, `1`="Urban",`2`="Rural"),
         sex = recode(sex, `1`="Male", `2`="Female"),
         age = ifelse(age<=20, "<=20",
                      ifelse(age<=30, "21-30",
                             ifelse(age<=40, "31-40",
                                    ifelse(age<=50, "41-50",
                                           ifelse(age<=60, "51-60", ">60"))))),
         marital = ifelse(marital==2, "Married", "Not married"),
         education = ifelse(education<=3, "Low",
                            ifelse(education<=5, "Medium", "High")),
         length_stay = ifelse(length_stay<=20, "<=20",
                              ifelse(length_stay<=40, "21-40",
                                     ifelse(length_stay<=60, "41-60",
                                            ifelse(length_stay<=80, "61-80", ">80")))),
         employment = recode(employment, `1`="Employed", `2`="Unemployed"),
         income = recode(income, `1`=">4M", `2`="2.5M-4M", `3`="1.5M-2.5M",
                         `4`="1M-1.5M", `5`="<1M"))


#### Visualisasi


aggregate(happy~prov, pandemic, mean) %>% 
  arrange(happy) %>% mutate(prov = factor(prov, levels=prov)) %>% 
  ggplot(aes(happy, prov)) +
  geom_bar(stat="identity") + geom_vline(xintercept=0) +
  labs(title="Happiness score difference before & after pandemic\nbased on provinces")

aggregate(happy~urban, pandemic, mean) %>% 
  ggplot(aes(happy, urban)) +
  geom_bar(stat="identity") + geom_vline(xintercept=0) +
  labs(title="Happiness score difference before & after pandemic\nbased on urban-rural location")

aggregate(happy~sex, pandemic, mean) %>% 
  ggplot(aes(happy, sex)) +
  geom_bar(stat="identity") + geom_vline(xintercept=0) +
  labs(title="Happiness score difference before & after pandemic\nbased on sex")

aggregate(happy~age, pandemic, mean) %>% 
  mutate(age = factor(age, levels=c("<=20","21-30","31-40","41-50","51-60",">60"))) %>% 
  ggplot(aes(happy, age)) +
  geom_bar(stat="identity") + geom_vline(xintercept=0) +
  labs(title="Happiness score difference before & after pandemic\nbased on age interval")

aggregate(happy~marital, pandemic, mean) %>% 
  ggplot(aes(happy, marital)) +
  geom_bar(stat="identity") + geom_vline(xintercept=0) +
  labs(title="Happiness score difference before & after pandemic\nbased on marital status")

aggregate(happy~education, pandemic, mean) %>% 
  mutate(education=factor(education, levels=c("Low","Medium","High"))) %>% 
  ggplot(aes(happy, education)) +
  geom_bar(stat="identity") + geom_vline(xintercept=0) +
  labs(title="Happiness score difference before & after pandemic\nbased on education level")

aggregate(happy~length_stay, pandemic, mean) %>% 
  mutate(length_stay=factor(length_stay, levels=c("<=20","21-40","41-60","61-80",">80"))) %>%
  ggplot(aes(happy, length_stay)) +
  geom_bar(stat="identity") + geom_vline(xintercept=0) +
  labs(title="Happiness score difference before & after pandemic\nbased on years of stay")

aggregate(happy~employment, pandemic, mean) %>% 
  ggplot(aes(happy, employment)) +
  geom_bar(stat="identity") + geom_vline(xintercept=0) +
  labs(title="Happiness score difference before & after pandemic\nbased on employment")

aggregate(happy~income, pandemic, mean) %>% 
  mutate(income = factor(income, levels=c("<1M","1M-1.5M","1.5M-2.5M","2.5M-4M",">4M"))) %>% 
  ggplot(aes(happy, income)) +
  geom_bar(stat="identity") + geom_vline(xintercept=0) +
  labs(title="Happiness score difference before & after pandemic\nbased on income")


### 

# Regression model
pandemic <- 
  data.frame(province = sptk21$R101_NAMA,
             urban = ifelse(sptk21$R105==1, 1, 0),
             female = ifelse(sptk21$B4K4==2, 1, 0),
             age = sptk21$B4K5,
             marital = sptk21$B4K6,
             education = sptk21$B4K7,
             length_stay = sptk21$LAMA_TINGG,
             unemployed = ifelse(sptk21$R601A==2, 1, 0),
             income = sptk21$R603,
             happy_after = sptk21$R1501,
             happy_before = sptk21$R1502) %>% 
  mutate(marital = factor(recode(marital, `1`="not married yet", `2`="married", `3`="divorced", `4`="widowed"),
                          levels=c("not married yet","married","divorced","widowed")),
         happy_diff = happy_after-happy_before,
         happy_diff_cat = ifelse(happy_diff>0, 1, 0))
str(pandemic)

pandemic3 <- pandemic %>% select(!c(province, income))
happy_after <- lm(happy_after~., pandemic3 %>% select(!c("happy_before","happy_diff","happy_diff_cat")))
happy_before <- lm(happy_before~., pandemic3 %>% select(!c("happy_after","happy_diff","happy_diff_cat")))
happy_diff <- lm(happy_diff~., pandemic3 %>% select(!c("happy_after","happy_before","happy_diff_cat")))
happy_diff_cat <- glm(happy_diff_cat~., family=binomial, data=pandemic3 %>% select(!c("happy_after","happy_before","happy_diff")))
summary(happy_after)
summary(happy_before)
summary(happy_diff)
summary(happy_diff_cat)
stargazer(happy_before, happy_after, happy_diff, happy_diff_cat, type="html")


## Decision tree

pandemic4 <- pandemic3 %>% 
  mutate(suffer_covid = ifelse(happy_diff<0, 1, 0),
         marital = as.numeric(marital)) %>% 
  select(!(happy_after:happy_diff_cat))
str(pandemic4)

tree <- ctree(suffer_covid~., pandemic4)
plot(tree)

### VERSI 2

library(dplyr)
library(ggplot2)

sptk21 <- read.csv("sptk2021.csv")
names(sptk21)

pandemic <- data.frame(happy_diff = sptk21$R1501-sptk21$R1502,
                       happy_after = sptk21$R1501,
                       happy_before = sptk21$R1502,
                       prov = sptk21$R101_NAMA,
                       urban = sptk21$R105,
                       sex = sptk21$B4K4,
                       age = sptk21$B4K5,
                       marital = sptk21$B4K6,
                       education = sptk21$B4K7,
                       length_stay = sptk21$LAMA_TINGG,
                       employment = sptk21$R601A,
                       income = sptk21$R603)
pandemic <- pandemic %>% 
  mutate(urban = recode(urban, `1`="Urban",`2`="Rural"),
         sex = recode(sex, `1`="Male", `2`="Female"),
         age = ifelse(age<=20, "<=20",
                      ifelse(age<=30, "21-30",
                             ifelse(age<=40, "31-40",
                                    ifelse(age<=50, "41-50",
                                           ifelse(age<=60, "51-60", ">60"))))),
         marital = recode(marital, `1`="Not married", `2`="Married",
                          `3`="Divorced", `4`="Widowed"),
         education = ifelse(education<=3, "No education and\nElementary School",
                            ifelse(education<=5, "High School", "College")),
         length_stay = ifelse(length_stay<=20, "<=20",
                              ifelse(length_stay<=40, "21-40",
                                     ifelse(length_stay<=60, "41-60",
                                            ifelse(length_stay<=80, "61-80", ">80")))),
         employment = recode(employment, `1`="Employed", `2`="Unemployed"),
         income = recode(income, `1`=">4M", `2`="2.5M-4M", `3`="1.5M-2.5M",
                         `4`="1M-1.5M", `5`="<1M"))
str(pandemic)

# prov
dat <- aggregate(happy_diff~prov, pandemic, mean) %>% 
  arrange(happy_diff) %>% 
  mutate(prov = factor(prov, levels=prov))
ggplot(dat, aes(happy_diff, prov, fill=happy_diff)) +
  geom_bar(stat="identity", width=0.7) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(limits=c(-0.8,0.1),
                     breaks = round(seq(-0.8,0.1,by=0.1),1)) +
  labs(x=element_blank(), y=element_blank(),
       title="Rata-rata selisih skor kebahagiaan berdasarkan Provinsi", 
       subtitle="Sebelum dan sesudah pandemi COVID-19 tahun 2021") +
  theme_minimal() + 
  theme(legend.position="none",
        panel.grid.minor.x=element_blank()) +
  geom_text(data=dat[1:30,], aes(happy_diff, prov, label=round(happy_diff,2)), hjust=1.1, size=3) +
  geom_text(data=dat[31:34,], aes(happy_diff, prov, label=round(happy_diff,2)), hjust=-0.1, size=3)

# sex
dat2 <- aggregate(happy_diff~sex, pandemic, mean)
ggplot(dat2, aes(sex, happy_diff)) +
  geom_bar(stat="identity", width=0.5, fill="#076fa2") +
  geom_hline(yintercept = 0) +
  scale_x_discrete(position = "top") +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x=element_blank(), y=element_blank(),
       title="Rata-rata selisih skor kebahagiaan berdasarkan Jenis Kelamin", 
       subtitle="Sebelum dan sesudah pandemi COVID-19 tahun 2021") +
  theme_minimal() +
  theme(legend.position="none",
        panel.grid.major.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x=element_text(size=15)) +
  geom_text(aes(label=round(happy_diff,2)), vjust=-1, color="white", size=6)

# urban
dat3 <- aggregate(happy_diff~urban, pandemic, mean)
ggplot(dat3, aes(urban, happy_diff)) +
  geom_bar(stat="identity", width=0.5, fill="#076fa2") +
  geom_hline(yintercept = 0) +
  scale_x_discrete(position = "top") +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x=element_blank(), y=element_blank(),
       title="Rata-rata selisih skor kebahagiaan berdasarkan Kota/Desa", 
       subtitle="Sebelum dan sesudah pandemi COVID-19 tahun 2021") +
  theme_minimal() +
  theme(legend.position="none",
        panel.grid.major.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x=element_text(size=15)) +
  geom_text(aes(label=round(happy_diff,2)), vjust=-1, color="white", size=6)

# age
dat4 <- aggregate(happy_diff~age, pandemic, mean) %>% 
  mutate(age = factor(age, levels=c("<=20","21-30","31-40","41-50","51-60",">60")))
ggplot(dat4, aes(happy_diff, age)) +
  geom_bar(stat="identity", width=0.7, fill="#076fa2") +
  geom_vline(xintercept=0) +
  labs(x=element_blank(), y=element_blank(),
       title="Rata-rata selisih skor kebahagiaan berdasarkan Usia", 
       subtitle="Sebelum dan sesudah pandemi COVID-19 tahun 2021") +
  theme_minimal() + 
  theme(legend.position="none",
        panel.grid.minor.x=element_blank(),
        axis.text=element_text(size=10),
        panel.grid.major.y = element_blank()) +
  geom_text(aes(happy_diff, age, label=round(happy_diff,2)), hjust=-0.3, color="white")

# marital
dat5 <- aggregate(happy_diff~marital, pandemic, mean) %>% 
  arrange(happy_diff) %>% 
  mutate(marital = factor(marital, levels=marital))
ggplot(dat5, aes(happy_diff, marital)) +
  geom_bar(stat="identity", width=0.7, fill="#076fa2") +
  geom_vline(xintercept=0) +
  labs(x=element_blank(), y=element_blank(),
       title="Rata-rata selisih skor kebahagiaan berdasarkan Status Perkawinan", 
       subtitle="Sebelum dan sesudah pandemi COVID-19 tahun 2021") +
  theme_minimal() + 
  theme(legend.position="none",
        panel.grid.minor.x=element_blank(),
        axis.text=element_text(size=10),
        panel.grid.major.y = element_blank()) +
  geom_text(aes(happy_diff, marital, label=round(happy_diff,2)), hjust=-0.3, color="white")

# education
dat6 <- aggregate(happy_diff~education, pandemic, mean) %>% 
  arrange(happy_diff) %>% 
  mutate(education = factor(education, levels=education))
ggplot(dat6, aes(happy_diff, education)) +
  geom_bar(stat="identity", width=0.7, fill="#076fa2") +
  geom_vline(xintercept=0) +
  labs(x=element_blank(), y=element_blank(),
       title="Rata-rata selisih skor kebahagiaan berdasarkan Pendidikan Terakhir", 
       subtitle="Sebelum dan sesudah pandemi COVID-19 tahun 2021") +
  theme_minimal() + 
  theme(legend.position="none",
        panel.grid.minor.x=element_blank(),
        axis.text=element_text(size=10),
        panel.grid.major.y = element_blank()) +
  geom_text(aes(happy_diff, education, label=round(happy_diff,2)), hjust=-0.3, color="white")

# length stay
dat7 <- aggregate(happy_diff~length_stay, pandemic, mean) %>% 
  mutate(length_stay = factor(length_stay, levels=c("<=20","21-40","41-60","61-80",">80")))
ggplot(dat7, aes(happy_diff, length_stay)) +
  geom_bar(stat="identity", width=0.7, fill="#076fa2") +
  geom_vline(xintercept=0) +
  labs(x=element_blank(), y=element_blank(),
       title="Rata-rata selisih skor kebahagiaan berdasarkan Lama Menetap", 
       subtitle="Sebelum dan sesudah pandemi COVID-19 tahun 2021") +
  theme_minimal() + 
  theme(legend.position="none",
        panel.grid.minor.x=element_blank(),
        axis.text=element_text(size=10),
        panel.grid.major.y = element_blank()) +
  geom_text(aes(happy_diff, length_stay, label=round(happy_diff,2)), hjust=-0.3, color="white")

# employment
dat8 <- aggregate(happy_diff~employment, pandemic, mean)
ggplot(dat8, aes(employment, happy_diff)) +
  geom_bar(stat="identity", width=0.5, fill="#076fa2") +
  geom_hline(yintercept = 0) +
  scale_x_discrete(position = "top") +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x=element_blank(), y=element_blank(),
       title="Rata-rata selisih skor kebahagiaan berdasarkan Status Bekerja", 
       subtitle="Sebelum dan sesudah pandemi COVID-19 tahun 2021") +
  theme_minimal() +
  theme(legend.position="none",
        panel.grid.major.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x=element_text(size=15)) +
  geom_text(aes(label=round(happy_diff,2)), vjust=-1, color="white", size=6)

# income
dat9 <- aggregate(happy_diff~income, pandemic, mean) %>% 
  mutate(income = factor(income, levels=c("<1M","1M-1.5M","1.5M-2.5M","2.5M-4M",">4M")))
ggplot(dat9, aes(happy_diff, income)) +
  geom_bar(stat="identity", width=0.7, fill="#076fa2") +
  geom_vline(xintercept=0) +
  labs(x=element_blank(), y=element_blank(),
       title="Rata-rata selisih skor kebahagiaan berdasarkan Pendapatan", 
       subtitle="Sebelum dan sesudah pandemi COVID-19 tahun 2021") +
  theme_minimal() + 
  theme(legend.position="none",
        panel.grid.minor.x=element_blank(),
        axis.text=element_text(size=10),
        panel.grid.major.y = element_blank()) +
  geom_text(aes(label=round(happy_diff,2)), hjust=-0.3, color="white")




### Logistic regression


# Regression model
pandemic <- 
  data.frame(province = sptk21$R101_NAMA,
             urban = ifelse(sptk21$R105==1, 1, 0),
             female = ifelse(sptk21$B4K4==2, 1, 0),
             age = sptk21$B4K5,
             marital = sptk21$B4K6,
             education = sptk21$B4K7,
             income = ifelse(is.na(sptk21$R603), 0, sptk21$R603),
             happy_after = sptk21$R1501,
             happy_before = sptk21$R1502,
             sat_health = sptk21$R708,
             sat_social = sptk21$R1005) %>% 
  mutate(marital = factor(recode(marital, `1`="not married yet", `2`="married", `3`="divorced", `4`="widowed"),
                          levels=c("not married yet","married","divorced","widowed")),
         happy_diff = happy_after-happy_before,
         happy_diff_cat = ifelse(happy_diff<0, 1, 0))
str(pandemic)

pandemic3 <- pandemic %>% select(!c(province, income))
happy_after <- lm(happy_after~., pandemic3 %>% select(!c("happy_before","happy_diff","happy_diff_cat")))
happy_before <- lm(happy_before~., pandemic3 %>% select(!c("happy_after","happy_diff","happy_diff_cat")))
happy_diff <- lm(happy_diff~., pandemic3 %>% select(!c("happy_after","happy_before","happy_diff_cat")))
happy_diff_cat <- glm(happy_diff_cat~., family=binomial, data=pandemic3 %>% select(!c("happy_after","happy_before","happy_diff")))
summary(happy_after)
summary(happy_before)
summary(happy_diff)
summary(happy_diff_cat)
stargazer(happy_before, happy_after, happy_diff, happy_diff_cat, type="html")

pandemic3 <- pandemic %>% select(!c(province,happy_after,happy_before,happy_diff))
m1 <- glm(happy_diff_cat~., family=binomial, data=pandemic3 %>% select(!c(sat_health)))
m2 <- glm(happy_diff_cat~., family=binomial, data=pandemic3 %>% select(!c(sat_social)))
m3 <- glm(happy_diff_cat~., family=binomial, data=pandemic3)
m4 <- glm(happy_diff_cat~.+
            urban*sat_social+urban*sat_health+
            female*sat_social+female*sat_health+
            income*sat_social+income*sat_health, family=binomial, data=pandemic3)
stargazer(m1,m2,m3,m4)
