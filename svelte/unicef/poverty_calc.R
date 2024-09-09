library("dplyr")
library(readxl)
library(ggplot2)
library(jsonlite)


# 2023 рік

data = read_excel("/Users/yevheniia/Desktop/SESHS HH 10042024 FIN_.xlsx")
  
  
dg = data %>% filter(!is.na(`А.2. З них дітей віком до 18 років?`) & `А.2. З них дітей віком до 18 років?` > 0)

income_data = dg %>% select(113) %>% 
  rename(income = `B1. Яким є загальний дохід Вашого домогосподарства в середньому на місяць, грн.?`) %>% 
  mutate(income = as.numeric(income)) %>% 
  filter(income > 0)
  

ggplot(income_data, aes(x=income))+
  geom_histogram(binwidth = 5000, fill = "blue", color = "black") +
  labs(title = "Грошовий дохід у родинах із дітьми", x = "грн", y = "Frequency")+
  theme_minimal()

bin_width = 5000

# Function to format bin labels without currency symbol
format_bin_labels <- function(breaks) {
  labels <- paste0(breaks[-length(breaks)], " - ", breaks[-1] - 1)
  return(labels)
}

# Create bins and format labels
breaks <- seq(0, max(income_data$income) + bin_width, by = bin_width)

binned_data <- income_data %>%
  mutate(income_bin = cut(income, 
                          breaks = breaks,
                          right = FALSE, 
                          include.lowest = TRUE,
                          labels = format_bin_labels(breaks))) %>%
  group_by(income_bin) %>%
  summarize(count = n()) %>%
  ungroup()

# View the resulting data frame
write.csv(binned_data, "/Users/yevheniia/Desktop/monthly_income_families_with_children.csv", row.names = F)
json_data <- toJSON(binned_data, pretty = TRUE)
print(json_data)
write(json_data, file = "/Users/yevheniia/Desktop/binned_data.json")

# чи вситачає коштів на мʼясо/рибу
meat = dg %>% 
  select(`А.2. З них дітей віком до 18 років?`, 200) %>%
  rename(value = `Г1.8. Чи вистачало Вам коштів на споживання м’яса та риби хоча б двічі на тиждень?`,
         children = `А.2. З них дітей віком до 18 років?`) %>% 
  filter(value != "G1_8") %>% 
  filter(children == 3) %>% 
  group_by(value) %>% 
  summarize(freq = n()) %>% 
  mutate(percentage = (freq / sum(freq)) * 100)

# забезпечення за кількістю дітей
meat = dg %>% 
  select(`А.2. З них дітей віком до 18 років?`, 200) %>%
  rename(value = `Г1.8. Чи вистачало Вам коштів на споживання м’яса та риби хоча б двічі на тиждень?`,
         children = `А.2. З них дітей віком до 18 років?`) %>% 
  filter(value != "G1_8") %>% 
  filter(children == 3) %>% 
  group_by(value) %>% 
  summarize(freq = n()) %>% 
  mutate(percentage = (freq / sum(freq)) * 100)


new_clothes = dg %>% 
  select(`А.2. З них дітей віком до 18 років?`, `Е3. Чи має кожна дитина, яка проживає у Вашому домогосподарстві, наступні можливості:`) %>% 
  rename(value = `Е3. Чи має кожна дитина, яка проживає у Вашому домогосподарстві, наступні можливості:`,
         children = `А.2. З них дітей віком до 18 років?`) %>% 
  filter(value != "E3_1") %>% 
  filter(children == 3) %>% 
  group_by(value) %>% 
  summarize(freq = n()) %>% 
  mutate(percentage = (freq / sum(freq)) * 100)


# 2021 рік

data = read_excel("/Users/yevheniia/Desktop/UNICEF/mic_doch_i_umovy_2021/Households_microdani_anonimni_2021.xls") %>% 
  filter(type_dom == 1)


c_fruit = data %>% 
  filter(c_fruit == 1 ) %>% 
  select()


table(data$rooms)
table(data$tp_ns_p)
table(data$gnd)
table(data$cashinc)
table(data$bath)
table(data$ozn_depr_13)

ggplot(data, aes(x=cashinc/12))+
  geom_histogram(binwidth = 1000, fill = "blue", color = "black") +
  labs(title = "Грошовий дохід у родинах із дітьми", x = "грн", y = "Frequency")+
  theme_minimal()

median(data$cashinc/12)



data = read_excel("/Users/yevheniia/Desktop/dataset_2024-07-23T18_31_34.055436265Z_DEFAULT_INTEGRATION_SSSU_DF_SURVEY_LIVING_CONDITIONS_HOUSEHOLDS_P1_1.0.0.xlsx")  
type = data.frame(table(data$`Обсяг доходів`))  
