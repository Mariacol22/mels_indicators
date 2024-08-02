df$Group1_percentage <- df$Group1 / df$total##### Paquetes necesarios #####

install.packages("openxlsx") # Para importar desde excel
install.packages("tidyverse") # Manipulación de datos y ggplot2
install.packages("magrittr") # pipe
install.packages('modeest') # Obtener moda
install.packages('fdth') # Calcular tabla frecuencias
install.packages("prettyR") # Opcion de Descriptivas
install.packages("data.table")
install.packages("rsurveycto")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("xlsx")
install.packages('epiDisplay')
install.packages('arsenal')
install.packages('hrbrthemes')

##### Abrir Librerias #####

library(readr)
library(openxlsx)
library(data.table)
library(rsurveycto)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(xlsx)
library(scales)
library(epiDisplay)

##### SURVEYCTO ###### 
# Put the url of your project to link

get_session_auth = function(servername, username, password) {
  index_url = glue('')
  index_res = GET(index_url)
  csrf_token = headers(index_res)$`x-csrf-token`
  
  if (is.null(csrf_token)) {
    scto_abort(paste(
      'Unable to access server {.server `heifer`}.',
      'Please check that server is running.'))
  }
  
  login_url = glue(
    'https://heifer.surveycto.com/login?spring-security-redirect=%2F')
  login_res = POST(
    login_url,
    body = list(
      username = jose.beltran@heifer.org,
      password = Kisame95,
      csrf_token = csrf_token),
    encode = 'form')
  
  scto_cookies = cookies(login_res)
  session_id = scto_cookies$value[scto_cookies$name == 'JSESSIONID']
  
  return(list(csrf_token = csrf_token, session_id = session_id))
}

#### DB ######
# Put the path of your database
library(readxl)
EC1281 <- read_excel("path_of_your_database.xlsx")
View(EC1281)
#### Limpieza de BD####

EC1281<-  dplyr::select(EC1281, -c(SubmissionDate, caseid, F.I_Name_2, 	Country, 	Exchange_Rate, 	Interview_starttime, 	Data_Collection_Firm,respo_not_available, 	respo_available_other, 	Introduction, interview_decline, 	interview_decline_other, postoned_date, 	Informed_Consent, 	ID, OGorPOG, 	MuncipalityName, 	CommunityName, PhoneNumber, 	CompleteAddress, mthsinadqfood, Q1A,Net_Return_Confirmation, Revenue_Costs, 	BTM6_Final_LocalCurrency, 	BTM6_Final_USD, Other_results, 	instanceID, formdef_version, NumberofMinutes))
EC1281<-EC1281 %>% dplyr::select(-starts_with("HDDS_Fooddplyr::selection"))
EC1281<-EC1281 %>% dplyr::select(-starts_with("Foods_Taboo"))
EC1281<-EC1281 %>% dplyr::select(-starts_with("Food_Taboo"))
EC1281<-EC1281 %>% dplyr::select(-starts_with("whichmonthsofinadqfood_"))
EC1281<- EC1281[,!grepl("*Water", names(EC1281))]
EC1281<-EC1281 %>% dplyr::select(-starts_with("Governance_dplyr::selection"))
EC1281<-EC1281 %>% dplyr::select(-starts_with("Sanitation_Practice_dplyr::selection"))
EC1281<-EC1281 %>% dplyr::select(-starts_with("CSA_Practice_dplyr::selection_"))
EC1281<-EC1281[,!grepl("*WomenJointDecisions", names(EC1281))]
EC1281<-EC1281 %>% dplyr::select(-starts_with("GI_13_Count"))
EC1281<-EC1281 %>% dplyr::select(-starts_with("GI-13_Count"))
EC1281<-EC1281 %>% dplyr::select(-starts_with("Waste_Management"))
EC1281<-EC1281 %>% dplyr::select(-starts_with("Waste_Management_dplyr::selection"))
EC1281<-EC1281 %>% dplyr::select(-starts_with("GI_07_"))
EC1281<-EC1281[,!grepl("*LandRestoration", names(EC1281))]
EC1281<-EC1281[,!grepl("*WomenJointDecisions", names(EC1281))]
column_names <- names(EC1281)
print(column_names)
column_names<- data.frame(column_names)
write.xlsx(column_names,"columnstokeep.xlsx")
view(column_names)
#Cambiar los nombres de las variables con guion alto
colnames(EC1281) <- gsub("-", "_", colnames(EC1281))

#Filtrar por solo bionegocios
#EC1281<-EC1281 %>% filter(Project_Name=="4") 


# Check numeric
numeric_cols <- sapply(EC1281 , is.numeric)
numeric_df <-EC1281  %>%  dplyr::select_if(is.numeric)
column_namesnumeric<- names(EC1281)
print(column_namesnumeric)
EC1281 <- EC1281 %>% mutate(Totmembers=Household_Members + 1)

EC1281 <- transform(EC1281, percapitaincome_d= ifelse(GI_01_Final_LocalCurrency > 0 & Totmembers> 0,GI_01_Final_LocalCurrency/(Totmembers*12*30.5), NA))
#Percapita income for sources
EC1281 <- EC1281 %>%
  mutate(across(starts_with("If_No_"), ~ ifelse(. > 0 & Totmembers> 0, ./ Totmembers, .), .names = "percap_diar_{col}"))
#Check NAs and replace
EC1281[, numeric_cols][is.na(EC1281[,numeric_cols])] <- 0
#Modify CSA_Practices_Promoted 
EC1281$CSA_Practices_Promoted[EC1281$CSA_Practices_Promoted == 0] <- 3
EC1281$CSA_Practices_Promoted[is.na(EC1281$CSA_Practices_Promoted)] <- 0
EC1281$GI_11_Final<- EC1281$GI_11_Count /EC1281$CSA_Practices_Promoted
####INTRODUCTION####
library(arsenal) 
descriptive_table1 <- tableby(~HouseholdHead + Household_Members + PartipatingMember + Household_Youth + If_No_Crops+ If_No_Livestock+ If_No_OnFarm +If_No_NonFarm+ If_No_Wage+ If_No_Salaries+ If_No_Dividends+If_No_Transfers +GI_01_Final_LocalCurrency, data =EC1281 ) 
descriptive_table1 <-summary(descriptive_table1)
write.table(descriptive_table1, file = "DescriptiveTable.txt", sep = ",", quote = FALSE, row.names = F)
#Para provincia y comunidad
tab1(EC1281$ProvinceName, sort.group = "decreasing", cum.percent = TRUE)
frequency_table1 <-tab1(EC1281$ProvinceName, sort.group = "decreasing", cum.percent = TRUE)
write.table(frequency_table1, file = "FrequencyTable.txt", sep = ",", quote = FALSE, row.names = F)
####INDICATORS####
#1. Income by projects
Incomeproject<-EC1281 %>% group_by(Project_Name_2) %>% summarise(Mean_income=mean(GI_01_Final_LocalCurrency,na.rm = TRUE), Min_income=min(GI_01_Final_LocalCurrency,na.rm = TRUE),Max_income=max(GI_01_Final_LocalCurrency,na.rm = TRUE), sd_income=sd(GI_01_Final_LocalCurrency,na.rm = TRUE))
print(Incomeproject)
#Percapita income
Incomeproject1<-EC1281 %>% group_by(Project_Name_2) %>% summarise(Mean_income=mean( percapitaincome_d,na.rm = TRUE), Min_income=min(percapitaincome_d,na.rm = TRUE),Max_income=max( percapitaincome_d,na.rm = TRUE), sd_income=sd( percapitaincome_d,na.rm = TRUE))
Incomeproject1<-EC1281 %>% summarise(Mean_income=mean( percapitaincome_d,na.rm = TRUE), Min_income=min(percapitaincome_d,na.rm = TRUE),Max_income=max( percapitaincome_d,na.rm = TRUE), sd_income=sd( percapitaincome_d,na.rm = TRUE))
print(Incomeproject1)
#1.1. Mean of incomes and expenses
sum_income_sources<-EC1281 %>% summarize(across(starts_with("If_No_"), sum, na.rm = TRUE))

for (i in seq_along(sum_income_sources)) {
  col_name <- names(sum_income_sources)[i]
  sum_column1 <- sum(EC1281$GI_01_Final_LocalCurrency, na.rm = TRUE)
  new_col_name <- paste0("percent_", col_name)
  EC1281[, new_col_name] <- sum_income_sources[i] /sum_column1 
}

Income_expenses<-EC1281 %>%summarize(across(starts_with("If_No_"), mean, na.rm = TRUE))
install.packages("ggrepel")
library(ggrepel)
df_Income_sources <- data.frame(t(sum_income_sources))
colnames(df_Income_sources)[1] <- "Income_source"
df_Income_sources$Source <- rownames(df_Income_sources)
df_Income_sources$Source <- factor(df_Income_sources$Source)
df_Income_sources$prop <- df_Income_sources$Income_source / sum(df_Income_sources$Income_source)
df_Income_sources$ypos <- cumsum(df_Income_sources$prop) - 0.5 * df_Income_sources$prop
df_Income_sources <- df_Income_sources %>%
  mutate(label_pos = cumsum(prop) - 0.5 * prop)
num_top_sources <- 3
top_sources_df <- df_Income_sources %>%
  arrange(desc(prop)) %>%
  slice(1:num_top_sources)

pie_chart1 <- ggplot(df_Income_sources, aes(x = "", y = prop, fill = Source)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text_repel(data = top_sources_df, aes(label = percent(prop)), size = 4, 
                  box.padding = 0.8, point.padding = 0.5, segment.color = "grey50") +
  scale_fill_brewer(palette = "Dark2") +  # You can choose any other palette from RColorBrewer
  labs(title = "Family Income by Source", size = 10)

pie_chart1 +
  geom_text_repel(data = top_sources_df %>% filter(Source == "If_No_Crops"),
                  aes(label = percent(prop)), size = 4, nudge_x = 0.4, nudge_y = 0.1)


print(pie_chart1)

print(pie_chart1)
#Percapita income
Income_expensespercap<-EC1281 %>% group_by(Project_Name_2) %>%summarize(across(starts_with("percap_If"), mean, na.rm = TRUE))
Income_expenses1<-EC1281 %>%summarize(across(starts_with("percap_If"), mean, na.rm = TRUE))

view(Income_expensespercap)

#3.	Modelo de graduación (HHTM) para indicador
#Indicator 1
EC1281$hhtm_01 <- with(EC1281, ifelse( percapitaincome_d< 2.63,"A", ifelse(percapitaincome_d>=5.26,"C", "B")))
hhtm_01 <- dplyr::select(EC1281, c(hhtm_01))                                 
print(hhtm_01) 

tabla1 <- table(hhtm_01$hhtm_01)
tabla_relativa <- prop.table(tabla1)
tabla_relativa <- data.frame(t(tabla_relativa))
#Indicator 4
EC1281$hhtm_04 <- with(EC1281, ifelse( GI_04_Final < 3,"A", ifelse(GI_04_Final>=5,"C", "B")))
hhtm_04 <- dplyr::select(EC1281, c(hhtm_04))                                 
print(hhtm_04) 

tabla2 <- table(hhtm_04$hhtm_04)
tabla_relativa2 <- prop.table(tabla2)
tabla_relativa2 <- data.frame(t(tabla_relativa2))
print(tabla_relativa2)
#Indicator 11
EC1281$hhtm_11 <- with(EC1281, ifelse( GI_11_Final < 0.4,"A", ifelse(GI_11_Final>=0.7,"C", "B")))
hhtm_11 <- dplyr::select(EC1281, c(hhtm_11))                                 
print(hhtm_11) 

tabla3 <- table(hhtm_11$hhtm_11)
tabla_relativa3 <- prop.table(tabla2)
tabla_relativa2 <- data.frame(t(tabla_relativa2))
print(tabla_relativa2)
#Combinar indicadores 

indicatorhhtm<-cbind.data.frame(hhtm_01,hhtm_04,hhtm_11)


                                
                                
indicatorhhtm<-indicatorhhtm  %>%
  mutate(across(starts_with("hhtm"), 
                list(A_Count = ~sum(. == 'A'),
                     B_Count = ~sum(. == 'B'),
                     C_Count = ~sum(. == 'C')), .names = "{.col}_{.fn}"))


indicatorhhtm_final<-data.frame(Indicador= c("hhtm_01","hhtm_04", "hhtm_11"), Grade = rep(c("A", "B", "C"), times = 6))
indicatorhhtm_final <- indicatorhhtm_final[order(indicatorhhtm_final$Grade), ]

indicatorhhtm_final <- indicatorhhtm_final %>% arrange(A)
#Grouped charts
library(viridis)
library(hrbrthemes)
ggplot(indicatorhhtm, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Studying 4 species..") +
  theme_ipsum() +
  xlab("")





indicatorhhtm<-data.frame(t(indicatorhhtm))
indicatorhhtm<-indicatorhhtm %>% dplyr::select(-starts_with("Freq"))