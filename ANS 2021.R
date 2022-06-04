#########################################################################################
#                                                                                       #
#  Script utilizado para elaboração do TCC em Data Science e Analytics MBA USP/ESALQ    #
#                                                                                       #
# #######################################################################################

#########################################################################################
#                                                                                       #
#  Título: A contribuição da consulta pública na incorporação de novos medicamentos no  #
#  Rol de Procedimentos e Eventos em Saúde da ANS                                       #
#                                                                                       #
# #######################################################################################


#########################################################################################
#                                                                                       #
#  PAsso 1: Instalação e Carregamento dos Pacotes utlizados no trabalho.                #
#                                                                                       #
# #######################################################################################


pacotes <- c("tidyverse","readxl", "kableExtra", "janitor", "ggplot2", "jtools", "stringr",
             "caret", "ROCR", "pROC", "plotly", "lmtest")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}
  
# Importando e ajustando os arquivos em formato MS Excel.
# Os nomes das colunas foram renomeados com os mesmos nomes quando os dados das colunas eram iguais.


Tabela01_NTRP <- read_excel("Tabela1. NTRP.xlsx") 
  colnames(Tabela01_NTRP)[1:2] <- c("UAT", "Recomendacao")

Tabela02_CP_81_2b <- read_excel("Tabela2. CP 81 2b.xlsx")
  colnames(Tabela02_CP_81_2b)[1:6] <- c("UAT", "Tipo", "Concordo", "Parcial", "Discordo", "Total")

Tabela03_Rol_ANS_2021 <- read_excel("Tabela3. Rol ANS 2021.xlsx")
  colnames(Tabela03_Rol_ANS_2021)[1:2] <- c("UAT", "Rol")
  
Tabela04_Quali_Contribuinte <- read_excel("Tabela4. Tipo_Contribuintes.xlsx")  
  colnames(Tabela04_Quali_Contribuinte)[1:1] <- c("Tipo")
  
Tabela05_CP_raw <-  read_excel("Tabela5. Contribuicoes_raw.xlsx")  
  
  
# Sumarizando o arquivo Tabela 3 para obter o total de contribuicoes por PAR, e somando os tipos 
# de contribuintes ignorando os valores Nulos.

Tabela06_CP_total <- Tabela02_CP_81_2b %>% 
  group_by (UAT) %>%
  summarise (Concordo = sum(Concordo,na.rm = TRUE), Discordo = sum(Discordo,na.rm = TRUE), 
           Parcial = sum(Parcial,na.rm = TRUE), Total = sum(Total,na.rm = TRUE))


# Unindo as 3 bases de dados reunindo os atributos qualitativos e quantitativos

Tabela07_CP_Completa <- left_join(Tabela01_NTRP,Tabela03_Rol_ANS_2021, by = "UAT")%>%
  left_join (., Tabela06_CP_total, by = "UAT")


#########################################################################################
#                                                                                       #
#  Passo 2: Análise Exploratória dos dados                                              #
#                                                                                       #
# #######################################################################################

# Tabela para visualização completa dos dados utilizados no trabalho

Tabela07_CP_Completa %>%
  arrange(desc(Total)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 19)

# Tabela com a contagem das PAR conforme recomenção preliminar da ANS (FAV / DES / N/A)

Tabela07_CP_Completa %>% count(Recomendacao, sort = TRUE) %>%
  mutate(Percentual = (round(n/sum(n)*100, digits = 2))) %>%
  adorn_totals("row") %>%  
    kable() %>%
    kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 19)

# Gráfico com a contagem das PAR conforme recomenção preliminar da ANS

Tabela07_CP_Completa %>% count(Recomendacao, sort = TRUE) %>%
  ggplot(aes(x = reorder(Recomendacao, +n), y = n)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = 0, hjust = -0.2) +
  coord_flip() +
  scale_color_viridis_d()+
  theme_bw() +
    xlab("")

# Tabela com a contagem das PAR incorporaradas após etapa de consulta pública

Tabela07_CP_Completa %>% count(Rol, sort = TRUE) %>%
  mutate(Percentual = (round(n/sum(n)*100, digits = 2))) %>%
  adorn_totals("row") %>%  
    kable() %>%
    kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 19)

# Gráfico com a contagem das PAR incorporaradas após etapa de consulta pública

Tabela07_CP_Completa %>% count(Rol, sort = TRUE) %>%
  ggplot(aes(x = reorder(Rol, +n), y = n)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = 0, hjust = -0.2) +
  coord_flip() +
  scale_color_viridis_d()+
  theme_bw() +
  xlab("")
               

# Tabela com a contagem De incorporacoes De acordo com a recomendacao preliminar

Tabela07_CP_Completa %>% 
  count(Recomendacao , Rol, sort = TRUE) %>%
  mutate(Percentual = (round(n/sum(n)*100, digits = 2))) %>%
  adorn_totals("row") %>% 
    kable() %>%
    kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 19)

# Gráfico com a contagem De incorporacoes De acordo com a recomendacao preliminar

Tabela07_CP_Completa %>% count(Recomendacao, Rol, sort = TRUE) %>%
  ggplot(aes(Recomendacao, n)) +
  geom_bar(aes(fill = Rol), stat = "identity", position = "dodge") +
  scale_color_viridis_d() +
  geom_text(aes(label = n, group = Rol), vjust = -0.4, position = position_dodge(width = 0.9)) +
  theme_bw() +
  xlab("")


# Tabela com a contagem da quantidade de contribuições totais vs. recomendação preliminar da ANS

Tabela07_CP_Completa %>% 
group_by(Recomendacao)%>%
summarise (Concordo = sum(Concordo), Discordo = sum(Discordo), Parcial = sum(Parcial), Total = sum(Total)) %>%
  adorn_totals("row") %>% 
  kable() %>%
   kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 19)

# Gráfico com  a tabela com a contagem da quantidade de contribuições totais vs. recomendação preliminar 
# da ANS

  gather(Tabela07_CP_Completa %>% 
  group_by(Recomendacao)%>%
  summarise (Concordo = sum(Concordo), Discordo = sum(Discordo), Parcial = sum(Parcial), 
  Total = sum(Total)), key="Opiniao", value="n", -Recomendacao, -Total) %>%
  ggplot(aes(Recomendacao, n,)) +
  geom_bar(aes(fill = Opiniao), stat = "identity", position = "dodge") +
  geom_text(aes(label = n, group = Opiniao), vjust = -0.4, position = position_dodge(width = 0.9)) +
  theme_bw() +
  xlab("")
  
# Unindo a tabela contendo a recomendação preliminar da ANS com a tabela de qtde de contribuições por 
# tipo de contribuinte
  
  
Tabela08_CP_Des_Tipo <- left_join(Tabela01_NTRP,Tabela02_CP_81_2b, by = "UAT")%>%
  left_join (., Tabela03_Rol_ANS_2021, by = "UAT")

# Contagem da quantidade de contribuições "Discordo" por tipo de contribuinte das PAR com recomendação 
# preliminar desfavorável

Tabela08_CP_Des_Tipo %>% 
  filter(.,Recomendacao == "DESFAVORAVEL") %>%
  group_by(Tipo)%>% 
  summarise (Discordo = sum(Discordo, na.rm = TRUE))%>% 
  arrange(desc(Discordo))%>% 
  adorn_totals("row") %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 19)

# Número total de Contribuições "Discordo" conforme incorporação da PAR no Rol da ANS após consulta pública


Tabela08_CP_Des_Tipo %>% 
  filter(Recomendacao == "DESFAVORAVEL") %>%
  group_by(Rol)%>% 
  summarise (Discordo = sum(Discordo, na.rm = TRUE))  %>%
  adorn_totals("row") %>%  
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 19)

# Gráfico com as contribuições absolutas "Discordo" conforme incorporação da PAR no Rol da ANS após consulta pública

Tabela08_CP_Des_Tipo %>% 
  filter(Recomendacao == "DESFAVORAVEL") %>%
  group_by(Rol, Tipo)%>% 
  summarise (Discordo = sum(Discordo, na.rm = TRUE))%>%
  ggplot(aes(x = reorder(Tipo, +Discordo), Discordo)) +
  geom_bar(stat = "identity", fill = "gray")+
  geom_text(aes(label = Discordo), vjust = 0.5, hjust = 0.4, fontface = "bold") +
  ##ggtitle("Percentual de Contribuições por tipo de contribuinte")+
  facet_wrap(~Rol) +
  coord_flip()+
  theme_bw() +
  theme(axis.text.y = element_text(size = 12, face = "plain")) +
  xlab("")


# Gráfico com as contribuições absolutas "Discordo" conforme incorporação da PAR no Rol da ANS após consulta pública

Tabela08_CP_Des_Tipo %>% 
  filter(Recomendacao == "DESFAVORAVEL") %>%
  group_by(Rol, Tipo)%>% 
  summarise (Discordo = sum(Discordo, na.rm = TRUE))%>%
  mutate(Freq = round(Discordo/sum(Discordo)*100, digits = 1))%>%
  ggplot(aes(x = reorder(Tipo, +Freq), Freq)) +
  geom_bar(stat = "identity", fill = "gray")+
  geom_text(aes(label = Freq), vjust = 0.5, hjust = 0.7, fontface = "bold", size = 5) +
  facet_wrap(~Rol) +
  coord_flip() +
  theme_bw() +
  theme(axis.text.y = element_text(size = 15, face = "plain"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.border = element_blank(),
        panel.background = element_blank())+
  labs(y= "Percentual relativo de contribuições por tipo de contribuinte", size = 15)+
  xlab("")


# Agrupando a quantidade de contribuições "Discordo" das PAR com recomendacao preliminar desafavoravel

Tabela09_CP_Discordo_Rol <- Tabela08_CP_Des_Tipo %>% 
  filter(Recomendacao == "DESFAVORAVEL") %>%
  group_by(Rol, Tipo)%>% 
  summarise (Discordo = sum(Discordo, na.rm = TRUE))


# Pivotando em colunas a quantidade de contribuições, conforme incorporação no Rol após consulta pública

Tabela10_CP_Discordo_Rol <- Tabela09_CP_Discordo_Rol %>% 
  pivot_wider(names_from = "Rol", values_from = "Discordo") 

# Transformando em 0 os campos com N/A

Tabela10_CP_Discordo_Rol[is.na(Tabela10_CP_Discordo_Rol)] = 0


# Adicionando a qualidade da contribuição na tabela de quantidade de contribuições

Tabela11_CP_Tipo_Quali <- left_join(Tabela04_Quali_Contribuinte,Tabela10_CP_Discordo_Rol, by = "Tipo")


#########################################################################################
#                                                                                       #
#  Passo 3: Analisado a associação entre n° de conntriuições e incorporação no Rol      #
#                                                                                       #
# #######################################################################################

# Montando a tabela de frequencia das contribuições "Discordo" conforme "Qualidade dos Grupos"

Tabela12_CP_tb_Freq <- Tabela11_CP_Tipo_Quali %>% 
                              group_by(Qualidade) %>% 
                              summarise(NAO = sum(NAO), SIM = sum(SIM))

# Exibindo a tabela de maneira elegante

Tabela12_CP_tb_Freq %>% 
  adorn_totals("row") %>%  
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                       full_width = F,
                       font_size = 19)

# Reproduzindo manualamente a tabela de frequências para rodar o teste Qui-quadrado


Tabela13_CP_tb_Freq <- data.frame (nao=c(1341, 513), sim=c(1346, 755))

# Realizando o teste Qui-quadrado conforme qualidade das contribuições

chisq.test(Tabela13_CP_tb_Freq)

# Realizando o teste Qui-quadrado conforme o número de contribuições absolutas


# Reproduzindo manualamente a tabela de frequências por grupo de PAR (incorporadas e não incorporadas) para rodar 
# teste Qui-quadrado

Tabela14_CP_tb_Freq_abs <- data.frame (Qtde_PAR=c(7, 19), Contr=c(2101, 1854))

# Realizando o teste Qui-quadrado conforme o número de contribuições absolutas

chisq.test(Tabela14_CP_tb_Freq_abs)

#########################################################################################
#                                                                                       #
#  Passo 4: ESTIMANDO O MODELO LOGÍSTICO BINÁRIO                                        #
#                                                                                       #
#########################################################################################

# Ajustando a varariável dependente (Incorpoiração no Rol) para binária

Tabela15_CP_Rol <-Tabela07_CP_Completa %>%
  filter(Recomendacao == "DESFAVORAVEL") %>%
  mutate(Rol_bin = if_else(Rol == "SIM",1,0))

# Tabela de frequências absolutas da variável 'Rol'

table(Tabela15_CP_Rol$Rol_bin) 

# Estatística descritiva da tabela que será utilizada para o desenvolvimento do modelo
summary (Tabela15_CP_Rol)

# Modelo 1
# Estimando o modelo com as quantidades total de contribuições (Concordo, Discordo e Parcial) 
# considerando apenas as PAR com recomendação preininar desfavorável

modelo_rol_completo <- glm(formula = Rol_bin ~ Concordo + Discordo + Parcial,
                      data = Tabela15_CP_Rol, 
                      family = "binomial")

logLik(modelo_rol_completo)
summary(modelo_rol_completo)
summ(modelo_rol_completo, confint = T, digits = 3, ci.width = .95)
export_summs(modelo_rol_completo, scale = F, digits = 6) 


# Realizando o procedimento stepwise para feature selection

step_modelo_rol_completo <- step(object = modelo_rol_completo,
                              k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))

logLik(step_modelo_rol_completo)
summary(step_modelo_rol_completo)
summ(step_modelo_rol_completo, confint = T, digits = 3, ci.width = .95)
export_summs(step_modelo_rol_completo, scale = F, digits = 6) 


# Modelo 2
# Estimando o modelo com as quantidades de contribuições conforme qualidade (Técnico e Não Técnico) 
# definada de forma arbritrária # (experiência pesquisador) considerando apenas PARS com recomendação 
# desfavorável e contrbuições em discordancia

# Preparando a tabela agrupando as contribuicoesconforme qualidade "Tecnicas" e "Nao Tecnicas"

Tabela16_CP_Rol_Quali <- left_join(Tabela04_Quali_Contribuinte,Tabela08_CP_Des_Tipo, by = "Tipo") %>% 
  filter(Recomendacao == "DESFAVORAVEL") %>% 
  group_by(Qualidade, UAT, Rol) %>% 
  summarise (Concordo = sum(Concordo, na.rm = TRUE), Discordo = sum(Discordo, na.rm = TRUE), 
            Parcial = sum(Parcial, na.rm = TRUE), Total = sum(Total, na.rm = TRUE) )

# Selecionando apenas oas colunas necessárias

Tabela16_CP_Rol_Quali <- Tabela16_CP_Rol_Quali %>% select(Qualidade, UAT, Rol, Discordo)

# Pivotando as features "Tecnicas" e "Nao Tecnicas"

Tabela17_CP_Rol_Quali <- Tabela16_CP_Rol_Quali %>% 
  pivot_wider(names_from = "Qualidade", values_from = "Discordo")  


# Incluindo a coluna da váriavel dependente "Rol" para valor binário

Tabela17_CP_Rol_Quali <- Tabela17_CP_Rol_Quali %>% mutate(Rol_bin = if_else(Rol == "SIM",1,0))

# Estimando o modelo conforme contribuições dos grupos Técnico e Não Técnicos

modelo_rol_quali <- glm(formula = Rol_bin ~ Tecnico + Nao_Tecnico,
                  data = Tabela17_CP_Rol_Quali, 
                  family = "binomial")

logLik(modelo_rol_quali)
summary(modelo_rol_quali) 
summ(modelo_rol_quali, confint = T, digits = 3, ci.width = .95)
export_summs(modelo_rol_quali, scale = F, digits = 6) 

# Realizando o procedimento stepwise para feature selection

step_modelo_rol_quali <- step(object = modelo_rol_quali,
                              k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))

logLik(step_modelo_rol_quali)
summary(step_modelo_rol_quali) 
summ(step_modelo_rol_quali, confint = T, digits = 3, ci.width = .95)
export_summs(step_modelo_rol_quali, scale = F, digits = 6) 


# Realizando o teste estatístico de comparação entre o modelo e o modelo pós procedimento stepwise

lrtest(modelo_rol_quali, step_modelo_rol_quali)


# Comparando os modelos na mesma visualização

export_summs(modelo_rol_quali, step_modelo_rol_quali, scale = F)


# Modelo 3
# Estimando o modelo com as quantidades de contribuições conforme tipo de contribuinte original definidos
# como "TECNICOS" arbitrariamente pelo pesquisador, considerando apenas PARs com recomendação desfavorável e 
# contribuições em discordancia

# Importando o arquivo em MS Excel com os nomes dos tipos de contribuintes abreviados

Tabela18_Tipo_Red <-  read_excel("Tabela18. Tipo_Reduzido.xlsx")  

# Incluindo os nomes dos tipos de contribuintes abreviados na tabela preparada para estimar o modelo

Tabela19_CP_Rol_Tipo_Ori <- left_join(Tabela18_Tipo_Red,Tabela08_CP_Des_Tipo, by = "Tipo") %>% 
  filter(Recomendacao == "DESFAVORAVEL")

# Pivotando as contribuições por tipo de contribuinte em colunas

Tabela20_CP_Rol_Tipo_Ori <- Tabela19_CP_Rol_Tipo_Ori %>% 
                            pivot_wider(names_from = "Tipo_Red", values_from = "Discordo")  

# Removendo as colunas desnecessárias para prepração da tabela para estimação do modelo
 
Tabela21_CP_Rol_Tipo_Ori <- Tabela20_CP_Rol_Tipo_Ori %>% 
   group_by(UAT, Rol) %>% 
   summarise (Con_Pro = sum(Con_Pro,na.rm = TRUE), 
             Emp_Det = sum(Emp_Det,na.rm = TRUE), 
             Emp_Ind = sum(Emp_Ind,na.rm = TRUE), 
             Ent_Ope = sum(Ent_Ope,na.rm = TRUE), 
             Fam_Ami = sum(Fam_Ami,na.rm = TRUE), 
             Gru_Ass = sum(Gru_Ass,na.rm = TRUE), 
             Ins_Sau = sum(Ins_Sau,na.rm = TRUE), 
             Int_Tem = sum(Int_Tem,na.rm = TRUE), 
             Ope_Sau = sum(Ope_Sau,na.rm = TRUE), 
             Outro = sum(Outro,na.rm = TRUE), 
             Paciente = sum(Paciente,na.rm = TRUE), 
             Prestador = sum(Prestador,na.rm = TRUE), 
             Pro_Sau = sum(Pro_Sau,na.rm = TRUE), 
             Soc_Med = sum(Soc_Med,na.rm = TRUE), 
             Ent_Pre = sum(Ent_Pre,na.rm = TRUE), 
             Ins_Aca = sum(Ins_Aca,na.rm = TRUE), 
             Consultoria = sum(Consultoria,na.rm = TRUE), 
             Org_Gov = sum(Org_Gov,na.rm = TRUE))


# Removendo os campas com N/A

Tabela21_CP_Rol_Tipo_Ori[is.na(Tabela20_CP_Rol_Tipo_Ori)]=0

# Transformando a variável dependente em binária

Tabela21_CP_Rol_Tipo_Ori <- Tabela21_CP_Rol_Tipo_Ori %>% mutate(Rol_bin = if_else(Rol == "SIM",1,0))

# Estimação do modelo conforme contribuições dos tipos de contribuintes originais

modelo_rol_tipo_ori <- glm(formula = Rol_bin ~ Pro_Sau + Con_Pro + Soc_Med + Ins_Aca + Ins_Sau,
                        data = Tabela21_CP_Rol_Tipo_Ori, 
                        family = "binomial")


logLik(modelo_rol_tipo_ori)
summary(modelo_rol_tipo_ori)
summ(modelo_rol_tipo_ori, confint = T, digits = 3, ci.width = .95)
export_summs(modelo_rol_tipo_ori, scale = F, digits = 6) 


# Realizando o procedimento stepwise para feature selection

modelo_rol_tipo_ori_step <- step(object = modelo_rol_tipo_ori,
                              k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))

logLik(modelo_rol_tipo_ori_step)
summary(modelo_rol_tipo_ori_step)
summ(modelo_rol_tipo_ori_step, confint = T, digits = 3, ci.width = .95)
export_summs(modelo_rol_tipo_ori_step, scale = F, digits = 6) 

# Realizando o teste estatístico de comparação entre o modelo e o modelo pós procedimento stepwise

lrtest(modelo_rol_tipo_ori, modelo_rol_tipo_ori_step)

# Comparando os modelos na mesma visualização

export_summs(modelo_rol_tipo_ori, modelo_rol_tipo_ori_step, scale = F)


# Definição do modelo final 

# Realizando o teste estatístico de comparação dos modelos "Profissionais de Saúde" x modelo "Tecnico"

lrtest(step_modelo_rol_quali, modelo_rol_tipo_ori_step)

# Comparando os modelos na mesma visualização

export_summs(step_modelo_rol_quali,modelo_rol_tipo_ori_step , scale = F)

# Fazendo predições conforme o número de contribuições desfavoráveis do modelo final profissionais de saúde

predict(object = modelo_rol_tipo_ori_step, 
        data.frame(Pro_Sau = 200), 
        type = "response")



#########################################################################################
#                                                                                       #
#  Passo 5: Analisando a eficiência do modelo final                                     #
#                                                                                       #
#########################################################################################

# Construindo a matriz de confusão baseada no modelo de Contribuições Discordantes de Profissionais de Saúde

Tabela21_CP_Rol_Tipo_Ori$phat <- modelo_rol_tipo_ori_step$fitted.values

confusionMatrix(table(predict(modelo_rol_tipo_ori_step, type = "response") >= 0.5,
                      Tabela21_CP_Rol_Tipo_Ori$Rol_bin == 1)[2:1, 2:1])

# Gerando a curva ROC

ROC <- roc(response = Tabela21_CP_Rol_Tipo_Ori$Rol_bin, 
           predictor = modelo_rol_tipo_ori_step$fitted.values)


# Gerando o gráfico da curva ROC

ggplotly(
  ggroc(ROC, color = "#440154FF", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
                 color="grey40",
                 size = 0.2) +
    labs(x = "Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:",
                       round(ROC$auc, 3))) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          #panel.border = element_blank(),
          legend.position = "right",
          panel.background = element_blank()
          
    )
  )

# Gerando a curva ROC2 considerando todas as features originais

ROC2 <- roc(response = Tabela21_CP_Rol_Tipo_Ori$Rol_bin, 
           predictor = modelo_rol_tipo_ori$fitted.values)

# Teste estatistico comparando a curva ROC só com profissinais de saúde e a Curva ROC com todos tipos originais

roc.test(ROC, ROC2)

# Grafico para visualizaão das probabilidades de evento/não evento em função da qde de contribuições

ggplotly(
  Tabela21_CP_Rol_Tipo_Ori %>% 
    ggplot() +
    geom_point(aes(x = Pro_Sau , y = Rol_bin ), color = "#95D840FF", size = 2) +
    geom_smooth(aes(x = Pro_Sau, y = phat), 
                method = "glm", formula = y ~ x, 
                method.args = list(family = "binomial"), 
                se = F,
                color = "#440154FF", size = 2) +
        labs(x = "Contribuições de Profissionais de Saúde",
         y = "Probabilidade Rol") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          #panel.border = element_blank(),
          legend.position = "right",
          panel.background = element_blank()
          
          )
)
