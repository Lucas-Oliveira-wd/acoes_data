library(tidyverse)
library(gridExtra)
library(ggplot2)
library(readxl)
library(quantmod)
library(dplyr)
library(lubridate)

setwd("C:/files/projects/programacao/python/acoes_data/")

df_tri = read.csv("data/acoesb3.csv")

'df_day = read.csv("data/acoesb3cot.csv")'

emp_codes = unique(df_tri$codigo)

get_data_yahoo <- function(ticker, from = "2010-01-01", to = Sys.Date()) {
  # Obter cotações diárias
  stock_data <- getSymbols(ticker, src = "yahoo", from = from, to = to, auto.assign = FALSE)
  stock_data <- as.data.frame(stock_data)
  stock_data$date <- as.Date(rownames(stock_data))
  colnames(stock_data) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted", "date")
  
  # Obter dividendos
  dividends <- getDividends(ticker, from = from, to = to, auto.assign = FALSE)
  dividends <- as.data.frame(dividends)
  dividends$date <- as.Date(rownames(dividends))
  colnames(dividends) <- c("dividends", "date")
  
  # Juntar dividendos com cotações
  df <- left_join(stock_data, dividends, by = "date") %>%
    arrange(date) %>%
    mutate(dividends = ifelse(is.na(dividends), 0, dividends))
  
  # Calcular dividendos acumulados nos últimos 12 meses para cada linha
  df <- df %>%
    mutate(div_12m = sapply(date, function(d) {
      sum(df$dividends[df$date > (d - 365) & df$date <= d], na.rm = TRUE)
    }))
  
  # Calcular dividend yield (DY = dividendos dos últimos 12 meses / preço ajustado)
  df <- df %>%
    mutate(div_yield = ifelse(Adjusted > 0, div_12m / Adjusted, NA))
  
  return(df)
}


#função para removendo valores infinitos

rmv_inf_values_row <- function(df) {
  rows_with_inf <- apply(df, 1, function(row) any(is.infinite(row)))
  df <- df[!rows_with_inf, ]
  return(df)
}

# função para remover valores NA

rmv_na_val <- function(df) {
  incomplete_rows <- !complete.cases(df)
  
  if (any(incomplete_rows)) {
    df <- df[complete.cases(df), ]
  }
  
  return(df)
}

################################################################################
########         função para escrever tabela em uma planilha         ###########
################################################################################

##    Argumentos
# 1 - object
# 2 - file path
# 3 - extensão do arquivo (.alguma_coisa)
# 4 - nome do arquivo

escrever_res = function(df, fpath, extension, fname){
  library(openxlsx)
  
  # Obter uma representação do tempo como uma única string
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  
  # Criar o nome do arquivo combinando o nome do DataFrame, timestamp e extensão
  fnam <- paste(fname, '_', timestamp, extension, sep = '')
  fil_nam <- gsub("\\s+", "", fnam)
  fil_nam <- gsub(':', '_', fil_nam)
  
  # Verificar a extensão e escrever o arquivo apropriado
  if (extension == '.xlsx'){
    return(write.xlsx(df, file = paste(fpath, fil_nam, extension, sep = '')))
  }
  if (extension == '.csv'){
    return(write.csv(df, file = paste(fpath, fil_nam, sep = '')))
  }
}


#coletando as datas dos últimos balanços  
ult_bal_dates = c()
for (i in 1:nrow(df_tri)){
  if (df_tri[i,"ultBal"] %in% ult_bal_dates){}
  else{
    ult_bal_dates = c(ult_bal_dates, df_tri[i,"ultBal"])
  }
}

#retirando os valores das empresas que tem os trimestres vigentes diferentes
#de 31/03, 30/06, 30/09 e 31/12
for (t in ult_bal_dates) {
  if (endsWith(t, '03-31')){} else if(endsWith(t, '06-30')){}
  else if(endsWith(t, '09-30')){} else if(endsWith(t, '12-31')){}else {
    ult_bal_dates = ult_bal_dates[-match(t, ult_bal_dates)]
  }
}

#separando os df por períodos

#lista para conter os períodos
trimestres = list()

#lista para conter os critérios por períodos
crit_tri = list()

qtd_dias = 6*30 #qtd de dias de variação

qtd_dias_est = 7   #qtd de dias para o mercado estabelecer o preço

#criando os valores dos critérios
for (per in 1:length(ult_bal_dates)){
  print(paste('Criando o DF para o periodo: ', ult_bal_dates[per]))
  
  
  #Criando as variáveis do db trimestral
  ult_bal = codigo = roic = cres_rec5 = divb = disp = ativc = c();
  ativ = patl = recl12 = ebit12 = Lucl12 = recl3 = ebit3 = Lucl3 = ultIns = c()
  
  #loop através das linhas do db trimestral
  for (i in 1:nrow(df_tri)){
    
    #escolhendo o trimestre a ser trabalhado
    if (df_tri[i,"ultBal"] == ult_bal_dates[per]){
      
      ult_bal =  c(ult_bal, df_tri[i,"ultBal"])
      ultIns = c(ultIns, df_tri[i, "ultInsert"])
      codigo = c(codigo, df_tri[i,"codigo"])
      roic = c(roic, df_tri[i,"roic"])
      cres_rec5 = c(cres_rec5, df_tri[i,"cresRec5a"])
      divb = c(divb, df_tri[i,"divBruta"])
      disp = c(disp, df_tri[i,"disponib"])
      ativc = c(ativc, df_tri[i,"ativCirc"])
      ativ = c(ativ, df_tri[i,"ativos"])
      patl = c(patl, df_tri[i,"patLiq"])
      recl12 = c(recl12, df_tri[i,"recLiq12m"])
      ebit12 = c(ebit12, df_tri[i,"ebit12m"])
      Lucl12 = c(Lucl12, df_tri[i,"LucLiq12m"])
      recl3 = c(recl3, df_tri[i,"recLiq3m"])
      ebit3 = c(ebit3, df_tri[i, "ebit3m"])
      Lucl3 = c(Lucl3, df_tri[i, "LucLiq3m"])
    }
  }
  
  #df trimestral das demonstrações
  df_tri_dem = data.frame(ult_bal, roic, cres_rec5,
                          divb, disp, ativc, ativ, patl, recl12,
                          ebit12, Lucl12, recl3, ebit3, Lucl3, ultIns,
                          row.names = codigo)
  
  #criando as variaveis do db diário
  
  ult_cot = cotAtual = cod = divY = n_ac = cotF = n_acF = c()
  
  # loop nas linhas do dbday
  for (i in 1:nrow(df_day)){
    
    # obtendo o número da posição do codigo atual no df_tri_dem
    j_dem = match(df_day[i,"cod"], row.names(df_tri_dem))
    if (is.na(j_dem)){}
    else {
      # verificando se o codigo da ação já pertence a variavel cod    
      if (df_day[i,"cod"] %in% cod){
        
        #Se pertence:
        # obtendo o número da posição desse código no cod
        j = match(df_day[i,"cod"], cod)
        
        # verificando se o valor da ultima cotação da iteração atual é 
        # menor ou igual a data de inserção + dias
        if (as.Date((df_day[i,'ultCot'])) <= as.Date(ultIns)[j_dem]+qtd_dias){
          
          # verificando se o valor da ultima cotação da iteração atual é maior que
          #o valor pertencente à variável e menor ou igual a data de inserção
          # acrescida dos dias de equilibrio de preço do mercado
          if (as.Date(df_day[i,"ultCot"]) > as.Date(df_day[j,"ultCot"]) &&
              as.Date(df_day[i,'ultCot']) <=
              as.Date(ultIns[j_dem]) + qtd_dias_est){
            
            #se for maior: retirando o valor de cada variável correspondente à
            #posição do valor pertencente
            ult_cot = ult_cot[-j]; cod = cod[-j]; cotAtual = cotAtual[-j];
            divY = divY[-j]; n_ac = n_ac[-j]; cotF = cotF[-j]; n_acF = n_acF[-j]
            
            #colocando os valores que teve a maior ultCot (preço atualizado)          
            ult_cot = c(ult_cot, df_day[i,"ultCot"])
            cod = c(cod, df_day[i,"cod"])
            cotAtual = c(cotAtual, df_day[i,"cotAtual"])
            divY = c(divY, df_day[i, "divYield"])
            n_ac = c(n_ac, df_day[i, "nAcoes"])
            cotF = c(cotF, df_day[i, "cotAtual"])
            n_acF = c(n_acF, df_day[i, "nAcoes"])
          } else {
            cotF[j] = df_day[i, "cotAtual"]
            
            n_acF[j] = df_day[i, "nAcoes"]
          }
          
        }
        
        
      } else if (as.Date(df_day[i,'ultCot']) <= as.Date(ultIns[j_dem])) {
        
        #se não pertence e é menor que o período:
        #apenas colocando o valor da iteração atual
        ult_cot = c(ult_cot, df_day[i,"ultCot"])
        cod = c(cod, df_day[i,"cod"])
        cotAtual = c(cotAtual, df_day[i,"cotAtual"])
        divY = c(divY, df_day[i, "divYield"])
        n_ac = c(n_ac, df_day[i, "nAcoes"])
        cotF = c(cotF, df_day[i, "cotAtual"])
        n_acF = c(n_acF, df_day[i, "nAcoes"])
      } 
    }
  }
  
  
  # ordenando as variáves antes de se unirem ao trimestral
  ult_cot_t = cotAtual_t = divY_t = n_ac_t = cotF_t = n_acF_t = c()
  
  #loop através das linhas do db trimestral das demonstrações
  for (i in 1:nrow(df_tri_dem)){
    
    #encontrando a posição do codigo atual na linha
    j = match(row.names(df_tri_dem)[i], cod)
    
    ult_cot_t = c(ult_cot_t, ult_cot[j])
    cotAtual_t = c(cotAtual_t, cotAtual[j])
    divY_t = c(divY_t, divY[j])
    n_ac_t = c(n_ac_t, n_ac[j])
    cotF_t = c(cotF_t, cotF[j])
    n_acF_t = c(n_acF_t, n_acF[j])
  }
  
  df_tri_dem$ult_cot_t = ult_cot_t; df_tri_dem$cotAtual_t = cotAtual_t
  df_tri_dem$divY_t = divY_t; df_tri_dem$n_ac_t = n_ac_t
  df_tri_dem$cotF_t = cotF_t; df_tri_dem$n_acF_t = n_acF_t
  
  
  #gridExtra::grid.table(df_tri_3t22 %>% slice(1:20))
  
  arr_ind = 3 #casas decimais dos indices
  
  # criando os índices
  lpa = round(Lucl12/n_ac_t, arr_ind)
  lpa3 = round(as.numeric(Lucl3)/n_ac_t, arr_ind)
  vpa = round(patl/n_ac_t, arr_ind)
  cx_a = round(disp/n_ac_t, arr_ind)
  ativc_a = round(ativc/n_ac_t, arr_ind)
  ativ_a = round(ativ/n_ac_t, arr_ind)
  divb_a = round(divb/n_ac_t, arr_ind)
  ebit_a = round(ebit12/n_ac_t, arr_ind)
  ebit_a3 = round(as.numeric(ebit3)/n_ac_t, arr_ind)
  dividend = round((as.numeric(divY_t)/100)*cotAtual_t, arr_ind)
  rec_a = round(recl12/n_ac_t, arr_ind)
  rec_a3 = round(as.numeric(recl3)/n_ac_t, arr_ind)
  cot = cotAtual_t
  
  
  crit = data.frame(lpa, lpa3, vpa, cx_a, ativc_a, ativ_a, divb_a, ebit_a,
                    ebit_a3, dividend, rec_a, rec_a3, cot,
                    row.names = codigo)
  
  #############       filtrando o db        ###################################
  
  
  
  crit = rmv_inf_values_row(crit)
  
  
  crit <- rmv_na_val(crit)
  
  
  #passando o df para a lista
  trimestres[[per]] = df_tri_dem
  
  
  colnames(crit) = c("LPA", 'LPA (tri)', "VPA",
                     "Caixa/Ação", "Ativos Circulantes/Ação",
                     "Ativos/Ação", "Dív Bruta/Ação", "EBIT/Ação",
                     'EBIT/Ação (tri)',
                     "Dividendos",
                     'Receita/ Ação',
                     'Receita/Ação (tri)',
                     'Preço da Ação')
  
  #gridExtra::grid.table(crit %>% slice(1:20))
  
  crit_tri[[per]] = crit
  print(paste('DF do periodo: ', ult_bal_dates[per], ' criado'))
}

################################################################################
#########       função para retornar as variações dos preços      ##############
################################################################################

#per_i => periodo a ser analisado
addVarPrice = function(per_i){
  
  var_price = c()
  
  for (r in 1:nrow(crit_tri[[per_i]])) {
    
    # encontrando a posição da linha do codigo da iteração no df das demonstrações
    j_dem = match(row.names(crit_tri[[per_i]])[r], row.names(trimestres[[per_i]]))
    
    # encontrando a posição da linha do codigo da iteração no df dos criterios
    # do período anterior
    j_dem_bef = match(row.names(crit_tri[[per_i]])[r], row.names(crit_tri[[per_i-1]]))
    
    # criando o df das variacoes dos indices
    
    price_i = trimestres[[per_i]][j_dem,'cotAtual_t']
    price_f = trimestres[[per_i]][j_dem,'cotF_t']*
      (trimestres[[per_i]][j_dem,'n_acF_t']/trimestres[[per_i]][j_dem, 'n_ac_t'])
    
    var_price = c(var_price, round((price_f - price_i)/price_i, 4))
    
  }
  
  # adicionando as variações dos precos ao df crit do periodo
  crit_tri[[per_i]]$v_price = var_price
  
  colnames(crit_tri[[per_i]]) = c("L/P", 'L/P (tri)', "VPA/P", "ROE" , 'ROE (tri)', "ROIC",
                                  "(Caixa/Ação)/Preço", "(Ativos Circulantes/Ação)/Preço",
                                  "(Ativos/Ação)/Preço", "Dív Bruta/Caixa", "Marg. EBIT",
                                  'Marg. EBIT (tri)', "Marg. Líquida", 'Marg. Líquida (tri)',
                                  "Cresc. Rec. (5 Anos)", "Dividendyield", "Lynch",
                                  'Lynch (tri)',  "Dív. Bruta/Lucro Mensal", 'PSR (invertido)',
                                  'PSR (invertido) (tri)', 'EBIT/P', 'EBIT/P (tri)',
                                  'EBIT/Ativo', 'EBIT/Ativo (tri)', 'Div Bruta/Patrimonio', "v_price")
  
  #removendo valores infinitos
  
  
  rmv_inf_values_row <- function(df) {
    rows_with_inf <- apply(df, 1, function(row) any(is.infinite(row)))
    df <- df[!rows_with_inf, ]
    return(df)
  }
  
  crit_tri[[per_i]] = rmv_inf_values_row(crit_tri[[per_i]])
  
  #removando valore NA
  
  rmv_na_val <- function(df) {
    incomplete_rows <- !complete.cases(df)
    
    if (any(incomplete_rows)) {
      df <- df[complete.cases(df), ]
    }
    
    return(df)
  }
  crit_tri[[per_i]] <- rmv_na_val(crit_tri[[per_i]])
  
  return(crit_tri[[per_i]])
}



############################################################################
###############    função para retirar valores extremos    #################
############################################################################

# remover valores extremos
rmv_wild = function(df, fator_multiplicativo) {
  
  exib_wild = function(dados, fator_multiplicativo){
    # Obter estatísticas do boxplot
    stats <- boxplot.stats(dados)
    
    # Calcular limites personalizados
    limite_inferior <- stats$stats[2] - fator_multiplicativo * IQR(dados, na.rm = T)
    limite_superior <- stats$stats[4] + fator_multiplicativo * IQR(dados, na.rm = T)
    
    # Identificar outliers
    outliers <- dados[dados < limite_inferior | dados > limite_superior]
    
    # Exibir os valores dos outliers
    return(outliers)  
  }
  
  wild_row = c('so', 'para','length', 'nao', 'ser', 'zero')
  
  n_iter = 0
  while (length(wild_row) != 0) {
    wild_row = c()
    for (c in 1:ncol(df)) {
      if (length(exib_wild(df[,c], fator_multiplicativo)) != 0){
        for (i in exib_wild(df[,c], fator_multiplicativo)) {
          #numero das linhas
          j = match(i, df[,c])
          df[j,c] = NA
          if (row.names(df)[j] %in% wild_row){} else {
            wild_row = c(wild_row, row.names(df)[j])  
          }
        }
      }
    }
    rmv_na_val <- function(df) {
      incomplete_rows <- !complete.cases(df)
      
      if (any(incomplete_rows)) {
        df <- df[complete.cases(df), ]
      }
      
      return(df)
    }
    
    print(paste('empresas removidas na iteração', n_iter, ':'))
    print(wild_row)
    n_iter = n_iter+1
    
    
    df = rmv_na_val(df)  
  }
  
  return(df)
}



#escolhendo o periodo que sera analisado
per_choice = crit_tri[[4]]

# verificando NA values
colSums(is.na(per_choice))

boxplot(scale(per_choice), range = 3)

per_choice=rmv_wild(per_choice, 3)

boxplot(scale(per_choice), range = 3)


################################################################################
#############         Escolhendo os Índices           ##########################
################################################################################

##############                 regressão                    ####################

# Realize a regressão linear
modelo_regressao <- lm(per_choice$`Preço da Ação`~per_choice$LPA+
                         per_choice$`LPA (tri)`+per_choice$VPA+
                         per_choice$`Caixa/Ação`+
                         per_choice$`Ativos Circulantes/Ação`+
                         per_choice$`Ativos/Ação`+per_choice$`Dív Bruta/Ação`+
                         per_choice$`EBIT/Ação`+per_choice$`EBIT/Ação (tri)`+
                         per_choice$Dividendos+per_choice$`Receita/ Ação`+
                         per_choice$`Receita/Ação (tri)`)

# Exiba o sumário do modelo
summary(modelo_regressao)

# vizualizando a matriz de correlação
library(ggcorrplot)
corr_matrix <- data.frame(cor(scale(per_choice)))
ggcorrplot(corr_matrix)

##    Argumentos
# 1 - object
# 2 - file path
# 3 - extensão do arquivo (.alguma_coisa)
# 4 - nome do arquivo
escrever_res (corr_matrix,
    'C:/files/projects/programacao/python/acoes_data/logs/correlation_matrix/',
    '.xlsx', 'corr_per_choice')

# regressão com algumas variáveis retiradas (valor mais alto das corr altas)
model_fit = lm(per_choice$`Preço da Ação`~per_choice$LPA+
                 per_choice$`LPA (tri)`+per_choice$VPA+
                 per_choice$`Caixa/Ação`+
                 per_choice$`Ativos Circulantes/Ação`+
                 per_choice$`Ativos/Ação`+per_choice$`Dív Bruta/Ação`+
                 per_choice$`EBIT/Ação`+per_choice$`EBIT/Ação (tri)`+
                 per_choice$Dividendos+
                 per_choice$`Receita/Ação (tri)`)

summary(model_fit)

# regressão com algumas variáveis retiradas (valor mais alto)
model_fit2 = lm(per_choice$`Preço da Ação`~per_choice$LPA+
                  per_choice$`LPA (tri)`+per_choice$VPA+
                  per_choice$`Caixa/Ação`+
                  per_choice$`Ativos Circulantes/Ação`+
                  per_choice$`Ativos/Ação`+per_choice$`Dív Bruta/Ação`+
                  per_choice$`EBIT/Ação`+
                  per_choice$Dividendos+
                  per_choice$`Receita/Ação (tri)`)

summary(model_fit2)

# regressão com algumas variáveis retiradas (valor mais alto)
model_fit3 = lm(per_choice$`Preço da Ação`~per_choice$LPA+
                  per_choice$`LPA (tri)`+per_choice$VPA+
                  per_choice$`Caixa/Ação`+
                  per_choice$`Ativos Circulantes/Ação`+
                  per_choice$`Dív Bruta/Ação`+
                  per_choice$`EBIT/Ação`+
                  per_choice$Dividendos+
                  per_choice$`Receita/Ação (tri)`)

summary(model_fit3)

# regressão com algumas variáveis retiradas (valor mais alto)
model_fit4 = lm(per_choice$`Preço da Ação`~per_choice$LPA+
                  per_choice$`LPA (tri)`+per_choice$VPA+
                  per_choice$`Caixa/Ação`+
                  per_choice$`Ativos Circulantes/Ação`+
                  per_choice$`EBIT/Ação`+
                  per_choice$Dividendos+
                  per_choice$`Receita/Ação (tri)`)

summary(model_fit4)

# regressão com algumas variáveis retiradas (valor mais alto)
model_fit5 = lm(per_choice$`Preço da Ação`~per_choice$LPA+
                  per_choice$`LPA (tri)`+per_choice$VPA+
                  per_choice$`Caixa/Ação`+
                  per_choice$`Ativos Circulantes/Ação`+
                  per_choice$`EBIT/Ação`+
                  per_choice$`Receita/Ação (tri)`)

summary(model_fit5)

# regressão com algumas variáveis retiradas (valor mais alto)
model_fit6 = lm(per_choice$`Preço da Ação`~per_choice$LPA+
                  per_choice$VPA+
                  per_choice$`Caixa/Ação`+
                  per_choice$`Ativos Circulantes/Ação`+
                  per_choice$`EBIT/Ação`+
                  per_choice$`Receita/Ação (tri)`)

summary(model_fit6)

# regressão com algumas variáveis retiradas (valor mais alto)
model_fit7 = lm(per_choice$`Preço da Ação`~per_choice$LPA+
                  per_choice$VPA+
                  per_choice$`Caixa/Ação`+
                  per_choice$`Ativos Circulantes/Ação`+
                  per_choice$`EBIT/Ação`)
summary(model_fit7)

# regressão com algumas variáveis retiradas (valor mais alto)
model_fit8 = lm(per_choice$`Preço da Ação`~per_choice$LPA+
                  per_choice$VPA+
                  per_choice$`Caixa/Ação`+
                  per_choice$`Ativos Circulantes/Ação`+
                  per_choice$`EBIT/Ação`)

summary(model_fit8)

# regressão com algumas variáveis retiradas (valor mais alto)
model_fit9 = lm(per_choice$`Preço da Ação`~
                  per_choice$`LPA (tri)`+per_choice$VPA+
                  per_choice$Dividendos)

summary(model_fit9)

# função para a equação da reta de regressão
#
#reta = function (lpa, vpa, cxa, atca) {
#  y = 3.1065 + 3.2643*lpa + 0.4131*vpa + 1.3144*cxa + -0.2865*atca
#  return(round(y, 2))
#}
#
## vetor para conter os valores
#p_jus = c()
#for (p in 1:nrow(per_choice)) {
#  p_jus[p] = reta(per_choice[p,"LPA"], per_choice[p,"VPA"],
#                  per_choice[p, "Caixa/Ação"],
#                  per_choice[p, "Ativos Circulantes/Ação"])
#}
#

# função para a equação da reta de regressão

reta = function (lpa_t, vpa, divid) {
  y = 2.3702 + 6.7774*lpa_t + 0.4300*vpa + 6.1210*divid
  return(round(y, 2))
}

# vetor para conter os valores
p_jus = c()
for (p in 1:nrow(per_choice)) {
  p_jus[p] = reta(per_choice[p,"LPA (tri)"], per_choice[p,"VPA"],
                  per_choice[p, "Dividendos"])
}

#adicionando ao db

per_choice$preco_jus = p_jus

# analisando a discrepancia

disc = (per_choice$preco_jus - per_choice$`Preço da Ação`)/
  per_choice$`Preço da Ação`

#adicionando ao db

per_choice$discrepancia = disc
