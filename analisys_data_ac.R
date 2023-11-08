library(tidyverse)
library(gridExtra)
library(ggplot2)
library(readxl)

df_tri = read.csv("data/acoesb3.csv")

df_day = read.csv("data/acoesb3cot.csv")


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

#gridExtra::grid.table(df_day %>% slice(1:20)) # para plotar o db

################################################################################
############      CRIANDO OS DFs DIÁRIOS     ###################################
################################################################################

#separando os df por períodos

#lista para conter os períodos
trimestres = list()

#lista para conter os critérios por períodos
crit_tri = list()

#criando os valores dos critérios
for (per in 1:length(ult_bal_dates)){
  print(paste('Criando o DF para o periodo: ', ult_bal_dates[per]))
  
###################   start att 2023/11/07 19:36    ############################
  
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
                          ebit12, Lucl12, recl3, ultIns, row.names = codigo)
  
  #criando as variaveis do db diário
  
  ult_cot = cotAtual = cod = divY = n_ac = c()
  
  # loop na coluna 'ultCot' do dbday
  for (i in 1:nrow(df_day)){
    
    # verificando se o codigo da ação já pertence a coluna do periodo df codigo    
    if (df_day[i,"cod"] %in% row.names(df_tri_dem)){
      
      # obtendo o número da posição desse código no df_tri_dem
      j = match(df_day[i,"cod"], row.names(df_tri_dem))
      
      # verificando se o valor da ultima cotação da iteração atual é maior que
      #o valor pertencente à variável e menor ou igual a data de inserção
      if (as.Date(df_day[i,"ultCot"]) > as.Date(df_day[j,"ultCot"]) &&
          as.Date(df_day[i,'ultCot']) <= as.Date(ultIns[j])){
        
        #se for maior: retirando o valor de cada variável correspondente à
        #posição do valor pertencente
        ult_cot = ult_cot[-j]; cod = cod[-j]; cotAtual = cotAtual[-j];
        divY = divY[-j]; n_ac = n_ac[-j]
        
        #colocando os valores que teve a maior ultCot (preço atualizado)
        #ult_cot[per] = df_day[i,"ultCot"]    ### codigo travado            
        ult_cot = c(ult_cot, df_day[i,"ultCot"])
        cod = c(cod, df_day[i,"cod"])
        cotAtual = c(cotAtual, df_day[i,"cotAtual"])
        divY = c(divY, df_day[i, "divYield"])
        n_ac = c(n_ac, df_day[i, "nAcoes"])
      }
    } else if (as.Date(df_day[i,'ultCot']) <= as.Date(ult_bal_dates[per])) {
      #se não pertence e é menor que o período:
      #apenas colocando o valor da iteração atual
      ult_cot = c(ult_cot, df_day[i,"ultCot"])
      cod = c(cod, df_day[i,"cod"])
      cotAtual = c(cotAtual, df_day[i,"cotAtual"])
      divY = c(divY, df_day[i, "divYield"])
      n_ac = c(n_ac, df_day[i, "nAcoes"])
    }
      
  }

  
  # ordenando as variáves antes de se unirem ao trimestral
  ult_cot_t = cotAtual_t = divY_t = n_ac_t = c()
  
  #loop através das linhas do db trimestral das demonstrações
  for (i in 1:nrow(df_tri_dem)){
    
    ult_cot_t = c(ult_cot_t, ult_cot[i])
    cotAtual_t = c(cotAtual_t, cotAtual[i])
    divY_t = c(divY_t, divY[i])
    n_ac_t = c(n_ac_t, n_ac[i])
    
  }
  
  df_tri_dem$ult_cot_t = ult_cot_t; df_tri_dem$cotAtual_t = cotAtual_t
  df_tri_dem$divY_t = divY_t; df_tri_dem$n_ac_t = n_ac_t


  
  
###################   end att 2023/11/07 19:36    ############################## 
  
  
  #gridExtra::grid.table(df_tri_3t22 %>% slice(1:20))
  
  
  # criando os índices
  pl = round(cotAtual_t/(Lucl12/n_ac_t), 2)
  pl3 = round(cotAtual_t/(as.numeric(Lucl3)/n_ac_t), 2)
  lp = round((Lucl12/n_ac_t)/cotAtual_t, 2)
  lp3 = round((as.numeric(Lucl3)/n_ac_t)/cotAtual_t, 2)
  vp = round((patl/n_ac_t)/cotAtual_t, 2)
  roe = round((Lucl12/patl)*100, 2)
  roe3 = round((as.numeric(Lucl3)/patl)*100, 2)
  roic = round(as.numeric(roic), 2)
  cxa_p = round((disp/n_ac_t)/cotAtual_t, 2)
  ativc_p = round((ativc/n_ac_t)/cotAtual_t, 2)
  ativ_p = round((ativ/n_ac_t)/cotAtual_t, 2)
  div_cxa = round(divb/disp, 2)
  marg_ebit = round((ebit12/recl12)*100, 2)
  marg_ebit3 = round((as.numeric(ebit3)/recl3)*100, 2)
  marg_liq = round((Lucl12/recl12)*100, 2)
  marg_liq3 = round((as.numeric(Lucl3)/recl3)*100, 2)
  cres_rec5 = round(as.numeric(cres_rec5), 2)
  divY_t = round(as.numeric(divY_t), 2)
  lynch = round((divY_t+(cres_rec5/5))/pl, 2)
  lynch3 = round((divY_t/3+(cres_rec5/15))/pl3, 2)
  div_lucm = round(divb/(as.numeric(Lucl3)/3), 2)
  psr_inv = round((recl12/n_ac_t)/cotAtual_t, 2)
  psr_inv3 = round((as.numeric(recl3)/n_ac_t)/cotAtual_t, 2)
  ebit_p = round((ebit12/n_ac_t)/cotAtual_t, 2)
  ebit_p3 = round((as.numeric(ebit3)/n_ac_t), 2)
  ebit_ativ = round(ebit12/ativ, 2)
  ebit_ativ3 = round((as.numeric(ebit3)/ativ), 2)
  divb_pat = round(divb/patl, 2)
  
  
  crit = data.frame(lp, lp3, vp, roe, roe3, roic, cxa_p, ativc_p, ativ_p,
                    div_cxa, marg_ebit, marg_ebit3 ,marg_liq, marg_liq3,
                    cres_rec5 ,divY_t ,lynch, lynch3, div_lucm, psr_inv,
                    psr_inv3, ebit_p, ebit_p3, ebit_ativ, ebit_ativ3,
                    divb_pat, row.names = codigo)
  
  #############       filtrando o db        ###################################
  
  #removendo valores infinitos
  
  
###############     chatgpt start      ###############################
  
  rmv_inf_values_row <- function(df) {
    rows_with_inf <- apply(df, 1, function(row) any(is.infinite(row)))
    df <- df[!rows_with_inf, ]
    return(df)
  }
  
###############     chatgpt  end     ###############################
  
  crit = rmv_inf_values_row(crit)
  
 
  
###############     chatgpt start      ###############################
  
  rmv_na_val <- function(df) {
    incomplete_rows <- !complete.cases(df)
    
    if (any(incomplete_rows)) {
      df <- df[complete.cases(df), ]
    }
    
    return(df)
  }
  crit <- rmv_na_val(crit)
  
###############     chatgpt  end     ###############################
  
  
#############       procurando roe's falsos, lucl e patl negativos       #######
  emp_lucpat_neg = c()
  for(r in 1:length(df_tri_dem[,1])){
    if(df_tri_dem[r,'Lucl12'] < 0 && df_tri_dem[r,'patl'] < 0){
      emp_lucpat_neg = c(emp_lucpat_neg, df_tri_dem[r, 'codigo'])
    }
  }
  
  print(paste('qtd de empresas com roes falsos: ', length(emp_lucpat_neg)))
  roe_err = c()
  for (r in emp_lucpat_neg){
    roe_err = c(roe_err,(df_tri_dem[r,"Lucl12"]/df_tri_dem[r,'patl'])*100)
  }
  
  print(paste('empresa com o maior roe falso: ',
              emp_lucpat_neg[match(max(roe_err), roe_err)]))
  
  ## retirando os roes falsos
  
  print(paste('número de empresas antes da retirada dos roes falsos: ',
              nrow(crit)))
  
  
  for (r in 1:length(crit$ROE)){
    if (length(emp_lucpat_neg) < 0 && row.names(crit)[r] %in% emp_lucpat_neg){
      print(row.names(crit)[r]) #empresa retirada (tinha passado pelos
      #2 filtros anteriores)
      crit = crit[-r,]
    }
  }
  
  print(paste('número de empresas depois da retirada dos roes falsos: ',
              nrow(crit)))
  
  #passando o df para a lista
  trimestres[[per]] = df_tri_dem
  
  
  colnames(crit) = c("L/P", 'L/P (tri)', "VPA/P", "ROE" , 'ROE (tri)', "ROIC",
                     "(Caixa/Ação)/Preço", "(Ativos Circulantes/Ação)/Preço",
                     "(Ativos/Ação)/Preço", "Dív Bruta/Caixa", "Marg. EBIT",
                     'Marg. EBIT (tri)', "Marg. Líquida", 'Marg. Líquida (tri)',
                     "Cresc. Rec. (5 Anos)", "Dividendyield", "Lynch",
                    'Lynch (tri)',  "Dív. Bruta/Lucro Mensal", 'PSR (invertido)',
                    'PSR (invertido) (tri)', 'EBIT/P', 'EBIT/P (tri)',
                    'EBIT/Ativo', 'EBIT/Ativo (tri)', 'Div Bruta/Patrimonio')
  
  #gridExtra::grid.table(crit %>% slice(1:20))
  
  crit_tri[[per]] = crit
  print(paste('DF do periodo: ', ult_bal_dates[per], ' criado'))
}


###################             periodo 6         ##############################
################################################################################


###########       essa linha de codigos é só para testes        ################
################################################################################

#criando as variaveis do db diário
ult_cot = cod = cotAtual = c()

# loop na coluna 'ultCot' do dbday
for (i in 1:nrow(df_day)){
  
  # verificando se o codigo da ação já pertence a coluna do periodo df cod    
  if (df_day[i,"cod"] %in% cod){
    
    # obtendo o número da posição desse código na varável cod
    j = match(df_day[i,"cod"], cod)
    
    # verificando se o valor da ultima cotação da iteração atual é maior que
    #o valor pertencente à variável
    if (as.Date(df_day[i,"ultCot"]) > as.Date(df_day[j,"ultCot"])){
      
      #se for maior: retirando o valor de cada variável correspondente à
      #posição do valor pertencente
      ult_cot = ult_cot[-j]; cod = cod[-j]; cotAtual = cotAtual[-j]
      
      #colocando os valores que teve a maior ultCot (preço atualizado)
      #ult_cot[per] = df_day[i,"ultCot"]    ### codigo travado            
      ult_cot = c(ult_cot, df_day[i,"ultCot"])
      cod = c(cod, df_day[i,"cod"])
      cotAtual = c(cotAtual, df_day[i,"cotAtual"])
    }
  } else if (as.Date(df_day[i,'ultCot']) <= as.Date(ult_bal_dates[per])) {
    #se não pertence apenas colocando o valor da iteração atual
    ult_cot = c(ult_cot, df_day[i,"ultCot"])
    cod = c(cod, df_day[i,"cod"])
    cotAtual = c(cotAtual, df_day[i,"cotAtual"])
  }
  
}

last_cot = data.frame(ult_cot, cotAtual)
row.names(last_cot) = cod

var_price = c()
qtd_dias = 10 #qtd de dias de variação
per_i = 6 #periodo a ser analisado

for (r in 1:nrow(crit_tri[[per_i]])) {
  
  # encontrando a posição da linha do codigo da iteração no df das demonstrações
  j_dem = match(row.names(crit_tri[[per_i]])[r], row.names(trimestres[[per_i]]))
  
  # encontrando a posição da linha do codigo da iteração no df last_cot
  j_lastc = match(row.names(crit_tri[[per_i]])[r], row.names(last_cot))
  
  price_i = trimestres[[per_i]][j_dem,cotAtual_t]
  
  
  
  var_price = c(var_price,
(last_cot[j_lastc, "cotAtual"] - trimestres[[6]][j_dem, "cotAtual_t"])/trimestres[[6]][j_dem, "cotAtual_t"])
  
}

# adicionando as variações dos precos ao df crit do 5 periodo
crit_tri[[6]]$v_price = var_price

colnames(crit_tri[[6]]) = c("L/P", 'L/P (tri)', "VPA/P", "ROE" , 'ROE (tri)', "ROIC",
                             "(Caixa/Ação)/Preço", "(Ativos Circulantes/Ação)/Preço",
                             "(Ativos/Ação)/Preço", "Dív Bruta/Caixa", "Marg. EBIT",
                             'Marg. EBIT (tri)', "Marg. Líquida", 'Marg. Líquida (tri)',
                             "Cresc. Rec. (5 Anos)", "Dividendyield", "Lynch",
                             'Lynch (tri)',  "Dív. Bruta/Lucro Mensal", 'PSR (invertido)',
                             'PSR (invertido) (tri)', 'EBIT/P', 'EBIT/P (tri)',
                             'EBIT/Ativo', 'EBIT/Ativo (tri)', 'Div Bruta/Patrimonio', "v_price")


#################         coeficiente de correlação         ####################
library(openxlsx)
cor_tab = data.frame(cor(crit_tri[[6]]))
write.xlsx(cor_tab,
  file = 'C:/files/projects/programacao/python/acoes_data/correlacao.xlsx')

##      diagrama de dispersão
plot(crit_tri[[6]]$`Marg. EBIT`, crit_tri[[6]]$`Marg. EBIT (tri)`)



## aplicando teste de normalidade
apply(crit_tri[[5]], 2, shapiro.test)


################################################################################
#################       Verificando os boxplot        ##########################
################################################################################

#                 L/P
boxplot(crit_tri[[5]]$`L/P`, horizontal = T, range = 3)
title(xlab = "Valores de L/P", main = 'Boxplot para o L/P', cex.main = 2.5,
      cex.lab = 2)

#                 L/P (tri)
boxplot(crit_tri[[5]]$`L/P (tri)`, horizontal = T, range = 3)
title(xlab = "Valores de L/P (tri)", main = 'Boxplot para o L/P (tri)',
      cex.main = 2.5, cex.lab = 2)

#                 VPA/P
boxplot(crit_tri[[5]]$`VPA/P`, horizontal = T, range = 3)
title(xlab = "Valores de VPA/P", main = 'Boxplot para o VPA/P',
      cex.main = 2.5, cex.lab = 2)

#                 ROE
boxplot(crit_tri[[5]]$ROE, horizontal = T, range = 3)
title(xlab = "Valores do ROE", main = 'Boxplot para o ROE',
      cex.main = 2.5, cex.lab = 2)

#                 ROE (tri)
boxplot(crit_tri[[5]]$`ROE (tri)`, horizontal = T, range = 3)
title(xlab = "Valores do ROE (tri)", main = 'Boxplot para o ROE (tri)',
      cex.main = 2.5, cex.lab = 2)

#                 ROIC
boxplot(crit_tri[[5]]$ROIC, horizontal = T, range = 3)
title(xlab = "Valores do ROIC", main = 'Boxplot para o ROIC',
      cex.main = 2.5, cex.lab = 2)

#                 Dív Bruta/Caixa
boxplot(crit_tri[[5]]$`Dív Bruta/Caixa`, horizontal = T, range = 3)
title(xlab = "Valores da Dív Bruta/Caixa",
      main = 'Boxplot para a Dív Bruta/Caixa',
      cex.main = 2.5, cex.lab = 2)

#                 Marg. EBIT
boxplot(crit_tri[[5]]$`Marg. EBIT`, horizontal = T, range = 3)
title(xlab = "Valores da Marg. EBIT", main = 'Boxplot para a Marg. EBIT',
      cex.main = 2.5, cex.lab = 2)

#                 Marg. EBIT (tri)
boxplot(crit_tri[[5]]$`Marg. EBIT (tri)`, horizontal = T, range = 3)
title(xlab = "Valores da Marg. EBIT (tri)",
      main = 'Boxplot para a Marg. EBIT (tri)',
      cex.main = 2.5, cex.lab = 2)





################################################################################
####       verificando os histogramas                  #########################
################################################################################

##########   L/P
hist(crit_tri[[5]]$`L/P`, breaks = 100)

##########      L/P (tri)     
d = density(crit_tri[[5]]$`L/P (tri)`, bw = 0.2)
plot(d, main = "Densidade do L/P (tri)", xlim = c(-1,1))
polygon(d, col = "#5577ff", border = "white")

#########      VPA/P
d = density(crit_tri[[5]]$`VPA/P`, bw = 0.2)
plot(d, main = "Densidade do VPA/P", xlim = c(-2,4))
polygon(d, col = "#ffcccc", border = "white")

d = density(crit_tri[[5]]$ROE)
plot(d, main = "Densidade do ROE")
polygon(d, col = "#00cccc", border = "black")

d = density(crit_tri[[5]]$ROIC)
plot(d, main = "Densidade do ROIC")
polygon(d, col = "#cccccc", border = "black")

d = density(crit_tri[[5]]$`Preço/(Caixa/Ação)`)
plot(d, main = "Densidade do P/Cx")
polygon(d, col = "#ccccff", border = "black")

d = density(crit_tri[[5]]$`Preço/(Ativos Circulantes/Ação)`)
plot(d, main = "Densidade do P/AtC")
polygon(d, col = "#ccff00", border = "black")

d = density(crit_tri[[5]]$`Preço/(Ativos/Ação)`)
plot(d, main = "Densidade do P/At")
polygon(d, col = "#005500", border = "white")

d = density(crit_tri[[5]]$`Dív Bruta/Caixa`)
plot(d, main = "Densidade do DivB/Cx")
polygon(d, col = "#aa0022", border = "black")




norm = function(vec){
  vecn = c()
  for (i in vec){
    vecn = c(vecn, (i - min(vec))/(max(vec)-min(vec)))
  }
  return(vecn)
}

v_ag = c()
for (i in 1:length(crit[,1])){
  v_ag[i] = -norm(crit$`P/L`)[i] -norm(crit$`P/VPA`)[i] +norm(crit$ROE)[i]
  +norm(crit$ROIC)[i]
  -norm(crit$`Preço/(Caixa/Ação)`)[i]
  -norm(crit$`Preço/(Ativos Circulantes/Ação)`)[i]
  -norm(crit$`Preço/(Ativos/Ação)`)[i] -norm(crit$`Dív Bruta/Caixa`)[i]
  +norm(crit$`Mar. EBITDA`)[i] +norm(crit$`Marg. Líquida`)[i]
  +norm(crit$`Cresc. Rec. (5 Anos)`)[i]
  +norm(crit$Dividendyield)[i] +norm(crit$Lynch)[i]
  -norm(crit$`Dív. Bruta/Lucro Mensal`)[i]
}

result = data.frame(v_ag, row.names(crit), row.names = row.names(crit))

## 10 melhores
best10 = c()
val10 = c()
for (i in sort(result$v_ag, decreasing = T)) {
  if (length(best10) < 30){
    val10 = c(val10, i)
    best10 = c(best10, result$row.names.crit.[match(i, result$v_ag)])
  }
}

for (i in 1:length(best10)){
  print(c(best10[i], val10[i]))
}


##################            Fuzzy Sets           #############################
library(FuzzyNumbers)
library(devtools)



# a função a seguir precisa de 6 argumentos:
# 1 - data.frame para ser encontrando as funções trapezoidais
# 2 - coluna objetivo, para ser comparada com as outras
# 3 - numero de linhas do resultado
# 4 - valor da acurácia , quanto maior, maior a acurácia, (accur > 1)
# 5 - valor mínimo para a média dos outputs da função fuzzy (< 0.5)

look_fuzzy_set = function(df, col_obj, num_row, accurate, mean_lim_bottom) {
  
  col_num = match(col_obj, colnames(df))
  
  if (mean_lim_bottom >= 0.5 || mean_lim_bottom < 0){
    stop('error: mean_lim_bottom deve ser maior que 0 e menor que
                   0.5')
  }
  
  arredond = 2  # qtd de casa decimais para os parametros
  
  # lista para conter as probabiblidades dos runifs
  prob_runif = c()
  
  # lista para conter os df com os valores dos paramentros
  df_list = list()
  for (c in 1:ncol(df)){
    if (colnames(df)[c] != col_obj){
      df_list[[c]] = data.frame(matrix(ncol = 6, nrow = 0))
      colnames(df_list[[c]]) = c("indices", "p", "q", "r", "s", "coef.corr")
      
      prob_runif = c(prob_runif, 0) # valor deve ser menor que 0.9
                                      # por causa do break  
    }
  }
  
  
  ## criando o indice de coluna
  
  c = 1
  while(round(mean(prob_runif),3) < 0.8){
    
    if (colnames(df)[c] != col_obj){
      input = df[,c]
      
      if (diff(range(input)) == 0){
        stop(paste('errot: coluna ', colnames(df)[c],
                   'tem amplitude igual a zero, sem desvio padrão'))
      }
    
      #############       improve parameters search       ######################
      ##########################################################################
      
      if (nrow(df_list[[c]]) < num_row){
        
        
        
        # Calculando os 5 percentis
        percentis <- quantile(input, probs = seq(0, 1, 1/5)) # 1/5 é para
                                                     #encontrar 4 valores
        
        # definindo os valores iniciais dos parametros
        p = round(percentis[2], arredond)
        q = round(percentis[3], arredond)
        r = round(percentis[4], arredond)
        s = round(percentis[5], arredond)
        
        #gerando a função que mostra o grau de pertencimento ao conjunto de
        #boas ações
        boa = TrapezoidalFuzzyNumber(p, q, r, s)
        
        output = c()
        for (ent in input) {
          # Cálculo do grau de pertinência
          membership_grade = ifelse(ent <= p | ent >= s, 0,
                                     ifelse(ent >=q & ent <= r, 1,
                                            ifelse(ent > p & ent < q,
                                                   (ent - p) / (q - p),
                                                   (s - ent) / (s - r))))
          
          # passando os valores para o output
          output = c(output, membership_grade)
        }
        
        #criando a nova linha do df res
        
        new_row = data.frame('indices' = colnames(df)[c], 'p' = p, 'q' = q, 'r' = r,
                             's' = s, "coef.corr" = cor(df[,col_num], output))
            
        #adicionando o valor encontrado
        df_list[[c]] = rbind(df_list[[c]], new_row)
            
      
        
        #########       continues of improvement       #########################
        ########################################################################
      } else {
        
        choice_path = sample(c('rnorm', 'runif'), 1,
                             prob = c(1 - round(prob_runif[c],2),
                                      round(prob_runif[c],2)))
        
        #pesos para a média ponderada
        pesos = abs(df_list[[c]][,'coef.corr']/sum(df_list[[c]][,'coef.corr']))
        
        ## selecionando aleatoriamente entre 'p' ou 's'
        choice_param = sample(c('p', 's'), 1)
        
        multidesv_times = 1/70 #multiplicador da fração entre as amplitudes
        
        p_col = df_list[[c]][,'p']
        q_col = df_list[[c]][,'q']
        r_col = df_list[[c]][,'r']
        s_col = df_list[[c]][,'s']
        
        #para evitar runifs com parametros menores ou maiores que o limite
        # definindo os limites para os parametros
        # from bottom to top
        limit_max = sort(input)[length(input) - length(input)*mean_lim_bottom]
        limit_min = sort(input)[(1-mean_lim_bottom)*length(input)]
        
        
        #verificando qual coeficiente foi escolhido
        if (choice_param == 'p'){
          #se foi 'p' gerando os parametros do menor para o maior
          
          #################     p       ##################################
          choice_path
          #verificando a escolha do caminho, runif ou rnorm
          if (choice_path == 'rnorm'){
            
            #####   verificando se o desvio padrão é igual a zero
            if (sd(p_col) == 0){
              if (mean(p_col) == 0){
                p = round(rnorm(1, sample(p_col, 1, prob = pesos),
                                diff(range(input))), arredond)
              } else {
                p = round(rnorm(1, sample(p_col, 1,prob = pesos),
                                sd = abs(mean(p_col))), arredond)
              }
            } else {
              desv_times = multidesv_times*diff(range(input))/diff(range(p_col))
              
              p = round(rnorm(1, mean = sample(p_col, 1, prob = pesos),
                              desv_times*sd(p_col)), arredond)
            }
            if (p < min(input)){
              p = min(input)
            } else if (p > max(input)){
              p = max(input)
            }
          } else {
            p = sample(sort(input)[1:match(limit_max,sort(input))], 1)
          }
          
          
          #################     q     ###################################
          choice_path
          #verificando a escolha do caminho, runif ou rnorm
          if (choice_path == 'rnorm'){
            #####   verificando se o desvio padrão é igual a zero
            if (sd(q_col) == 0){
              if (mean(q_col) == 0){
                q = round(rnorm(1, sample(q_col, 1, prob = pesos),
                                diff(range(input))), arredond)
              } else {
                q = round(rnorm(1, sample(q_col, 1, prob = pesos),
                                abs(mean(q_col))), arredond)
              }
            } else {
              desv_times = multidesv_times*diff(range(input))/diff(range(q_col))
              
              q = round(rnorm(1, sample( q_col, 1, prob = pesos),
                              desv_times*sd(q_col)), arredond)
            }
            if (q < p){
              q = p
            } else if (q > max(input)){
              q = max(input)
            }
          } else {
            q = ifelse(p>=limit_max,p,
        sample(sort(input)[match(p,sort(input)):length(input)], 1))
          }
          
          
          #################     r     ###################################
          choice_path
          #verificando a escolha do caminho, runif ou rnorm
          if (choice_path == 'rnorm'){
            #####   verificando se o desvio padrão é igual a zero
            if (sd(r_col) == 0){
              if (mean(r_col) == 0){
                r = round(rnorm(1, sample(r_col, 1, prob = pesos),
                                diff(range(input))), arredond)
              } else {
                r = round(rnorm(1, sample(r_col, 1, prob = pesos),
                                abs(mean(r_col))), arredond)
              }
            } else {
              desv_times = multidesv_times*diff(range(input))/diff(range(r_col))
              
              r = round(rnorm(1, sample(r_col, 1, prob = pesos),
                              desv_times*sd(r_col)), arredond)
            }
            if (r < q){
              r = q
            } else if (r > max(input)){
              r = max(input)
            }
          } else {
          r = sample(sort(input)[match(q,sort(input)):length(input)], 1)
            if (r < q){
              r = q
            }
          }
          
          
          #################     s     ###################################
          choice_path
          #verificando a escolha do caminho, runif ou rnorm
          if (choice_path == 'rnorm'){
            #####   verificando se o desvio padrão é igual a zero
            if (sd(s_col) == 0){
              if (mean(s_col) == 0){
                s = round(rnorm(1, sample(s_col, 1, prob = pesos),
                                diff(range(input))), arredond)
              } else {
                s = round(rnorm(1, sample(s_col, 1, prob = pesos),
                                abs(mean(s_col))), arredond)
              }
            } else {
              desv_times = multidesv_times*diff(range(input))/diff(range(s_col))
              
              s = round(rnorm(1, sample(s_col, 1, prob = pesos),
                              desv_times*sd(s_col)), arredond)
            }
            if (s < r){
              s = r
            } else if (s > max(input)){
              s = max(input)
            }
          } else {
            s = sample(sort(input)[match(r,sort(input)):length(input)], 1)
            if (s < r){
              s = r
            }
          } 
          
        } else {
          #se foi o 's' gerando os parametros do maior para o menor
          
          #################     s     ###################################
          choice_path
          #verificando a escolha do caminho, runif ou rnorm
          if (choice_path == 'rnorm'){
            #####   verificando se o desvio padrão é igual a zero
            if (sd(s_col) == 0){
              if (mean(s_col) == 0){
                s = round(rnorm(1, sample(s_col, 1, prob = pesos),
                                diff(range(input))), arredond)
              } else {
                s = round(rnorm(1, sample(s_col, 1, prob = pesos),
                                abs(mean(s_col))), arredond)
              }
            } else {
              desv_times = multidesv_times*diff(range(input))/diff(range(s_col))
              
              s = round(rnorm(1, sample(s_col, 1, prob = pesos),
                              desv_times*sd(s_col)), arredond)
            }
            if (s > max(input)){
              s = max(input)
            } else if (s < min(input)){
              s = min(input)
            }
          } else {
  s = round(sample(sort(input)[match(limit_min,sort(input)):length(input)], 1),
              arredond)
          }
          
          
          #################     r     ###################################
          choice_path
          #verificando a escolha do caminho, runif ou rnorm
          if (choice_path == 'rnorm'){
            #####   verificando se o desvio padrão é igual a zero
            if (sd(r_col) == 0){
              if (mean(r_col) == 0){
                r = round(rnorm(1, sample(r_col, 1, prob = pesos),
                                diff(range(input))), arredond)
              } else {
                r = round(rnorm(1, sample(r_col, 1, prob = pesos),
                                abs(mean(r_col))), arredond)
              }
            } else {
              desv_times = multidesv_times*diff(range(input))/diff(range(r_col))
              
              r = round(rnorm(1, sample(r_col, 1, prob = pesos),
                              desv_times*sd(r_col)), arredond)
            }
            if (r > s){
              r = s
            } else if (r < min(input)){
              r = min(input)
            }
          } else {
    r = ifelse(s<=limit_min,s,round(sample(sort(input)[1:match(s,sort(input))],
                                           1),arredond))
          }
          
          
          #################     q     ###################################
          choice_path
          #verificando a escolha do caminho, runif ou rnorm
          if (choice_path == 'rnorm'){
            #####   verificando se o desvio padrão é igual a zero
            if (sd(q_col) == 0){
              if (mean(q_col) == 0){
                q = round(rnorm(1, sample(q_col, 1, prob = pesos),
                                diff(range(input))), arredond)
              } else {
                q = round(rnorm(1, sample(q_col, 1, prob = pesos),
                                abs(mean(q_col))), arredond)
              }
            } else {
              desv_times = multidesv_times*diff(range(input))/diff(range(q_col))
              
              q = round(rnorm(1, sample(q_col, 1, prob = pesos),
                              desv_times*sd(q_col)), arredond)
            }
            if (q > r){
              q = r
            } else if (q < min(input)){
              q = min(input)
            }
          } else {
            q = round(sample(sort(input)[1:match(r,sort(input))], 1), arredond)
          }
          
          
          #################     p     ###################################
          choice_path
          #verificando a escolha do caminho, runif ou rnorm
          if (choice_path == 'rnorm'){
            #####   verificando se o desvio padrão é igual a zero
            if (sd(p_col) == 0){
              if (mean(p_col) == 0){
                p = round(rnorm(1, sample(p_col, 1, prob = pesos),
                                diff(range(input))), arredond)
              } else {
                p = round(rnorm(1, sample(p_col, 1, prob = pesos),
                                sd = abs(mean(p_col))), arredond)
              }
            } else {
              desv_times = multidesv_times*diff(range(input))/diff(range(p_col))
              
              p = round(rnorm(1, sample(p_col, 1, prob = pesos),
                              desv_times*sd(p_col)), arredond)
            }
            if (p > q){
              p = q
            } else if (p < min(input)){
              p = min(input)
            }
          } else {
            p = round(sample(sort(input)[1:match(q, sort(input))], 1), arredond)
          }
        }
        
        
        #gerando a função que mostra o grau de pertencimento ao conjunto de
        #boas ações
        boa = TrapezoidalFuzzyNumber(p, q, r, s)
        
        output = c()
        for (ent in input) {
          # Cálculo do grau de pertinência
          membership_grade = ifelse(ent <= p | ent >= s, 0,
                                    ifelse(ent >=q & ent <= r, 1,
                                           ifelse(ent > p & ent < q,
                                                  (ent - p) / (q - p),
                                                  (s - ent) / (s - r))))
          
          # passando os valores para o output
          output = c(output, membership_grade)
        }
        
        # condições para repetir a iteração
        if ((1 - mean_lim_bottom) < mean(output) ||
          mean(output) < mean_lim_bottom || is.na(cor(df[,col_num], output)) ||
          sd(df[,col_num]) == 0 || sd(output) == 0){
          next
        }
        
        #criando a possível nova linha do df res
        
        new_row = data.frame('indices' = colnames(df)[c], 'p' = p, 'q' = q, 'r' = r,
                             's' = s, "coef.corr" = cor(df[,col_num], output))
          
         #verificando se o coeficiente encontrado e maior que o mínimo
        if (cor(df[,col_num], output) > min(df_list[[c]][,"coef.corr"])){
          
          #encontrando a linha  do valor minimo do coeficiente
          j_min = match(min(df_list[[c]][,"coef.corr"]), df_list[[c]][,"coef.corr"])
          
          #removendo a linha do valor mínimo
          df_list[[c]] = df_list[[c]][-j_min,]
          
          #adicionando o valor encontrado
          df_list[[c]] = rbind(df_list[[c]], new_row)
          
          # recolocando o valor do prob_runif para 0.1
          prob_runif[c] = 0.1
          
          print(paste(colnames(df)[c], 'p, q, r, s: ', p, q, r, s))
          print(paste('media_coef: ', media_coef))
          print(Sys.time())
          print(paste("mean(output): ", mean(output)))
          
          plot(input, output, xlab = colnames(df)[c])
          plot(boa, xlab = colnames(df)[c])
        }
        
        else if (prob_runif[c] < 0.8){
            prob_runif[c] = prob_runif[c]+(0.05/accurate)
          }
      
      }
     
    media_all = c()
    for (d in df_list) {
      media_all = c(media_all, max(d[,'coef.corr']))
    }
    media_coef = mean(media_all)
    }
    c = ifelse(c==ncol(df)-1,1,c+1)
  }
  
  return(df_list[1:length(df_list)])
}
look_fuzzy_set(crit_tri[[6]], "v_price", 5, 2, 0.2)



