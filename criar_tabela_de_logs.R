library(tidyverse)
library(gridExtra)
library(ggplot2)
library(readxl)

setwd("C:/files/projects/programacao/python/acoes_data/")

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

################################################################################
########     função para criar a tabela de registros de resultados    ##########
################################################################################
#separando os df por períodos

criar_tab = function(qtd_dias, samp_range, accurate, range_per){
  
  tab_log = data.frame(matrix(ncol = 6)) ## data.frame para conter as colunas
  ## da tabela resultados
  colnames(tab_log) = c('data e hora', 'media_coef', 'num_row', 'accurate',
                        'per_i', 'qtd_dias') ## mesma qtd de colunas
  
  for (d in 1:qtd_dias) {
    
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
            if (as.Date((df_day[i,'ultCot'])) <= as.Date(ultIns)[j_dem]+d){
              
              # verificando se o valor da ultima cotação da iteração atual é maior que
              #o valor pertencente à variável e menor ou igual a data de inserção
              if (as.Date(df_day[i,"ultCot"]) > as.Date(df_day[j,"ultCot"]) &&
                  as.Date(df_day[i,'ultCot']) <= as.Date(ultIns[j_dem])){
                
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
      
      
      
      
      ###################   end att 2023/11/07 19:36    ############################## 
      
      
      #gridExtra::grid.table(df_tri_3t22 %>% slice(1:20))
      
      arr_ind = 3 #casas decimais dos indices
      
      # criando os índices
      pl = round(cotAtual_t/(Lucl12/n_ac_t), arr_ind)
      pl3 = round(cotAtual_t/(as.numeric(Lucl3)/n_ac_t), arr_ind)
      lp = round((Lucl12/n_ac_t)/cotAtual_t, arr_ind)
      lp3 = round((as.numeric(Lucl3)/n_ac_t)/cotAtual_t, arr_ind)
      vp = round((patl/n_ac_t)/cotAtual_t, arr_ind)
      roe = round((Lucl12/patl)*100, arr_ind)
      roe3 = round((as.numeric(Lucl3)/patl)*100, arr_ind)
      roic = round(as.numeric(roic), arr_ind)
      cxa_p = round((disp/n_ac_t)/cotAtual_t, arr_ind)
      ativc_p = round((ativc/n_ac_t)/cotAtual_t, arr_ind)
      ativ_p = round((ativ/n_ac_t)/cotAtual_t, arr_ind)
      div_cxa = round(divb/disp, arr_ind)
      marg_ebit = round((ebit12/recl12)*100, arr_ind)
      marg_ebit3 = round((as.numeric(ebit3)/recl3)*100, arr_ind)
      marg_liq = round((Lucl12/recl12)*100, 2)
      marg_liq3 = round((as.numeric(Lucl3)/recl3)*100, arr_ind)
      cres_rec5 = round(as.numeric(cres_rec5), arr_ind)
      divY_t = round(as.numeric(divY_t), arr_ind)
      lynch = round((divY_t+(cres_rec5/5))/pl, arr_ind)
      lynch3 = round((divY_t/3+(cres_rec5/15))/pl3, arr_ind)
      div_lucm = round(divb/(as.numeric(Lucl3)/3), arr_ind)
      psr_inv = round((recl12/n_ac_t)/cotAtual_t, arr_ind)
      psr_inv3 = round((as.numeric(recl3)/n_ac_t)/cotAtual_t, arr_ind)
      ebit_p = round((ebit12/n_ac_t)/cotAtual_t, arr_ind)
      ebit_p3 = round((as.numeric(ebit3)/n_ac_t), arr_ind)
      ebit_ativ = round(ebit12/ativ, arr_ind)
      ebit_ativ3 = round((as.numeric(ebit3)/ativ), arr_ind)
      divb_pat = round(divb/patl, arr_ind)
      
      
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
    
    #########       função para retornar as variações dos índices     ##############
    #########         de um período anterior a um especificado        ##############
    ################################################################################
    
    #per_i => periodo a ser analisado
    varInd = function(per_i){
      
      var_price = c()
      
      for (r in 1:nrow(crit_tri[[per_i]])) {
        
        # encontrando a posição da linha do codigo da iteração no df das demonstrações
        j_dem = match(row.names(crit_tri[[per_i]])[r], row.names(trimestres[[per_i]]))
        
        # encontrando a posição da linha do codigo da iteração no df dos criterios
        # do período anterior
        j_dem_bef = match(row.names(crit_tri[[per_i]])[r], row.names(crit_tri[[per_i-1]]))
        
        # criando o df das variacoes dos indices
        for (ind in 1:ncol(crit_tri[[per_i]]))
          if (!is.na(j_dem_bef)){
            crit_tri[[per_i]][r,ind] = (crit_tri[[per_i]][r,ind] -
                                          crit_tri[[per_i-1]][j_dem_bef,ind])/abs(crit_tri[[per_i-1]][j_dem_bef,ind])
            
            #arredondando
            crit_tri[[per_i]][r, ind] = round(crit_tri[[per_i]][r, ind], 4)
          } else {
            crit_tri[[per_i]][r, ind] = NA
          }
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
      
      arredond = 4  # qtd de casa decimais para os parametros
      
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
      
      return(media_coef)
      
      
    }  
    
    num_row = 10
    for (qtd_per in range_per) {
      for (s in 1:samp_range) {
        new_row = c(paste('',Sys.time(),''), look_fuzzy_set(varInd(qtd_per), 'v_price', num_row, accurate, 0.2),
                  num_row, accurate,  qtd_per, d)
        tab_log = rbind(tab_log, new_row)
        View(tab_log)
      }
    }
  }
  
  ###########         passando o tab_log para uma planilha         #############
  library(openxlsx)
  write.xlsx(tab_log,
             file = 'C:/files/projects/programacao/python/acoes_data/logs/tab_log.xlsx')

}

criar_tab(30, 5, 1, range_per = 4:7)

