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


############################################################################
###############    função para retirar valores extremos    #################
############################################################################

# remover valores extremos
rmv_wild = function(df, fator_multiplicativo) {
  
  exib_wild = function(dados, fator_multiplicativo){
    # Obter estatísticas do boxplot
    stats <- boxplot.stats(dados)
    
    # Calcular limites personalizados
    limite_inferior <-
      stats$stats[2] - fator_multiplicativo * IQR(dados, na.rm = T)
    limite_superior <-
      stats$stats[4] + fator_multiplicativo * IQR(dados, na.rm = T)
    
    # Identificar outliers
    outliers <- dados[dados < limite_inferior | dados > limite_superior]
    
    # Exibir os valores dos outliers
    return(outliers)  
  }
  
  wild_row = c('so', 'para','length', 'nao', 'ser', 'zero')
  
  while (length(wild_row) != 0) {
    wild_row = c()
    if (is.null(ncol(df))){
      if (length(exib_wild(df, fator_multiplicativo)) != 0){
        for (i in exib_wild(df, fator_multiplicativo)) {
          #numero no df
          j = match(i, df)
          if (df[j] %in% wild_row){} else {
            wild_row = c(wild_row, df[j])
          }
          df[j] = NA
        }
      }
      rmv_na_val = function(df) {
        if (any(!complete.cases(df))){
          df = df[complete.cases(df)]
        }
        return(df)
      }
      
    } else {
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
    
    }
    
    
    df = rmv_na_val(df)  
  }
  
  return(df)
}

#######   função para mudar um vetor para apenas valores positivos    ##########
trans_only_pos = function(vec) {
  out = c()
  for(i in 1:length(vec)){ out = c(out, vec[i]+abs(min(vec)))}
  
  # verificando a veracidade dessa função
  if (cor(out, vec) != 1){
    stop('error: correlação entre entre entrada e saída não é 1')
  }
  if (min(out) != 0){
    stop('error: minimo da saída não é igual a 0')
  }
  if (max(out) != diff(range(vec))){
    stop('error: maximo da saida não é igual a amplitude da entrada')
  }
  if (sd(vec) != sd(out)){
    stop('error: desvios padrões da entrada e saida são diferentes')
  }
  
  return(out)
  }

################################################################################
########         função para retornar o ccp sem os outliers        #############
################################################################################

ccp_sem_out = function(varx, vary, fator_multiplicativo){
  if (length(varx) != length(vary)){
    return('os comprimentos das variáveis devem ser iguais')
  }
  vec1 = varx
  vec2 = vary
  for (x in 1:length(vec1)) {
    if (!(varx[x] %in% rmv_wild(vec1, fator_multiplicativo)) ||
        !(vary[x] %in% rmv_wild(vec2, fator_multiplicativo))) {
      varx[x] = NA; vary[x] = NA  
    }
  }
  
  rmv_na_val = function(df) {
    if (any(!complete.cases(df))){
      df = df[complete.cases(df)]
    }
    return(df)
  }
  return(cor(rmv_na_val(varx), rmv_na_val(vary)))
  
}

################################################################################
########         função para escrever tabela em uma planilha         ###########
################################################################################

##    Argumentos
# 1 - file name
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
    return(write.xlsx(df, file = paste(fpath, fil_nam, sep = '')))
  }
  if (extension == '.csv'){
    return(write.csv(df, file = paste(fpath, fil_nam, sep = '')))
  }
}

################################################################################
########     função para criar a tabela de registros de resultados    ##########
################################################################################

criar_tab = function(qtd_dias, range_per, fator_multiplicativo){
  
  tab_log = data.frame(matrix(ncol = 5)) ## data.frame para conter as colunas
  ## da tabela resultados
  colnames(tab_log) = c('data e hora', 'media_coef',
                        'per_i', 'qtd_dias',
                        'fator_multiplicatovo')## mesma qtd de colunas
  
  for (d in 1:qtd_dias) {
    
    #lista para conter os períodos
    trimestres = list()
    
    #lista para conter os critérios por períodos
    crit_tri = list()
    
    #criando os valores dos critérios
    for (per in 1:length(ult_bal_dates)){
      print(paste('Criando o DF para o periodo: ', ult_bal_dates[per]))
      
      ###################   start att 2023/11/07 19:36    ######################
      
      #Criando as variáveis do db trimestral
      ult_bal = codigo = roic = cres_rec5 = divb = disp = ativc =
      ativ = patl = recl12 = ebit12 = Lucl12 = recl3 = ebit3 = Lucl3 =
      ultIns = c()
      
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
                              ebit12, Lucl12, recl3, ebit3, Lucl3,
                              ultIns, row.names = codigo)
      
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
                divY = divY[-j]; n_ac = n_ac[-j]; cotF = cotF[-j];
                n_acF = n_acF[-j]
                
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
      
      
      
      
      ###################   end att 2023/11/07 19:36    ########################
      
      
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
      
      #############       filtrando o db        ################################
      
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
      
      ###############     chatgpt  end     ##############################
      

      ####       procurando indices falsos, negativo sobre negativo       ######
      emp_lucpat_neg = emp_lucpat_neg3 = emp_lucrec_neg = emp_lucrec_neg3 =
        emp_ebitrec_neg = emp_ebitrec_neg3 = c()
      if (nrow(crit) > 0){
        for(lin in 1:nrow(crit)){
          r = match(row.names(crit)[lin], row.names(df_tri_dem))
          
          # verificando os ROEs falsos
          if(df_tri_dem[r,'Lucl12'] < 0 && df_tri_dem[r,'patl'] < 0){
            emp_lucpat_neg = c(emp_lucpat_neg, row.names(df_tri_dem)[r])
          }
          
          # verificando os ROE(tri)s falsos
          if(as.numeric(df_tri_dem[r,"Lucl3"]) < 0 && df_tri_dem[r,'patl'] < 0){
            emp_lucpat_neg3 = c(emp_lucpat_neg3, row.names(df_tri_dem)[r])
          }
          
          # verificando os marg.liqs falsos
          if(df_tri_dem[r,'Lucl12'] < 0 && df_tri_dem[r,'recl12'] < 0){
            emp_lucrec_neg = c(emp_lucrec_neg, row.names(df_tri_dem)[r])
          }
          
          # verificando os marg.liq(tri)s falsos
          if(as.numeric(df_tri_dem[r,"Lucl3"]) < 0 &&
             df_tri_dem[r,"recl3"] < 0){
            emp_lucrec_neg3 = c(emp_lucrec_neg3, row.names(df_tri_dem)[r])
          }
          
          # verificando os marg.ebit falsos
          if(df_tri_dem[r,'ebit12'] < 0 && df_tri_dem[r,'recl12'] < 0){
            emp_ebitrec_neg = c(emp_ebitrec_neg, row.names(df_tri_dem)[r])
          }
          
          # verificando os marg.ebit(tri) falsos
          if(as.numeric(df_tri_dem[r,'ebit3']) < 0 &&
             df_tri_dem[r,'recl3'] < 0){
            emp_ebitrec_neg3 = c(emp_ebitrec_neg3, row.names(df_tri_dem)[r])
          }
        }
      }
      
      
      #empresas com ROE falso
      roe_err = c()
      for (r in emp_lucpat_neg){
        roe_err = c(roe_err,(df_tri_dem[r,"Lucl12"]/df_tri_dem[r,'patl'])*100)
      }
      print(paste('qtd de empresas com roes falsos: ', length(roe_err)))
      print(paste('empresa com o maior roe falso: ',
                  emp_lucpat_neg[match(max(roe_err), roe_err)]))
      
      #empresas com ROEtri falso
      roe_err_tri = c()
      for (r in emp_lucpat_neg3){
        roe_err_tri = c(roe_err_tri,
                 (as.numeric(df_tri_dem[r,"Lucl3"])/df_tri_dem[r,'patl'])*100)
      }
      print(paste("qtd de empresas com roe_tri's falsos: ",
                  length(roe_err_tri)))
      print(paste('empresa com o maior roe_tri falso: ',
                  emp_lucpat_neg3[match(max(roe_err_tri), roe_err_tri)]))
      
      #empresas com marg. liq. falso
      marliq_err = c()
      for (r in emp_lucrec_neg){
        marliq_err = c(marliq_err,
                       (df_tri_dem[r,"Lucl12"]/df_tri_dem[r,'recl12'])*100)
      }
      print(paste('qtd de empresas com mar.liqs falsos: ', length(marliq_err)))
      print(paste('empresa com o maior mar.liq falso: ',
                  emp_lucrec_neg[match(max(marliq_err), marliq_err)]))
      
      #empresas com marg. liq.tri falso
      marliq_err_tri = c()
      for (r in emp_lucrec_neg3){
        marliq_err_tri = c(marliq_err_tri,
                (as.numeric(df_tri_dem[r,"Lucl3"])/df_tri_dem[r,'recl3'])*100)
      }
      print(paste('qtd de empresas com mar.liq_tris falsos: ',
                  length(marliq_err_tri)))
      print(paste('empresa com o maior mar.liq_tri falso: ',
                  emp_lucrec_neg3[match(max(marliq_err_tri), marliq_err_tri)]))
      
      #empresas com marg. ebit. falso
      margebit_err = c()
      for (r in emp_lucrec_neg){
        margebit_err = c(margebit_err,
                         (df_tri_dem[r,"ebit12"]/df_tri_dem[r,'recl12'])*100)
      }
      print(paste('qtd de empresas com marg.ebit falsos: ',
                  length(margebit_err)))
      print(paste('empresa com o maior marg.ebit falso: ',
                  emp_ebitrec_neg[match(max(margebit_err), margebit_err)]))
      
      #empresas com marg. ebit.(tri) falso
      margebit_err_tri = c()
      for (r in emp_lucrec_neg3){
        margebit_err_tri = c(margebit_err_tri,
                (as.numeric(df_tri_dem[r,"ebit3"])/df_tri_dem[r,'recl3'])*100)
      }
      print(paste('qtd de empresas com marg.ebit_tri falsos: ',
                  length(margebit_err_tri)))
      print(paste('empresa com o maior marg.ebit_tri falso: ',
                  emp_ebitrec_neg3[match(max(margebit_err_tri),
                                         margebit_err_tri)]))
      
      
      
      ## retirando os indices falsos
      
      print(paste('número de empresas antes da retirada dos indices falsos: ',
                  nrow(crit)))
      
      
      for (r in 1:nrow(crit)){
        if (length(row.names(crit)[r]) > 0){
          if (length(emp_lucpat_neg) > 0 &&
              row.names(crit)[r] %in% emp_lucpat_neg)
          {
            crit[r,] = NA
          }
          if (length(emp_lucpat_neg3) > 0 &&
              row.names(crit)[r] %in% emp_lucpat_neg3)
          {
            crit[r,] = NA
          }
          if (length(emp_lucrec_neg) > 0 &&
              row.names(crit)[r] %in% emp_lucrec_neg){
            crit[r,] = NA
          }
          if (length(emp_lucrec_neg3) > 0 &&
              row.names(crit)[r] %in% emp_lucrec_neg3){
            crit[r,] = NA
          }
          if (length(emp_ebitrec_neg) > 0 &&
              row.names(crit)[r] %in% emp_ebitrec_neg){
            crit[r,] = NA
          }
          if (length(emp_ebitrec_neg3) > 0 &&
              row.names(crit)[r] %in% emp_ebitrec_neg3){
            crit[r,] = NA
          }
        }
      }
      
      crit = rmv_na_val(crit)
      
      print(paste('número de empresas depois da retirada dos indices falsos: ',
                  nrow(crit)))
      
      
      #passando o df para a lista
      trimestres[[per]] = df_tri_dem
      
      
      colnames(crit) =
        c("L/P", 'L/P (tri)', "VPA/P", "ROE" , 'ROE (tri)', "ROIC",
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
    
    
    
    
    #########       função para retornar as variações dos índices     ##########
    #########         de um período anterior a um especificado        ##########
    ############################################################################
    
    #per_i => periodo a ser analisado
    varInd = function(per_i){
      
      var_price = c()
      
      for (r in 1:nrow(crit_tri[[per_i]])) {
        
  # encontrando a posição da linha do codigo da iteração no df das demonstrações
        j_dem =
          match(row.names(crit_tri[[per_i]])[r], row.names(trimestres[[per_i]]))
        
  # encontrando a posição da linha do codigo da iteração no df dos criterios
  # do período anterior
        j_dem_bef =
          match(row.names(crit_tri[[per_i]])[r], row.names(crit_tri[[per_i-1]]))
        
        # criando o df das variacoes dos indices
        for (ind in 1:ncol(crit_tri[[per_i]]))
          if (!is.na(j_dem_bef)){
            crit_tri[[per_i]][r,ind] =
              (crit_tri[[per_i]][r,ind] -
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
      
      colnames(crit_tri[[per_i]]) =
        c("L/P", 'L/P (tri)', "VPA/P", "ROE" , 'ROE (tri)', "ROIC",
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
    
    
    #########     procurando as melhores correlações      #####################
    
    # a função a seguir precisa de 6 argumentos:
    # 1 - data.frame para ser encontrando as funções trapezoidais
    # 2 - coluna objetivo, para ser comparada com as outras
    
      look_fubest_corr = function(df, col_obj) {
        df = rmv_wild(df, fator_multiplicativo)
      if (ncol(df) < 2) {
        return('data.frame não tem colunas suficientes')
      }
      
      if (nrow(df) <= 1){
        return('data.frame não tem linhas suficientes')
      }
      
      col_num = match(col_obj, colnames(df))
      output = df[,col_num]
      
      
      if (any(is.na(df[, col_num]))) {
        return("A coluna objetivo contém valores ausentes.")
      } else {
        sd_value <- sd(df[, col_num], na.rm = TRUE)
        
        if (is.finite(sd_value)) {
          if (sd_value == 0) {
            return("O desvio padrão da coluna objetivo é igual a zero.")
          }
          
        } else {
          return("O desvio padrão não é um número finito.")
        }
      }
      
      
      # lista para conter os df com os valores das corr
      df_list = list()
      for (c in 1:ncol(df)){
        if (colnames(df)[c] != col_obj){
          df_list[[c]] = data.frame(matrix(ncol = 2, nrow = 0))
          colnames(df_list[[c]]) = c("indices", "coef.corr")
          
        }
      }
      
      
      ## criando o indice de coluna
      
      for (c in 1:ncol(df)) {
        
        if (colnames(df)[c] != col_obj){
          input = df[,c]
          
          if (diff(range(input)) == 0){
            stop(paste('error: coluna ', colnames(df)[c],
                       'tem amplitude igual a zero, sem desvio padrão'))
          }
          
          
            
            #criando a nova linha do df res
            
            new_row = data.frame('indices' = colnames(df)[c],
                                 "coef.corr" = abs(cor(input, output)))
              
              #adicionando o valor encontrado
              df_list[[c]] = rbind(df_list[[c]], new_row)
              
              plot(input, output, xlab = colnames(df)[c])

          
          media_all = c()
          for (d in df_list) {
            media_all = c(media_all, d[1,'coef.corr'])
          }
          media_coef = mean(media_all)
        }
        
      }
      
      return(media_coef)
      
      
    }  
    for (qtd_per in range_per) {
      
      for (fator_multiplicativo in seq(2, fac_max, 0.5)) {
        
        new_row = c(paste('',format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),''),
look_fubest_corr(varInd(qtd_per),
               colnames(varInd(qtd_per))[length(varInd(qtd_per))]),qtd_per, d,
               fator_multiplicativo)
        if (any(is.na(tab_log[1,]), na.rm = F)){
          tab_log[1,] = new_row
          View(tab_log)
        } else{
          tab_log = rbind(tab_log, new_row)
          View(tab_log)  
        }
        
      }
        
    }
  }
  
  ###########         passando o tab_log para uma planilha         #############
  
  ##    Argumentos
  # 1 - file name
  # 2 - file path
  # 3 - extensão do arquivo (.alguma_coisa)
  
  escrever_res(tab_log,
               'C:/files/projects/programacao/python/acoes_data/logs/',
               '.csv',
               'fuzzy_corr_log')

}

#variaveis da função
# 1 - quantidades máximas de dias de variação do preço
# 4 - vetor com os períodos que serão analizados
# 5 - fator máximo que ira multiplicar o iqr para a retirada de outliers

criar_tab(15, range_per = 4:7, 5)

