library(tidyverse)
library(gridExtra)


df_tri = read.csv("data/acoesb3.csv")

df_day = read.csv("data/acoesb3cot.csv")


#gridExtra::grid.table(df_day %>% slice(1:20)) # para plotar o db

ult_cot = cod = cotAtual = divY = c()
  
  for (i in 1:length(df_day[,"ultCot"])){
    if (df_day[i,"cod"] %in% cod){
      j = match(df_day[i,"cod"], cod)
        if (df_day[i,"ultCot"]>df_day[j,"ultCot"]){
          ult_cot = ult_cot[-j]; cod = cod[-j]; cotAtual = cotAtual[-j];
          divY = divY[-j]
          
          ult_cot = c(ult_cot, df_day[i,"ultCot"])
          cod = c(cod, df_day[i,"cod"])
          cotAtual = c(cotAtual, df_day[i,"cotAtual"])
          divY = c(divY, df_day[i, "divYield"])
        }
    } else {
      ult_cot = c(ult_cot, df_day[i,"ultCot"])
      cod = c(cod, df_day[i,"cod"])
      cotAtual = c(cotAtual, df_day[i,"cotAtual"])
      divY = c(divY, df_day[i, "divYield"])
    }
    
    
  }
  df_day_fil = data.frame(cod, cotAtual, divY, ult_cot)

  
  ult_bal = codigo = roic = cres_rec5 = n_ac = divb = disp = ativc = c();
  ativ = patl = recl12 = ebit12 = Lucl12 = recl3 = c()
  
  ult_cot_t = cotAtual_t = divY_t = c()
  for (i in 1:length(df_tri[,"ultBal"])){
    if (df_tri[i,"ultBal"] == "2022-09-30"){
      j = match(df_tri[i,"codigo"], cod)
      ult_cot_t = c(ult_cot_t, ult_cot[j])
      cotAtual_t = c(cotAtual_t, cotAtual[j])
      ult_bal =  c(ult_bal, df_tri[i,"ultBal"])
      codigo = c(codigo, df_tri[i,"codigo"])
      roic = c(roic, df_tri[i,"roic"])
      cres_rec5 = c(cres_rec5, df_tri[i,"cresRec5a"])
      divY_t = c(divY_t, divY[j])
      n_ac = c(n_ac, df_tri[i,"nAcoes"])
      divb = c(divb, df_tri[i,"divBruta"])
      disp = c(disp, df_tri[i,"disponib"])
      ativc = c(ativc, df_tri[i,"ativCirc"])
      ativ = c(ativ, df_tri[i,"ativos"])
      patl = c(patl, df_tri[i,"patLiq"])
      recl12 = c(recl12, df_tri[i,"recLiq12m"])
      ebit12 = c(ebit12, df_tri[i,"ebit12m"])
      Lucl12 = c(Lucl12, df_tri[i,"LucLiq12m"])
      recl3 = c(recl3, df_tri[i,"recLiq3m"])
  }
}
ult_bal_dates = c()
for (i in df_tri[,"ultBal"]){
  if (i %in% ult_bal_dates){}
  else{
    ult_bal_dates = c(ult_bal_dates, i)
  }
}
per = c()
for (i in length(df_tri[,"ultBal"])){
  j = match(df_tri[i,"ultBal"], ult_bal_dates)
    per = c(per,data.frame(ult_bal, codigo, roic, cres_rec5, n_ac, divb, disp,
                           ativc, ativ, patl, recl12, ebit12, Lucl12, recl3))
}

df_tri_3t22 = data.frame(ult_cot_t, cotAtual_t, ult_bal, codigo, roic, cres_rec5,
                         divY_t, n_ac, divb, disp,
                         ativc, ativ, patl, recl12, ebit12, Lucl12, recl3,
                         row.names = codigo)

#gridExtra::grid.table(df_tri_3t22 %>% slice(1:20))


pl = round(cotAtual_t/(Lucl12/n_ac), 2)
pv = round(cotAtual_t/(patl/n_ac), 2)
roe = round((Lucl12/patl)*100, 2)
roic = round(roic, 2)
p_cxa = round(cotAtual_t/(disp/n_ac), 2)
p_ativc = round(cotAtual_t/(ativc/n_ac), 2)
p_ativ = round(cotAtual_t/(ativ/n_ac), 2)
div_cxa = round(divb/disp, 2)
marg_ebit = round((ebit12/recl12)*100, 2)
marg_liq = round((Lucl12/recl12)*100, 2)
cres_rec5 = round(cres_rec5, 2)
divY_t = round(divY_t, 2)
lynch = round((divY_t+(cres_rec5/5))/pl, 2)
per_res = round(disp/(recl12/12 - Lucl12/12), 2)
div_lucm = round(divb/(Lucl12/12), 2)


crit = data.frame(pl ,pv ,roe ,roic ,p_cxa ,p_ativc ,p_ativ , div_cxa,
                  marg_ebit ,marg_liq ,cres_rec5 ,divY_t ,lynch ,per_res ,
                  div_lucm, row.names = codigo)


rmv_inf_values_row = function(df){
  for (r in 1:length(df[,1])){
    if (is.na(match(Inf, df[r,])) || is.na(match(Inf, df[r,]))){
      
    } else {
      df = df[-r,]
    }
    
  }
  return(df)
}

rmv_neg_pl = function(df){
  pl_neg = c()
  col_i = match("P/L",colnames(df))
  len = length(df[,col_i])
  for (r in 1:len){
    if (!is.na(df[r,col_i])){
      if (df[r,col_i] < 0){
        pl_neg = c(pl_neg,(df[r,"P/L"]))
      }
    }
  }
  
  for (i in pl_neg) {
    if (i %in% df[,col_i]){
      df = df[-match(i,df[,col_i]),]
    }
  }
  return(df)
}

rmv_neg_pvpa = function(df){
  pvpa_neg = c()
  col_i = match("P/VPA",colnames(df))
  len = length(df[,col_i])
  for (r in 1:len){
    if (!is.na(df[r,col_i])){
      if (df[r,col_i] < 0){
        pvpa_neg = c(pvpa_neg,df[r,"P/VPA"])
      }
    }
  }
  for (i in pvpa_neg) {
    if (i %in% df[,col_i]){
      df = df[-match(i,df[,col_i]),]
    }
  }
  return(df)
}

colnames(crit) = c("P/L", "P/VPA",
                   "ROE",
                   "ROIC",
                   "Preço/(Caixa/Ação)", "Preço/(Ativos Circulantes/Ação)",
                   "Preço/(Ativos/Ação)", "Dív Bruta/Caixa", "Mar. EBITDA",
                   "Marg. Líquida", "Cresc. Rec. (5 Anos)",
                   "Dividendyield", "Lynch",
                   "Per. Resistência",
                   "Dív. Bruta/Lucro Mensal")

crit = rmv_inf_values_row(crit)
length(crit[,1])
crit = rmv_neg_pl(crit)
length(crit[,1])
crit = rmv_neg_pvpa(crit)
length(crit[,1])


#gridExtra::grid.table(crit %>% slice(1:20))

apply(crit, 2, shapiro.test)

emp_lucpat_neg = c()
for(r in 1:length(df_tri_3t22[,1])){
  if(df_tri_3t22[r,'Lucl12'] < 0 && df_tri_3t22[r,'patl'] < 0){
    emp_lucpat_neg = c(emp_lucpat_neg, df_tri_3t22[r, 'codigo'])
    }
}

length(emp_lucpat_neg)
roe_err = c()
for (r in emp_lucpat_neg){
  roe_err = c(roe_err,(df_tri_3t22[r,"Lucl12"]/df_tri_3t22[r,'patl'])*100)
}

emp_lucpat_neg[match(max(roe_err), roe_err)] ## empresa com o maior roe falso


## retirando os roes falsos

length(crit[,1])

for (r in 1:length(crit$ROE)){
  if (row.names(crit)[r] %in% emp_lucpat_neg){
    print(row.names(crit)[r]) #empresa retirada (tinha passado pelos
                              #2 filtros anteriores)
    crit = crit[-r,]
  }
}

length(crit[,1])

boxplot(names = c("P/L", "P/VPA", "ROE", "ROIC", "P/(Cx/A)", "P/(Ativ Circ/A)",
                  "P/(Ativ/A)", "Dív Bruta/Cx", "Mar. EBIT", "Marg. Líq",
                  "Cresc.Rec.5A", "Dividendyield", "Lynch", "Per.Res",
                  "Dív.Br/Luc Mens"), crit, outline = T, range = 40)


## Retirando max(marg. liq)

row.names(crit)[match(max(crit$`Marg. Líquida`),crit$`Marg. Líquida`)]

crit = crit[-match(max(crit$`Marg. Líquida`), crit$`Marg. Líquida`),]

boxplot(names = c("P/L", "P/VPA", "ROE", "ROIC", "P/(Cx/A)", "P/(Ativ Circ/A)",
                  "P/(Ativ/A)", "Dív Bruta/Cx", "Mar. EBIT", "Marg. Líq",
                  "Cresc.Rec.5A", "Dividendyield", "Lynch", "Per.Res",
                  "Dív.Br/Luc Mens"), crit, outline = T, range = 40)


## retirando min(marg. ebitda)

row.names(crit)[match(min(crit$`Mar. EBITDA`),crit$`Mar. EBITDA`)]

crit = crit[-match(min(crit$`Mar. EBITDA`), crit$`Mar. EBITDA`),]

boxplot(names = c("P/L", "P/VPA", "ROE", "ROIC", "P/(Cx/A)", "P/(Ativ Circ/A)",
                  "P/(Ativ/A)", "Dív Bruta/Cx", "Mar. EBIT", "Marg. Líq",
                  "Cresc.Rec.5A", "Dividendyield", "Lynch", "Per.Res",
                  "Dív.Br/Luc Mens"), crit, outline = T, range = 40)

## retirando max(div/lucm)

row.names(crit)[match(max(crit$`Dív. Bruta/Lucro Mensal`),crit$`Dív. Bruta/Lucro Mensal`)]

crit = crit[-match(max(crit$`Dív. Bruta/Lucro Mensal`), crit$`Dív. Bruta/Lucro Mensal`),]



boxplot(names = c("pl", "pv", "roe", "roic", "p/cx", "p/at.c",
"p/at", "div.b/cx", "marg.eb", "marg.l", "cres.rec",
"dy", "ly", "p.res", "div.b/luc.m"), crit, outline = T,
range = 40)

rmv_outl = function(df){
  outlr = c()
  outlc = c()
  for (c in 1:length(df[1,])) {
    for (r in 1:length(df[,c])){
      q1p = (length(df[,c])+1)/4 ## regra 1
        if (q1p%%1!=0){
          if(q1p%%1%%0.5!=0){
            q1p = round(q1p,0) ## regra 3
            q1v = sort(df[,c])[q1p]
          } else {
            q1v = mean(sort(df[,c])[q1p-0.5],sort(df[,c])[q1p+0.5]) ## regra 2
          }
        }
      q1v = sort(df[,c])[q1p]
      q3p = 3*(length(df[,c])+1)/4 ## regra 1
      if (q3p%%1!=0){
        if(q3p%%1%%0.5!=0){
          q3p = round(q3p,0) ## regra 3
          q3v = sort(df[,c])[q3p]
        } else {
          q3v = mean(sort(df[,c])[q3p-0.5],sort(df[,c])[q3p+0.5]) ## regra 2
        }
      }
      q3v = sort(df[,c])[q3p]
      iqr = q3v - q1v
      rang = 20
      if (!is.na(df[r,c] - q3v > rang*iqr)&&!is.na(q1v - df[r,c] > rang*iqr)){
        if (df[r,c] - q3v > rang*iqr || q1v - df[r,c] > rang*iqr){
        outlr = c(outlr,row.names(df)[r])
        print(row.names(df)[r])
        outlc = c(outlc, colnames(df)[c])
        print(colnames(df)[c])
        print(df[r,c])
        }
      }
    }
  }
  outlr_nrep = c()
  for (i in 1:length(outlr)){
    if (outlr[i] %in% outlr_nrep){
      
    } else{
      outlr_nrep = c(outlr_nrep, outlr[i])
    }
  }
  for (cod in outlr_nrep){
    df = df[-match(cod, row.names(df)),]
  }
  return(df)
}

length(crit[,1])
crit = rmv_outl(crit)
length(crit[,1])

crit = rmv_outl(crit)
length(crit[,1])

crit = rmv_outl(crit)
length(crit[,1])

crit = rmv_outl(crit)
length(crit[,1])

crit = rmv_outl(crit)
length(crit[,1])


boxplot(names = c("P/L", "P/VPA", "ROE", "ROIC", "P/(Cx/A)", "P/(Ativ Circ/A)",
                  "P/(Ativ/A)", "Dív Bruta/Cx", "Mar. EBIT", "Marg. Líq",
                  "Cresc.Rec.5A", "Dividendyield", "Lynch", "Per.Res",
                  "Dív.Br/Luc Mens"), crit, outline = T, range = 4)


nor = function(vec){
  vecn = c()
  for (i in vec){
    vecn = c(vecn, (i - min(vec))/(max(vec)-min(vec)))
  }
  return(vecn)
}
