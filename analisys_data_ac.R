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

for (c in 1:length(df_tri_3t22[1,])){
  for (r in 1:length(df_tri_3t22[,1])){
    if (df_tri_3t22[r,c]<0){
      print(c(row.names(df_tri_3t22)[r],colnames(df_tri_3t22)[c]))
    }
  }
}

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
div_lucm = round(divb/(Lucl12/12), 2)


crit = data.frame(pl ,pv ,roe ,roic ,p_cxa ,p_ativc ,p_ativ , div_cxa,
                  marg_ebit ,marg_liq ,cres_rec5 ,divY_t ,lynch,
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
                  "Cresc.Rec.5A", "Dividendyield", "Lynch",
                  "Dív.Br/Luc Mens"), crit, outline = T, range = 5500)


## Retirando max(marg. liq)

row.names(crit)[match(max(crit$`Marg. Líquida`),crit$`Marg. Líquida`)]

crit = crit[-match(max(crit$`Marg. Líquida`), crit$`Marg. Líquida`),]

boxplot(names = c("P/L", "P/VPA", "ROE", "ROIC", "P/(Cx/A)", "P/(Ativ Circ/A)",
                  "P/(Ativ/A)", "Dív Bruta/Cx", "Mar. EBIT", "Marg. Líq",
                  "Cresc.Rec.5A", "Dividendyield", "Lynch",
                  "Dív.Br/Luc Mens"), crit, outline = T, range = 700)


## retirando min(marg. ebitda)

row.names(crit)[match(min(crit$`Mar. EBITDA`),crit$`Mar. EBITDA`)]

crit = crit[-match(min(crit$`Mar. EBITDA`), crit$`Mar. EBITDA`),]

boxplot(names = c("P/L", "P/VPA", "ROE", "ROIC", "P/(Cx/A)", "P/(Ativ Circ/A)",
                  "P/(Ativ/A)", "Dív Bruta/Cx", "Mar. EBIT", "Marg. Líq",
                  "Cresc.Rec.5A", "Dividendyield", "Lynch",
                  "Dív.Br/Luc Mens"), crit, outline = T, range = 70)

## retirando max(P/L)

row.names(crit)[match(max(crit$`P/L`),crit$`P/L`)]

crit = crit[-match(max(crit$`P/L`), crit$`P/L`),]

boxplot(names = c("pl", "pv", "roe", "roic", "p/cx", "p/at.c",
                  "p/at", "div.b/cx", "marg.eb", "marg.l", "cres.rec",
                  "dy", "ly", "div.b/luc.m"), crit, outline = T,
        range = 50)

## retirando max(Div/Lucm)

row.names(crit)[match(max(crit$`Dív. Bruta/Lucro Mensal`),crit$`Dív. Bruta/Lucro Mensal`)]

crit = crit[-match(max(crit$`Dív. Bruta/Lucro Mensal`), crit$`Dív. Bruta/Lucro Mensal`),]

boxplot(names = c("pl", "pv", "roe", "roic", "p/cx", "p/at.c",
                  "p/at", "div.b/cx", "marg.eb", "marg.l", "cres.rec",
                  "dy", "ly", "div.b/luc.m"), crit, outline = T,
        range = 30)

marg.l = crit$`Marg. Líquida`
marg.l_rmv_max = c()
for (i in sort(marg.l)){
  if (i != max(marg.l)){
    marg.l_rmv_max = c(marg.l_rmv_max,i)
  }
}
rng_max_mrg_l_s = c(max(marg.l),max(marg.l_rmv_max))
range(rng_max_mrg_l_s); range(marg.l_rmv_max)
(range(rng_max_mrg_l_s)[2]-range(rng_max_mrg_l_s)[1])/(range(marg.l_rmv_max)[2]-range(marg.l_rmv_max)[1]) ## > 1

## retirando max(marg.l)

row.names(crit)[match(max(crit$`Marg. Líquida`),crit$`Marg. Líquida`)]

crit = crit[-match(max(crit$`Marg. Líquida`), crit$`Marg. Líquida`),]

boxplot(names = c("pl", "pv", "roe", "roic", "p/cx", "p/at.c",
                  "p/at", "div.b/cx", "marg.eb", "marg.l", "cres.rec",
                  "dy", "ly", "div.b/luc.m"), crit, outline = T,
        range = 26)

## retirando max(P/L)

row.names(crit)[match(max(crit$`P/L`),crit$`P/L`)]

crit = crit[-match(max(crit$`P/L`), crit$`P/L`),]

boxplot(names = c("pl", "pv", "roe", "roic", "p/cx", "p/at.c",
                  "p/at", "div.b/cx", "marg.eb", "marg.l", "cres.rec",
                  "dy", "ly", "div.b/luc.m"), crit, outline = T,
        range = 23)

## retirando max(P/cxa)

row.names(crit)[match(max(crit$`Preço/(Caixa/Ação)`),crit$`Preço/(Caixa/Ação)`)]

crit = crit[-match(max(crit$`Preço/(Caixa/Ação)`), crit$`Preço/(Caixa/Ação)`),]

boxplot(names = c("pl", "pv", "roe", "roic", "p/cx", "p/at.c",
                  "p/at", "div.b/cx", "marg.eb", "marg.l", "cres.rec",
                  "dy", "ly", "div.b/luc.m"), crit, outline = T,
        range = 22)

## retirando max(P/L)

row.names(crit)[match(max(crit$`P/L`),crit$`P/L`)]

crit = crit[-match(max(crit$`P/L`), crit$`P/L`),]

boxplot(names = c("pl", "pv", "roe", "roic", "p/cx", "p/at.c",
                  "p/at", "div.b/cx", "marg.eb", "marg.l", "cres.rec",
                  "dy", "ly", "div.b/luc.m"), crit, outline = T,
        range = 21.725)

## retirando max(ly)

row.names(crit)[match(max(crit$Lynch),crit$Lynch)]

crit = crit[-match(max(crit$Lynch), crit$Lynch),]

boxplot(names = c("pl", "pv", "roe", "roic", "p/cx", "p/at.c",
                  "p/at", "div.b/cx", "marg.eb", "marg.l", "cres.rec",
                  "dy", "ly", "div.b/luc.m"), crit, outline = T,
        range = 21)

## retirando max(P/L)

row.names(crit)[match(max(crit$`P/L`),crit$`P/L`)]

crit = crit[-match(max(crit$`P/L`), crit$`P/L`),]

boxplot(names = c("pl", "pv", "roe", "roic", "p/cx", "p/at.c",
                  "p/at", "div.b/cx", "marg.eb", "marg.l", "cres.rec",
                  "dy", "ly", "div.b/luc.m"), crit, outline = T,
        range = 20)

## retirando max(ly)

row.names(crit)[match(max(crit$Lynch),crit$Lynch)]

crit = crit[-match(max(crit$Lynch), crit$Lynch),]

boxplot(names = c("pl", "pv", "roe", "roic", "p/cx", "p/at.c",
                  "p/at", "div.b/cx", "marg.eb", "marg.l", "cres.rec",
                  "dy", "ly", "div.b/luc.m"), crit, outline = T,
        range = 19)

## retirando max(ly)

row.names(crit)[match(max(crit$Lynch),crit$Lynch)]

crit = crit[-match(max(crit$Lynch), crit$Lynch),]

boxplot(names = c("pl", "pv", "roe", "roic", "p/cx", "p/at.c",
                  "p/at", "div.b/cx", "marg.eb", "marg.l", "cres.rec",
                  "dy", "ly", "div.b/luc.m"), crit, outline = T,
        range = 17.5)

## retirando max(cres.rec)

row.names(crit)[match(max(crit$`Cresc. Rec. (5 Anos)`), crit$`Cresc. Rec. (5 Anos)`)]

crit = crit[-match(max(crit$`Cresc. Rec. (5 Anos)`), crit$`Cresc. Rec. (5 Anos)`),]

boxplot(names = c("pl", "pv", "roe", "roic", "p/cx", "p/at.c",
                  "p/at", "div.b/cx", "marg.eb", "marg.l", "cres.rec",
                  "dy", "ly", "div.b/luc.m"), crit, outline = T,
        range = 16)


## retirando max(marg.l)

row.names(crit)[match(max(crit$`Marg. Líquida`),crit$`Marg. Líquida`)]

crit = crit[-match(max(crit$`Marg. Líquida`), crit$`Marg. Líquida`),]

boxplot(names = c("pl", "pv", "roe", "roic", "p/cx", "p/at.c",
                  "p/at", "div.b/cx", "marg.eb", "marg.l", "cres.rec",
                  "dy", "ly", "div.b/luc.m"), crit, outline = T,
        range = 15)


## retirando max(P/cx)

row.names(crit)[match(max(crit$`Preço/(Caixa/Ação)`),crit$`Preço/(Caixa/Ação)`)]

crit = crit[-match(max(crit$`Preço/(Caixa/Ação)`), crit$`Preço/(Caixa/Ação)`),]

boxplot(names = c("pl", "pv", "roe", "roic", "p/cx", "p/at.c",
                  "p/at", "div.b/cx", "marg.eb", "marg.l", "cres.rec",
                  "dy", "ly", "div.b/luc.m"), crit, outline = T,
        range = 12.5)


## retirando max(marg.ebit)

row.names(crit)[match(max(crit$`Mar. EBITDA`),crit$`Mar. EBITDA`)]

crit = crit[-match(max(crit$`Mar. EBITDA`), crit$`Mar. EBITDA`),]

boxplot(names = c("pl", "pv", "roe", "roic", "p/cx", "p/at.c",
                  "p/at", "div.b/cx", "marg.eb", "marg.l", "cres.rec",
                  "dy", "ly", "div.b/luc.m"), crit, outline = T,
        range = 12.5)


## retirando max(marg.liq)

row.names(crit)[match(max(crit$`Marg. Líquida`),crit$`Marg. Líquida`)]

crit = crit[-match(max(crit$`Marg. Líquida`), crit$`Marg. Líquida`),]

boxplot(names = c("pl", "pv", "roe", "roic", "p/cx", "p/at.c",
                  "p/at", "div.b/cx", "marg.eb", "marg.l", "cres.rec",
                  "dy", "ly", "div.b/luc.m"), crit, outline = T,
        range = 12)

## retirando max(cres.rec)

row.names(crit)[match(max(crit$`Cresc. Rec. (5 Anos)`), crit$`Cresc. Rec. (5 Anos)`)]

crit = crit[-match(max(crit$`Cresc. Rec. (5 Anos)`), crit$`Cresc. Rec. (5 Anos)`),]

boxplot(names = c("pl", "pv", "roe", "roic", "p/cx", "p/at.c",
                  "p/at", "div.b/cx", "marg.eb", "marg.l", "cres.rec",
                  "dy", "ly", "div.b/luc.m"), crit, outline = T,
        range = 11)

## retirando max(pv)

row.names(crit)[match(max(crit$`P/VPA`), crit$`P/VPA`)]

crit = crit[-match(max(crit$`P/VPA`), crit$`P/VPA`),]

boxplot(names = c("pl", "pv", "roe", "roic", "p/cx", "p/at.c",
                  "p/at", "div.b/cx", "marg.eb", "marg.l", "cres.rec",
                  "dy", "ly", "div.b/luc.m"), crit, outline = T,
        range = 10)

## retirando max(Div/Lucm)

row.names(crit)[match(max(crit$`Dív. Bruta/Lucro Mensal`),crit$`Dív. Bruta/Lucro Mensal`)]

crit = crit[-match(max(crit$`Dív. Bruta/Lucro Mensal`), crit$`Dív. Bruta/Lucro Mensal`),]

boxplot(names = c("pl", "pv", "roe", "roic", "p/cx", "p/at.c",
                  "p/at", "div.b/cx", "marg.eb", "marg.l", "cres.rec",
                  "dy", "ly", "div.b/luc.m"), crit, outline = T,
        range = 9)


## retirando max(marg.liq)

row.names(crit)[match(max(crit$`Marg. Líquida`),crit$`Marg. Líquida`)]

crit = crit[-match(max(crit$`Marg. Líquida`), crit$`Marg. Líquida`),]

boxplot(names = c("pl", "pv", "roe", "roic", "p/cx", "p/at.c",
                  "p/at", "div.b/cx", "marg.eb", "marg.l", "cres.rec",
                  "dy", "ly", "div.b/luc.m"), crit, outline = T,
        range = 9.15)


## retirando max(pv)

row.names(crit)[match(max(crit$`P/VPA`), crit$`P/VPA`)]

crit = crit[-match(max(crit$`P/VPA`), crit$`P/VPA`),]

boxplot(names = c("pl", "pv", "roe", "roic", "p/cx", "p/at.c",
                  "p/at", "div.b/cx", "marg.eb", "marg.l", "cres.rec",
                  "dy", "ly", "div.b/luc.m"), crit, outline = T,
        range = 9.15)

## retirando max(Div/Lucm)

row.names(crit)[match(max(crit$`Dív. Bruta/Lucro Mensal`),crit$`Dív. Bruta/Lucro Mensal`)]

crit = crit[-match(max(crit$`Dív. Bruta/Lucro Mensal`), crit$`Dív. Bruta/Lucro Mensal`),]

boxplot(names = c("pl", "pv", "roe", "roic", "p/cx", "p/at.c",
                  "p/at", "div.b/cx", "marg.eb", "marg.l", "cres.rec",
                  "dy", "ly", "div.b/luc.m"), crit, outline = T,
        range = 9.15)

## retirando max(Div/Lucm)

row.names(crit)[match(max(crit$`Dív. Bruta/Lucro Mensal`),crit$`Dív. Bruta/Lucro Mensal`)]

crit = crit[-match(max(crit$`Dív. Bruta/Lucro Mensal`), crit$`Dív. Bruta/Lucro Mensal`),]

boxplot(names = c("pl", "pv", "roe", "roic", "p/cx", "p/at.c",
                  "p/at", "div.b/cx", "marg.eb", "marg.l", "cres.rec",
                  "dy", "ly", "div.b/luc.m"), crit, outline = T,
        range = 8)

## retirando max(P/L)

row.names(crit)[match(max(crit$`P/L`),crit$`P/L`)]

crit = crit[-match(max(crit$`P/L`), crit$`P/L`),]

boxplot(names = c("pl", "pv", "roe", "roic", "p/cx", "p/at.c",
                  "p/at", "div.b/cx", "marg.eb", "marg.l", "cres.rec",
                  "dy", "ly", "div.b/luc.m"), crit, outline = T,
        range = 7.85)

## retirando max(P/ativ)

row.names(crit)[match(max(crit$`Preço/(Ativos/Ação)`),crit$`Preço/(Ativos/Ação)`)]

crit = crit[-match(max(crit$`Preço/(Ativos/Ação)`), crit$`Preço/(Ativos/Ação)`),]

boxplot(names = c("pl", "pv", "roe", "roic", "p/cx", "p/at.c",
                  "p/at", "div.b/cx", "marg.eb", "marg.l", "cres.rec",
                  "dy", "ly", "div.b/luc.m"), crit, outline = T,
        range = 7.5)

## retirando max(marg.liq)

row.names(crit)[match(max(crit$`Marg. Líquida`),crit$`Marg. Líquida`)]

crit = crit[-match(max(crit$`Marg. Líquida`), crit$`Marg. Líquida`),]

boxplot(names = c("pl", "pv", "roe", "roic", "p/cx", "p/at.c",
                  "p/at", "div.b/cx", "marg.eb", "marg.l", "cres.rec",
                  "dy", "ly", "div.b/luc.m"), crit, outline = T,
        range = 7.15)

## retirando max(P/ativc)

row.names(crit)[match(max(crit$`Preço/(Ativos Circulantes/Ação)`),
                      crit$`Preço/(Ativos Circulantes/Ação)`)]

crit = crit[-match(max(crit$`Preço/(Ativos Circulantes/Ação)`),
                   crit$`Preço/(Ativos Circulantes/Ação)`),]

boxplot(names = c("pl", "pv", "roe", "roic", "p/cx", "p/at.c",
                  "p/at", "div.b/cx", "marg.eb", "marg.l", "cres.rec",
                  "dy", "ly", "div.b/luc.m"), crit, outline = T,
        range = 6.75)

## retirando max(Div/Lucm)

row.names(crit)[match(max(crit$`Dív. Bruta/Lucro Mensal`),crit$`Dív. Bruta/Lucro Mensal`)]

crit = crit[-match(max(crit$`Dív. Bruta/Lucro Mensal`), crit$`Dív. Bruta/Lucro Mensal`),]

boxplot(names = c("pl", "pv", "roe", "roic", "p/cx", "p/at.c",
                  "p/at", "div.b/cx", "marg.eb", "marg.l", "cres.rec",
                  "dy", "ly", "div.b/luc.m"), crit, outline = T,
        range = 6.5)

## retirando max(pv)

row.names(crit)[match(max(crit$`P/VPA`), crit$`P/VPA`)]

crit = crit[-match(max(crit$`P/VPA`), crit$`P/VPA`),]

boxplot(names = c("pl", "pv", "roe", "roic", "p/cx", "p/at.c",
                  "p/at", "div.b/cx", "marg.eb", "marg.l", "cres.rec",
                  "dy", "ly", "div.b/luc.m"), crit, outline = T,
        range = 6.5)

## retirando max(P/cx)

row.names(crit)[match(max(crit$`Preço/(Caixa/Ação)`),crit$`Preço/(Caixa/Ação)`)]

crit = crit[-match(max(crit$`Preço/(Caixa/Ação)`), crit$`Preço/(Caixa/Ação)`),]

boxplot(names = c("pl", "pv", "roe", "roic", "p/cx", "p/at.c",
                  "p/at", "div.b/cx", "marg.eb", "marg.l", "cres.rec",
                  "dy", "ly", "div.b/luc.m"), crit, outline = T,
        range = 6)

## retirando max(roic)

row.names(crit)[match(max(crit$ROIC),crit$ROIC)]

crit = crit[-match(max(crit$ROIC), crit$ROIC),]

boxplot(names = c("pl", "pv", "roe", "roic", "p/cx", "p/at.c",
                  "p/at", "div.b/cx", "marg.eb", "marg.l", "cres.rec",
                  "dy", "ly", "div.b/luc.m"), crit, outline = T,
        range = 5.495)

## retirando max(cres.rec)

row.names(crit)[match(max(crit$`Cresc. Rec. (5 Anos)`), crit$`Cresc. Rec. (5 Anos)`)]

crit = crit[-match(max(crit$`Cresc. Rec. (5 Anos)`), crit$`Cresc. Rec. (5 Anos)`),]

boxplot(names = c("pl", "pv", "roe", "roic", "p/cx", "p/at.c",
                  "p/at", "div.b/cx", "marg.eb", "marg.l", "cres.rec",
                  "dy", "ly", "div.b/luc.m"), crit, outline = T,
        range = 5.4)

## retirando max(P/cx)

row.names(crit)[match(max(crit$`Preço/(Caixa/Ação)`),crit$`Preço/(Caixa/Ação)`)]

crit = crit[-match(max(crit$`Preço/(Caixa/Ação)`), crit$`Preço/(Caixa/Ação)`),]

boxplot(names = c("pl", "pv", "roe", "roic", "p/cx", "p/at.c",
                  "p/at", "div.b/cx", "marg.eb", "marg.l", "cres.rec",
                  "dy", "ly", "div.b/luc.m"), crit, outline = T,
        range = 5.2)

## retirando max(dy)

row.names(crit)[match(max(crit$Dividendyield),crit$Dividendyield)]

crit = crit[-match(max(crit$Dividendyield), crit$Dividendyield),]

boxplot(names = c("pl", "pv", "roe", "roic", "p/cx", "p/at.c",
                  "p/at", "div.b/cx", "marg.eb", "marg.l", "cres.rec",
                  "dy", "ly", "div.b/luc.m"), crit, outline = T,
        range = 5.1)

## retirando max(marg.liq)

row.names(crit)[match(max(crit$`Marg. Líquida`),crit$`Marg. Líquida`)]

crit = crit[-match(max(crit$`Marg. Líquida`), crit$`Marg. Líquida`),]

boxplot(names = c("pl", "pv", "roe", "roic", "p/cx", "p/at.c",
                  "p/at", "div.b/cx", "marg.eb", "marg.l", "cres.rec",
                  "dy", "ly", "div.b/luc.m"), crit, outline = T,
        range = 5)

## retirando max(P/cx)

row.names(crit)[match(max(crit$`Preço/(Caixa/Ação)`),crit$`Preço/(Caixa/Ação)`)]

crit = crit[-match(max(crit$`Preço/(Caixa/Ação)`), crit$`Preço/(Caixa/Ação)`),]

boxplot(names = c("pl", "pv", "roe", "roic", "p/cx", "p/at.c",
                  "p/at", "div.b/cx", "marg.eb", "marg.l", "cres.rec",
                  "dy", "ly", "div.b/luc.m"), crit, outline = T,
        range = 5)




























boxplot(names = c("pl", "pv", "roe", "roic", "p/cx", "p/at.c",
                  "p/at", "div.b/cx", "marg.eb", "marg.l", "cres.rec",
                  "dy", "ly", "div.b/luc.m"), crit, outline = T,
        range = 1.5)


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
