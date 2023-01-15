df_tri = read.csv("data/acoesb3.csv")

df_day = read.csv("data/acoesb3cot.csv")
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

head(df_tri,10)

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
  ativ = c(df_tri[i,"ativos"])
  patl = c(df_tri[i,"patLiq"])
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
                         ativc, ativ, patl, recl12, ebit12, Lucl12, recl3, row.names = codigo)
head(df_tri_3t22,10)


pl = round((cotAtual_t/(Lucl12/n_ac)), 2)

rmv_out = function(vec){
  dp = sd(vec)
  media = mean(vec)
  for (i in vec){
    if (i > 3*dp + media){
      print("if: ")
      vec[match(i,vec)] = media + 3*dp
    } else if (i < -3*dp + media){
      vec[match(i,vec)] = media - 3*dp
      print("else if: ")
    }
  }
  return(vec)
}

rmv_out(pl)