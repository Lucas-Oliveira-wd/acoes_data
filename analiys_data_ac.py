import mariadb
import datetime
import pandas as pd

mydb = mariadb.connect(
	host="localhost",
	user="root",
	password=None,
	database="invest"
)
mycursor = mydb.cursor()

sql = f"""SELECT DISTINCT ultBal FROM acoesb3 ORDER BY ultBal DESC"""

mycursor.execute(sql)
result = mycursor.fetchall()

ult_bal = []
for dat in result:
	ult_bal.append(dat[0])

pd.options.display.max_rows = 9999

df = pd.read_csv('ref/acoesb3.csv')
print(df.to_string())

'''
sql = f"""\
SELECT acoesb3.ultBal, acoesb3.codigo, acoesb3cot.cotAtual, acoesb3cot.divYield , acoesb3.roic, acoesb3.cresRec5a,\
acoesb3.nAcoes,\
acoesb3.divBruta, acoesb3.disponib,\
acoesb3.ativCirc, acoesb3.ativos, acoesb3.patLiq, acoesb3.recLiq12m, acoesb3.ebit12m, \
acoesb3.LucLiq12m, acoesb3.recLiq3m, acoesb3.ebit3m, acoesb3.LucLiq3m FROM acoesb3 INNER JOIN acoesb3cot ON \
acoesb3.codigo=acoesb3cot.cod GROUP BY acoesb3cot.cod ORDER BY acoesb3.ultBal DESC
"""
dados = []
mycursor.execute(sql)
result = mycursor.fetchall()
for row in result:
	print(row)

print("len(result): ", len(result))
'''