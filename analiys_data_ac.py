import mariadb
from datetime import datetime
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

df_tri = pd.read_csv('data/acoesb3.csv')

df_day = pd.read_csv('data/acoesb3cot.csv')

print('dft: ', df_tri.to_string(),'\ndfd: ', df_day.to_string())

## filtering rows form dfd
f_data_d = []
cod = []
df_day_g = pd.DataFrame()
for i in range(len(df_day['cod'])):
	if cod.count(df_day['cod'][i]) == 0:
		cod.append(df_day['cod'][i])

		f_data_d.append([df_day['cod'][i], datetime.strptime(df_day['ultCot'][i], '%Y-%m-%d'), df_day['cotAtual'][i],
						 df_day['divYield'][i]])

	df_day_g = pd.DataFrame(f_data_d)
for row in df_day_g:
	print(row)
print('len(f_data_d): ', len(f_data_d))
