import smtplib
from email.mime.text import MIMEText
from email.mime.multipart import MIMEMultipart

# Função para enviar e-mails em caso de erro
def enviar_email_erro(mensagem):
    de_email = 'emailautomatico11@gmail.com'
    para_email = 'lucasoliveira5978@gmail.com'
    senha = 'nucxkwaaqoazgnxh'

    msg = MIMEMultipart()
    msg['From'] = de_email
    msg['To'] = para_email
    msg['Subject'] = 'Erro na execução do script "up_data.py"'

    corpo_mensagem = mensagem
    msg.attach(MIMEText(corpo_mensagem, 'plain'))

    try:
        server = smtplib.SMTP('smtp.gmail.com', 587)
        server.starttls()
        server.login(de_email, senha)
        server.sendmail(de_email, para_email, msg.as_string())
        server.quit()
        print('E-mail de erro enviado com sucesso!')
    except Exception as e:
        print(f'Erro ao enviar e-mail de erro: {str(e)}')

try:
	import mariadb
	import datetime
	from selenium import webdriver
	from bs4 import BeautifulSoup
	import sys

	one_day = 60 * 60 * 24
	now = datetime.datetime.now()

	def converComTD(strin):  # função para transformar string com virgula in float
		wtt_dot = strin.replace('.', '')
		wtt_com = wtt_dot.replace(',', '.')
		wtt_perc = wtt_com.replace('%', '')
		conv = float(wtt_perc)
		return conv


	def isCorr(dic):  ## função para verificar se os dados estão corretos
		incorr = []
		for i in dic:
			if i == '' or i == '-' or i == '\n-':
				incorr.append(i)
		if len(incorr) > 0:
			for j in incorr:
				return j + ' is incorrect'  ## retornar o valor da variavel incorreta
		else:
			return 'correct'
	def last_letter(word):	## funcao para retornar o ultimo caractere de uma string
		return word[::-1]

	driver = webdriver.Chrome("C:\Program Files\Google\Chrome\Application\chromedriver-win64\chromedriver.exe")

	driver.get("https://fundamentus.com.br/resultado.php")
	content = driver.page_source
	soup = BeautifulSoup(content)

	#           Montando a lista de empresas                #
	empresas = []
	for a in soup.findAll('span'):
		element = a.find('a')
		str_ind = a.text[4]
		for i in range(3, 7):
			if str_ind == str(i) and 5 <= len(a.text) <= 6:  ## verificando se o codigo é de uma acao (o 5nt digito é 3-6)
				empresas.append(a.text)

	mydb = mariadb.connect(
		host="localhost",
		user="root",
		password=None,
		database="invest"
	)
	mycursor = mydb.cursor()

	insert_emp = [] #tuple para conter as empresas que ja foram registradas
	for emp in sorted(empresas, key=last_letter, reverse = True): ## ordenando as empresas para dar preferencias as PN
		stop_at = 365  ## variável para parar de atualizar os dados
		v_dados = []  # List to store values of datas coletados do site
		driver.get(f"https://fundamentus.com.br/detalhes.php?papel={emp}")  ## link que será pego para cada ação
		content = driver.page_source
		soup = BeautifulSoup(content)
		for a in soup.findAll('td'):  ## elementos da tabela que contém os valores
			element = a.find('span', attrs={'class': 'txt'})  ## os valores estao dentro de <span>
			if hasattr(element, 'text'):  ## checando se os elementos tem o atributo txt
				v_dados.append(element.text)  ## adicionando os dados e um dicionario
				'''\     
							# SUMÁRIO DOS DADOS EM v_dados[]  ## !IMPORTANT, Esse sumário não serve para os bancos
	
				Papel:  1; Cotação:  3; Tipo:  5; Data últ cot:  7; Empresa:  9; Min 52 sem:  11; Setor:  13;
				Max 52 sem:  15; Subsetor:  17; Vol $ méd (2m):  19; Valor de mercado:  21; Últ balanço processado:  23;
				Valor da firma:  25; Nro. Ações:  27;
	
						#       Indicadores fundamentalistas        #
	
				P/L:  32; LPA:  34; P/VP:  37; VPA:  39; P/EBIT:  42; Marg. Bruta:  44; PSR:  47; Marg. EBIT:  49;
				P/Ativos:  52; Marg. Líquida:  54; P/Cap. Giro:  57; EBIT / Ativo:  59; P/Ativ Circ Liq:  62; iROIC:  64;
				Div. Yield:  67; ROE:  69; EV / EBITDA:  72; Liquidez Corr:  74; EV / EBIT:  77; Div Br/ Patrim:  79;
				Cres. Rec (5a):  82; Giro Ativos:  84;
	
						#       Dados Balanço Patrimonial       #
	
				Ativo:  87; Dív. Bruta:  89; Disponibilidades:  91; Dív. Líquida:  93; Ativo Circulante:  95;
				Patrim. Líq:  97;
	
					#       Dados demonstrativos de resultados      #
					##      Últimos 12 meses        ##
	
				Receita Líquida:  102; EBIT:  106; Lucro Líquido:  110;       
	
					##      Últimos 3 meses         ##
	
				Receita Líquida:  104; EBIT:  108; Lucro Líquido:  112;
				'''

		if len(v_dados) > 110:  # filtrando as ações de bancos,
			# os bancos são analisados de uma maneira
			# diferente por nao possuir todos os dados de uma empresa comum:
			# como 'dívida' por exemplo
			valBefCheck = (v_dados[13], v_dados[17], v_dados[7], v_dados[9], v_dados[23], emp, v_dados[3], v_dados[64],
					v_dados[82], v_dados[67], v_dados[27], v_dados[89], v_dados[91], v_dados[95], v_dados[87],
					v_dados[97], v_dados[102], v_dados[106], v_dados[110], v_dados[104], v_dados[108], v_dados[112]) ##essa
			# lista é para checkar se os dados estão corretos
			if isCorr(valBefCheck) == 'correct':
				ult_bal = datetime.datetime.strptime(f'{v_dados[23][6:]}-{v_dados[23][3:5]}-{v_dados[23][0:2]}',
					'%Y-%m-%d')
				ult_cot = datetime.datetime.strptime(f'{v_dados[7][6:]}-{v_dados[7][3:5]}-{v_dados[7][0:2]}',
					'%Y-%m-%d')
				dif = now.date() - ult_bal.date()  ## diferença entre o ultimo balanço e hoje
				dif_last_cot = now.date() - ult_cot.date()  ## diferença entre a ultima cotação e hoje
								# retirando as repetidas
				if dif.days < stop_at and dif_last_cot.days < stop_at:  ##filtrando as empresa que nao
															# atualizam seus dados a mais de que o tempo de stop_at
					if insert_emp.count(emp[0:4]) == 0:
						insert_emp.append(emp[0:4])
						sql = f"SELECT MAX(ultBal) FROM acoesb3 WHERE codigo = '{emp}'"  ## buscando o
						# ultimo balanço das empresas no db
						mycursor.execute(sql)
						result = mycursor.fetchall()
						ult_bal_db = result[0][0]
						if ult_bal_db != ult_bal.date():
							sql = """INSERT INTO acoesb3 (setor, subSetor, empName, ultBal, codigo, roic, cresRec5a,
							nAcoes, divBruta, disponib, ativCirc, ativos, patLiq, recLiq12m, ebit12m, LucLiq12m,
							recLiq3m, ebit3m, LucLiq3m) VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s,
							%s, %s, %s, %s, %s, %s, %s)"""
							## verficando se os dados estão corretos
							val = (v_dados[13], v_dados[17], v_dados[9], datetime.datetime.strptime(
								f'{v_dados[23][6:]}-{v_dados[23][3:5]}-{v_dados[23][0:2]}', '%Y-%m-%d').date(), emp,
								converComTD(v_dados[64]), converComTD(v_dados[82]), converComTD(v_dados[27]),
								converComTD(v_dados[89]), converComTD(v_dados[91]), converComTD(v_dados[95]),
								converComTD(v_dados[87]),
								converComTD(v_dados[97]), converComTD(v_dados[102]), converComTD(v_dados[106]),
								converComTD(v_dados[110]), converComTD(v_dados[104]), converComTD(v_dados[108]),
								converComTD(v_dados[112])
								)
							mycursor.execute(sql, val)
							mydb.commit()
							print(mycursor.rowcount, f"record inserted. vales {val} on acoesb3")
						if ult_bal_db == ult_bal.date():
							print(f'''\
	{emp} não mudou desde o ultimo balaço processado. Portanto sem alterações feitas para os dados trimestrais dessa\
	empresa''')
						print('''analizando se a cotação já está atualizada''')
						# verificando a data de ultima cotacão no db ultima atualização no db
						sql = f"""SELECT MAX(ultCot) FROM acoesb3cot WHERE cod = '{emp}'"""
						mycursor.execute(sql)
						result = mycursor.fetchall()
						ult_cot_db = result[0][0]
						if ult_cot_db == None: ## filtrando os None pq nao da para converter para tipo date
							ult_cot_db = datetime.datetime.strptime("1970-01-01", '%Y-%m-%d').date()

						# verificando se a cotação ja foi atualizada hoje
						if ult_cot.date() > ult_cot_db:
							print('''\
	a cotação não foi atualizada. inserindo cotação e dividendyield para o db diario''')
							sql = '''INSERT INTO acoesb3cot (ultCot ,cod, cotAtual, nAcoes, divYield) VALUES (%s, %s, %s, %s, %s) '''
							## verificando se os dados estão corretos
							val = (ult_cot.date(), emp, converComTD(v_dados[3]), converComTD(v_dados[27]), converComTD(v_dados[67]))
							mycursor.execute(sql, val)
							mydb.commit()
							print(mycursor.rowcount, f"record inserted. values {val} on acoesb3cot")
						else:
							print(f'''\
	Não atualizando o preço para {emp}. Já está atualizado''')
		elif len(v_dados) >= 108:
			print(f'''\
	{emp} foi identificada como uma acão de banco''')
			valBefCheck = (v_dados[13], v_dados[17], v_dados[7], v_dados[9], v_dados[23], emp, v_dados[3],
					   v_dados[82], v_dados[67], v_dados[27], v_dados[87],
					   v_dados[102], v_dados[106], v_dados[93], v_dados[98], v_dados[100], v_dados[104],
					   v_dados[108])  ## essa lista é para
			# checkar se os dados estão corretos
			if isCorr(valBefCheck) == 'correct':
				ult_bal = datetime.datetime.strptime(f'{v_dados[23][6:]}-{v_dados[23][3:5]}-{v_dados[23][0:2]}',
													 '%Y-%m-%d')
				ult_cot = datetime.datetime.strptime(f'{v_dados[7][6:]}-{v_dados[7][3:5]}-{v_dados[7][0:2]}',
													 '%Y-%m-%d')
				dif = now.date() - ult_bal.date()  ## diferença entre o ultimo balanço e hoje
				dif_last_cot = now.date() - ult_cot.date()  ## diferença entre a ultima cotação e hoje
				# retirando as repetidas
				if dif.days < stop_at and dif_last_cot.days < stop_at:  ##filtrando as empresa que nao
					# atualizam seus dados a mais de que o tempo de stop_at
					if insert_emp.count(emp[0:4]) == 0:
						insert_emp.append(emp[0:4])
						sql = f"SELECT MAX(ultBal) FROM bank WHERE codigo = '{emp}'"  ## buscando o
						# ultimo balanço das empresas no db
						mycursor.execute(sql)
						result = mycursor.fetchall()
						ult_bal_db = result[0][0]
						if ult_bal_db != ult_bal.date():
							sql = """INSERT INTO bank (setor, subSetor, empName, ultBal, codigo, cresRec5a,
									nAcoes, ativos, patLiq, resIntFin12m, recServ12m, lucLiq12m,
									resIntFin3m, recServ3m, lucLiq3m ) VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s,
									%s, %s, %s, %s, %s)"""
							## verficando se os dados estão corretos
							val = (v_dados[13], v_dados[17], v_dados[9], datetime.datetime.strptime(
								f'{v_dados[23][6:]}-{v_dados[23][3:5]}-{v_dados[23][0:2]}', '%Y-%m-%d').date(), emp,
								   converComTD(v_dados[82]), converComTD(v_dados[27]),
								   converComTD(v_dados[87]), converComTD(v_dados[93]), converComTD(v_dados[98]),
								   converComTD(v_dados[102]), converComTD(v_dados[106]), converComTD(v_dados[100]),
								   converComTD(v_dados[104]), converComTD(v_dados[108])
								   )
							mycursor.execute(sql, val)
							mydb.commit()
							print(mycursor.rowcount, f"record inserted. vales {val} on bank")
						if ult_bal_db == ult_bal.date():
							print(f'''\
	{emp} não mudou desde o ultimo balaço processado. Portanto sem alterações feitas para os dados trimestrais dessa
				empresa''')
						print('''analizando se a cotação já está atualizada''')
						# verificando a data de ultima cotacão no db ultima atualização no db
						sql = f"""SELECT MAX(ultCot) FROM bankcot WHERE cod = '{emp}'"""
						mycursor.execute(sql)
						result = mycursor.fetchall()
						ult_cot_db = result[0][0]
						if ult_cot_db == None:  ## filtrando os None pq nao da para converter para tipo date
							ult_cot_db = datetime.datetime.strptime("1970-01-01", '%Y-%m-%d').date()

						# verificando se a cotação ja foi atualizada hoje
						if ult_cot.date() > ult_cot_db:
							print('''\
	a cotação não foi atualizada. inserindo cotação e dividendyield para o db diario''')
							sql = '''INSERT INTO bankcot (ultCot ,cod, cotAtual, nAcoes, divYield) VALUES (%s, %s, %s, %s, %s) '''
							## verificando se os dados estão corretos
							val = (ult_cot.date(), emp, converComTD(v_dados[3]), converComTD(v_dados[27]), converComTD(v_dados[67]))
							mycursor.execute(sql, val)
							mydb.commit()
							print(mycursor.rowcount, f"record inserted. values {val} on bankcot")
						else:
							print(f'''\
	Não atualizando o preço para {emp}. Já está atualizado''')
		else:
			print(f'''\
	{emp} não possui dados suficientes para ser analizada''')

except Exception as e: #enviar email em caso de error
    mensagem_erro = f'Erro na execução do script "up_data.py": {str(e)}'
    enviar_email_erro(mensagem_erro)