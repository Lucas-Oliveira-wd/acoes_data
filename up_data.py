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

driver = webdriver.Chrome("C:\Program Files\Google\Chrome\Application\chromedriver_win32\chromedriver.exe")

driver.get("https://fundamentus.com.br/resultado.php")
content = driver.page_source
soup = BeautifulSoup(content)

#           Montando a lista de empresas                #
empresas = []
for a in soup.findAll('span'):
    element = a.find('a')
    str_ind = a.text[4]
    for i in range(6, 2, -1):  # começando do 6 até o 3 par dá prioridade as ações PN
        if str_ind == f'{i}' and len(a.text) == 5:  ## verificando se o codigo é de uma acao (o 5nt digito é 3-6)
            empresas.append(a.text)
lenght = len(empresas) - 1
# retirando as repetidas
for i in range(lenght, 0, -1):  ## como o tamanho da lista vai diminuir, o primeiro loop sera de traz pra frente
    for j in range(
            lenght):  # o segundo loop será normal pois ele é limitado ao tamanho da lista, ue pode mudar dentro do primeiro loop
        if empresas[i][0:4] == empresas[j][0:4]:  # verificando se os 4 primeiros digitos sao iguais
            if int(empresas[i][4]) > int(
                    empresas[j][4]):  # só para ter certeza que está sendo removido o papel com menor 5to digito
                empresas.remove(empresas[j])
                lenght = len(empresas) - 1
            elif int(empresas[i][4]) != int(empresas[j][4]):
                empresas.remove(empresas[i])
                lenght = len(empresas) - 1

## Verificando se possuem empresas repetidas        ##  !!tentar fazer depos. melhorar a função de busca por ações
# repetidas para que ela encontre os codigos com mesmo digito final
for i in empresas:
    cont = []
    for j in range(6):
        if empresas.count(i[0:4] + str(j)):
            cont.append(i)
            if len(cont) > 1:
                print(f"existe papeis repetidos para a ação {cont}")

mydb = mariadb.connect(
    host="localhost",
    user="root",
    password=None,
    database="invest"
)
mycursor = mydb.cursor()

for emp in empresas:
    stop_at = 365  ## variável para parar de atualizar os dados
    v_dados = []  # List to store values of datas coletados do site
    driver.get(f"https://fundamentus.com.br/detalhes.php?papel={emp}")  ## link que será pego para cada ação
    content = driver.page_source
    soup = BeautifulSoup(content)
    for a in soup.findAll('td'):  ## elementos da tabela ue contém os valores
        element = a.find('span', attrs={'class': 'txt'})  ## os valores estao dentro de <span>
        if hasattr(element, 'text'):  ## checando se os elementos tem o atributo txt
            v_dados.append(element.text)  ## adicionando os dados e um dicionario
            '''     
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

    if v_dados[13] != 'Intermediários Financeiros' and v_dados[13] != '' and len(
            v_dados) > 110:  # filtrando as ações de bancos,
        # os bancos são analisados de uma maneira
        # diferente por nao possuir todos os dados de uma empresa comum:
        # como 'dívida' por exemplo
        valBefCheck = (v_dados[13], v_dados[17], v_dados[7], v_dados[9], v_dados[23], emp, v_dados[3], v_dados[64],
                       v_dados[82], v_dados[67], v_dados[27], v_dados[89], v_dados[91], v_dados[95], v_dados[87],
                       v_dados[97], v_dados[102], v_dados[106], v_dados[110], v_dados[104])  ## essa lista é para
        # checkar se os dados estão corretos
        if isCorr(valBefCheck) == 'correct':
            dif = now.date() - datetime.datetime.strptime(f'\
{v_dados[23][6:]}-{v_dados[23][3:5]}-{v_dados[23][0:2]}', '%Y-%m-%d').date()## diferença entre o ultimo balanço e hoje
            dif_last_cot = now.date() - datetime.datetime.strptime(f'\
{v_dados[7][6:]}-{v_dados[7][3:5]}-{v_dados[7][0:2]}', '%Y-%m-%d').date()  ## diferença entre a ultima cotação e hoje
            if dif.days < stop_at and dif_last_cot.days < stop_at:  ##filtrando as empresa que nao
                # atualizam seus dados a mais de que o tempo de stop_at
                sql = f"SELECT MAX(ultBal) FROM acoesb3 WHERE codigo = '{emp}'"  ## buscando o
                # ultimo balanço das empresas no db
                mycursor.execute(sql)
                result = mycursor.fetchall()
                ult_bal = result[0][0]
                if ult_bal == v_dados[23]:
                    print(f'''\
{emp} não mudou desde o ultimo balaço processado. Portanto sem alterações feitas paraos dados trimestrais dessa
empresa''')
                    print('''analizando se a cotação já está atualizada''')
                    # verificando a ultima atualização no db
                    sql = f"""SELECT MAX(date) FROM acoesb3daily WHERE cod = '{emp}'"""
                    mycursor.execute(sql)
                    result = mycursor.fetchall()
                    ult_at = result[0][0].date()
                    today = now.date().day
                    dif_days = today - ult_at.day
                    dif_last_cot_day = today - v_dados[7].date().day
                    if dif_days > dif_last_cot_day:  # verificando se já a cotação ja foi atualizada hoje
                        print('''\
a cotação não foi atualizada. inserindo cotação e dividendyield para o db diario''')
                        sql = '''INSERT INTO acoesb3daily (cod, cotAtual, divYield) VALUES (%s, %s, %s) '''
                        ## verificando se os dados estão corretos
                        val = (emp, converComTD(v_dados[3]), converComTD(v_dados[67]))
                        mycursor.execute(sql, val)
                        mydb.commit()
                        print(mycursor.rowcount, f"record inserted. vales {val} on acoesb3daily")
                    else:
                        print(f'''\
Não atualizando o preço para {emp}. A data da ultima cotação é a mesma em que a data da última atualização no db''')
                else:
                    sql = """INSERT INTO acoesb3 (setor, subSetor, empName, ultBal, codigo, cotAtual, roic, cresRec5a,
                    divYield, nAcoes, divBruta, disponib, ativCirc, ativos, patLiq, recLiq12m, ebit12m, LucLiq12m,
                    recLiq3m ) VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)"""
                    ## verficando se os dados estão corretos
                    val = (v_dados[13], v_dados[17], v_dados[9], datetime.datetime.strptime(
                            f'{v_dados[23][6:]}-{v_dados[23][3:5]}-{v_dados[23][0:2]}', '%Y-%m-%d').date(), emp,
                            converComTD(v_dados[3]), converComTD(v_dados[64]), converComTD(v_dados[82]),
                           converComTD(v_dados[67]), converComTD(v_dados[27]), converComTD(v_dados[89]),
                            converComTD(v_dados[91]), converComTD(v_dados[95]), converComTD(v_dados[87]),
                            converComTD(v_dados[97]), converComTD(v_dados[102]), converComTD(v_dados[106]),
                            converComTD(v_dados[110]), converComTD(v_dados[104])
                           )
                    mycursor.execute(sql, val)
                    mydb.commit()
                    print(mycursor.rowcount, f"record inserted. vales {val} on acoesb3")