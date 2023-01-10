import pandas as pd # Para evitar escrever pandas e trocar pela escrita apenas de pd para facilitar
from pandas_datareader import data as web # Evita a escrita do data e troca pelo web
import time
import smtplib, ssl
from email.mime.text import MIMEText
from email.mime.multipart import MIMEMultipart

def verMaxDate(date,vals): ## função para retornar a date que ocorreu o valor minimo
    for i in range(len(vals)):
        if vals[i] == max(vals):
            return date[i]

data_final = time.strftime('%m-%d-%y', time.localtime(time.time()))
##  criando a data inicial (3 meses)
one_day = 60*60*24
interval = 3 ## intervalo de tempo em meses

data_inicial = time.strftime('%m-%d-%y', time.localtime(time.time()-interval*30*one_day))

msg_aval_since = """\
Procurando por uma janela de oportunidade de compra para as empresas da planilha 'empresas.xlsx'.
Desde a data {}(Mês-Dia-Ano) até agr
"""
print(msg_aval_since.format(data_inicial))

empresas_df = pd.read_excel("empresas.xlsx")

for empresa in empresas_df['Empresas']:
    df = web.DataReader(f'{empresa}.SA', data_source='yahoo', start=data_inicial, end=data_final)

    var = ((df["Adj Close"][len(df["Adj Close"])-1]) - max(df["Adj Close"]))/max(df["Adj Close"])
    wind = -0.3 # valor para janela de oportunidade


    dates = [] ## para conter as datas
    vals = []  ## para conter os valores

    for i in df.index:
        dates.append(i)
    for i in df["Adj Close"]:
        vals.append(i)

    if var < wind:

        sender_email = "emailautomatico11@gmail.com"     # Enter your address
        receiver_email = "lucasoliveira5978@gmail.com"      # Enter receiver address
        password = "nucxkwaaqoazgnxh"
        message = MIMEMultipart("alternative")
        message["Subject"] = f"Oportunidade de compra: {empresa}!"
        message["From"] = sender_email
        message["To"] = receiver_email

        # Create the plain-text and HTML version of your message
        msg = """\
        Há uma janela de oportunidade de compra para {0}. A acão caiu mais de {1} % nos últimos {2} meses
        """

        text = msg.format(empresa, wind*-100, interval)

        msg = """\
        <html>
          <body style="text-align: center; padding: 50px 20px; font-size: 20px;">
            <h3 style="background-color: #ddd; max-width: max-content; margin: 0 auto; padding: 10px 30px;
            border-radius: 5px;"> Há uma janela de oportunidade de compra para: </h3>
                <div style='background-color: rgb(126, 15, 15); max-width: 100px; padding: 10px 50px;
                margin: 30px auto; font-size: 25px; border-radius: 5px; box-shadow: 5px 3px 9px; color: #fff;'>
                <b>{0}</b> </div>
            <p> A acão estava cotada no valor de <b> R$ {1} </b> no dia <b> {2}</b>,</p>
            <p> hoje está cotada no valor de <b>R$ {3}</b>,</p>
            <p style=' background-color: rgb(73, 28, 28); color: #fff; padding: 10px 0;'> uma queda de {4} %. </p>
          </body>
        </html>
        """

        html = msg.format(empresa, round(max(df["Adj Close"]), 2), str(verMaxDate(dates, vals))[0:10],
                          round(df["Adj Close"][len(df["Adj Close"])-1], 2), round(-var*100,2))

        # Turn these into plain/html MIMEText objects
        part1 = MIMEText(text, "plain")
        part2 = MIMEText(html, "html")

        # Add HTML/plain-text parts to MIMEMultipart message
        # The email client will try to render the last part first
        message.attach(part1)
        message.attach(part2)

        # Create secure connection with server and send email
        context = ssl.create_default_context()
        with smtplib.SMTP_SSL("smtp.gmail.com", 465, context=context) as server:
            server.login(sender_email, password)
            server.sendmail(
                sender_email, receiver_email, message.as_string().encode('utf-8')
            )
    else:
        msg = "Empresa {} analisada. Sem oportunidade"
        print(msg.format(empresa))

