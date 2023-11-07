import mariadb
import datetime
from selenium import webdriver
from bs4 import BeautifulSoup

mydb = mariadb.connect(
    host="localhost",
    user="root",
    password=None,
    database="invest"
)
mycursor = mydb.cursor()

sql = f"SELECT DISTINCT cod FROM acoesb3cot"
mycursor.execute(sql)
result = mycursor.fetchall()
empresas = []
for r in result:
    empresas.append(r[0])

for emp in empresas:
    sql = f"SELECT nAcoes, ultCot FROM acoesb3cot WHERE cod='{emp}' ORDER BY ultCot"
    mycursor.execute(sql)
    result = mycursor.fetchall()
    for r in result:
        if r[0] == None:
            sql = f"SELECT nAcoes, ultBal FROM acoesb3 WHERE codigo='{emp}' AND ultBal>='{r[1]}' ORDER BY ultBal LIMIT 1"
            mycursor.execute(sql)
            result2 = mycursor.fetchall()
            for r2 in result2:
                print(f"{emp}: {r2[0]}, {r[1]}")
                sql = f"UPDATE acoesb3cot SET nAcoes = {r2[0]} WHERE cod='{emp}' AND ultCot='{r[1]}'"

