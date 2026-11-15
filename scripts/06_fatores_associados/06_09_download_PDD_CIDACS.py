# Download das bases da Plataforma de Dados Desidentificados (PDD/CIDACS).
# Requer cadastro na PDD e a biblioteca pdd_lib, fornecida pelo CIDACS.
# Credenciais lidas das variáveis de ambiente PDD_EMAIL e PDD_TOKEN.
# Arquivo original: Análises/Parte 5 - Fatores Associados/Bancos/Cidacs/Cidacs - Códigos.py

import os
import sys
import time

caminho_cidacs = os.environ.get("PDD_LIB", "pdd_lib")  # pasta da biblioteca pdd_lib do CIDACS
sys.path.append(caminho_cidacs)

from bib_pdd_cidacs import BibPddCidacs

client = BibPddCidacs()

credentials = {
    "email": os.environ["PDD_EMAIL"],
    "token": os.environ["PDD_TOKEN"] 
}

print("1. A ligar aos servidores da Fiocruz/CIDACS...")
client.authentication(credentials)

bases_chagas = {
    279: "Precipitação Diária (2000-2024)",
    315: "Características Urbanísticas (Censo 2022)",
    277: "Temperatura (2000-2023)",
    278: "Umidade Relativa (2000-2023)",
    282: "SPEI (Anomalias Climáticas)",
    244: "Bolsa Família",
    256: "Programa Cisternas",
    56: "CNES Equipamentos",
    57: "CNES Leitos",
    58: "CNES Profissionais",
    37: "Registro de Óbitos (Controlo SIM)",
    71: "Índice de Vulnerabilidade Municipal (IVM)"
}

print("\n2. A extrair a estrutura de colunas de cada base...\n")

for id_base, nome_base in bases_chagas.items():
    print(f"--- {nome_base} (ID: {id_base}) ---")
    try:
        colunas = client.list_columns(dataset=id_base)
        print(f"Colunas encontradas: {colunas}\n")
    except Exception as e:
        print(f"Erro ao ler as colunas da base {id_base}: {e}\n")
    
    time.sleep(2)
    
print("Mapeamento concluído!")
