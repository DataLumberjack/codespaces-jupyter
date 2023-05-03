

import pandas as pd
import streamlit as st
import numpy as np
from matplotlib import pyplot as plt
from tkinter.tix import COLUMN

def main():
    st.title("PROJET FINALE UE 3.3")
    st.subheader("Créateur : Martin Ravelomanantsoa")
    
    #import des données:
      
    @st.cache(persist=True) #création d'un cache pour eviter le chargement trop long de l'appli
   
    def load_data(): 
        data = pd.read_csv('/Users/martinravelo/Desktop/projetpy/Data_Science_Fields_Salary_Categorization.csv')
        return data
    
    #affichage des données :
    df = load_data()
    df_sample = df.sample(50) #limitation de l'affichage des données
    if st.sidebar.checkbox("afficher données", False): #création d'une checkbox pour afficher ou non la table
        st.subheader("Catégories de salaires des différents domaines dans la DataScience : echantillon de 50") #création d'un sous-titre
        st.write(df_sample) #faire apparaitre les données
        
        
    #ajout d'une selection de critères
    classifier = st.sidebar.selectbox(
        "classificateur",
        ("Designation","Salary_In_Rupees",)
    )
    
    #ajout d'une selection de sous-critères
    if classifier == "Salary_In_Rupees":
        st.sidebar.subheader("montant du salaire en rupees") #création d'un sous-titre
        n_estimators = st.sidebar.number_input( #séléction du sous-critère avec le montant souhaité 
            "choisir le salaire",
            100,100000, step=1000, 
        )
    if classifier == "Designation":
        Designation = st.sidebar.selectbox( #séléction du type de métier avec une selectbox
            "type de métier",
            ("Data Scientist","Machine Learning Scientist","Data Engineer","Finance Data Analyst","BI Data Analyst","Cloud Data Engineer","Head of Data Science")
        )
            





    
     

if __name__ == '__main__' :
    main()
     