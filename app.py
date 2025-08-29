import pandas as pd

# --- DADOS DE PREVISÕES ---
previsoes_urls = {
    'ARIMA': "https://raw.githubusercontent.com/Sigerip/Curso-CONCICAT/refs/heads/main/Previs%C3%B5es%20e%20m%C3%A9tricas/ARIMA_previsoes_intervalos_arima.csv",
    'ETS': "https://github.com/Sigerip/Curso-CONCICAT/raw/refs/heads/main/Previs%C3%B5es%20e%20m%C3%A9tricas/ETS_previsoes.csv",
    'LC': "https://github.com/Sigerip/Curso-CONCICAT/raw/refs/heads/main/Previs%C3%B5es%20e%20m%C3%A9tricas/LC_prev_total.csv",
    'NNAR': "https://github.com/Sigerip/Curso-CONCICAT/raw/refs/heads/main/Previs%C3%B5es%20e%20m%C3%A9tricas/NNAR_previsoes.csv",
    'Media': "https://github.com/Sigerip/Curso-CONCICAT/raw/refs/heads/main/Previs%C3%B5es%20e%20m%C3%A9tricas/prev_media_simples.csv",
    'Ponderada': "https://github.com/Sigerip/Curso-CONCICAT/raw/refs/heads/main/Previs%C3%B5es%20e%20m%C3%A9tricas/previsoes_ponderada.csv"
}

dfs_previsoes = []
for modelo, url in previsoes_urls.items():
    df = pd.read_csv(url)
    df.columns = df.columns.str.lower()
    df['modelo'] = modelo
    # Padroniza o nome da coluna de previsão
    if 'previsao_final' in df.columns:
        df.rename(columns={'previsao_final': 'previsto'}, inplace=True)
    dfs_previsoes.append(df)

previsoes_consolidadas = pd.concat(dfs_previsoes, ignore_index=True)
# Seleciona e renomeia colunas para manter um padrão
previsoes_consolidadas = previsoes_consolidadas[['ano', 'sexo', 'idade', 'previsto', 'modelo']]
#previsoes_consolidadas.to_csv('previsoes_modelos.csv', index=False)



# --- DADOS DE ERROS ---
erros_urls = {
    'ARIMA': "https://raw.githubusercontent.com/Sigerip/Curso-CONCICAT/refs/heads/main/Previs%C3%B5es%20e%20m%C3%A9tricas/ARIMA_modelos_metricas_arima.csv",
    'ETS': "https://github.com/Sigerip/Curso-CONCICAT/raw/refs/heads/main/Previs%C3%B5es%20e%20m%C3%A9tricas/ETS_metricas.csv",
    'LC': "https://github.com/Sigerip/Curso-CONCICAT/raw/refs/heads/main/Previs%C3%B5es%20e%20m%C3%A9tricas/LC_metricas_por_sexo.csv",
    'NNAR': "https://github.com/Sigerip/Curso-CONCICAT/raw/refs/heads/main/Previs%C3%B5es%20e%20m%C3%A9tricas/NNAR_metricas.csv",
    'Media': "https://github.com/Sigerip/Curso-CONCICAT/raw/refs/heads/main/Previs%C3%B5es%20e%20m%C3%A9tricas/metrica_media_simples.csv",
    'Ponderada': "https://github.com/Sigerip/Curso-CONCICAT/raw/refs/heads/main/Previs%C3%B5es%20e%20m%C3%A9tricas/metricas_ponderada.csv"
}

dfs_erros = []
for modelo, url in erros_urls.items():
    df = pd.read_csv(url)
    df.columns = df.columns.str.lower()
    df['modelo'] = modelo
    dfs_erros.append(df)

erros_consolidados = pd.concat(dfs_erros, ignore_index=True)
# Transforma o DataFrame para o formato longo (ideal para gráficos)
erros_long = erros_consolidados.melt(
    id_vars=['sexo', 'idade', 'modelo'],
    value_vars=['rmse', 'smape', 'mae'],
    var_name='metrica',
    value_name='valor'
)
erros_long['metrica'] = erros_long['metrica'].str.upper() # Deixa as métricas em maiúsculo
#erros_long.to_csv('erros_modelos.csv', index=False)


########################################################################################################################

# Criação do dashboard com Streamlit

# 1. IMPORTAR BIBLIOTECAS
import streamlit as st
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go # Importante adicionar para o gráfico de radar

# 2. CONFIGURAÇÃO DA PÁGINA E TÍTULO
st.set_page_config(layout="wide", page_title="Dashboard")

st.title("Visualização dos Resultados Obtidos no Minicurso - CONCICAT")

# 3. CARREGAR OS DADOS (USANDO OS DADOS JÁ CARREGADOS E TRATADOS)
df_erros = erros_long.copy()
df_previsoes = previsoes_consolidadas.copy()

# 4. CRIAR A BARRA LATERAL (SIDEBAR) COM FILTROS
with st.sidebar:
    st.header("🔧 Filtros de Análise")
    
    sexos = df_erros["sexo"].unique()
    metricas_disponiveis = df_erros['metrica'].unique()
    anos_disponiveis = sorted(df_previsoes["ano"].unique())
    
    sexo_selecionado = st.radio("Selecione o sexo:", sexos, horizontal=True)
    metrica_selecionada = st.selectbox("Selecione a métrica:", metricas_disponiveis)
    ano_selecionado = st.selectbox("Ano para previsões:", anos_disponiveis)

# 5. FILTRAR OS DADOS CONFORME A SELEÇÃO DO USUÁRIO
df_erros_filtrado = df_erros[ 
    (df_erros["sexo"] == sexo_selecionado) & 
    (df_erros["metrica"] == metrica_selecionada)
]

df_previsoes_filtrado = df_previsoes[
    (df_previsoes['sexo'] == sexo_selecionado) &
    (df_previsoes['ano'] == ano_selecionado)
]

# 6. SEÇÃO DE ANÁLISE DE ERROS (GRÁFICOS ORIGINAIS RESTAURADOS)

col1, col2 = st.columns([1, 1])

# Coluna 1: Boxplot detalhado
with col1:
    st.subheader("📊 Distribuição das Métricas por Modelo")
    
    # Ordena os modelos pela mediana do erro para melhor visualização
    ordem_modelos = df_erros_filtrado.groupby('modelo')['valor'].mean().sort_values().index.tolist()
    
    fig_box = px.box(
        df_erros_filtrado,
        x='modelo',
        y='valor',
        height=550,
        color='modelo',
        title=f'Comparação da Métrica {metrica_selecionada} ({sexo_selecionado})',
        labels={'modelo': 'Modelo', 'valor': metrica_selecionada},
        points="all", # Mostra todos os pontos
        category_orders={'modelo': ordem_modelos} # Aplica a ordenação
    )
    fig_box.update_traces(
        pointpos=0,  # Centraliza os pontos na caixa (0 = centro, -1 = esquerda, 1 = direita)
        jitter=0.3,  # Adiciona dispersão horizontal aos pontos
        marker=dict(
            size=6,  # Tamanho dos pontos
            opacity=0.7,  # Transparência dos pontos
            line=dict(width=1, color='white')  # Borda branca nos pontos
        )
    )
    fig_box.update_layout(
        xaxis_title="Modelo (ordenado pela média do erro)",
        yaxis_title=metrica_selecionada,
        showlegend=False,
        yaxis_type='log',
        yaxis=dict(
            tickformat=".6f",
            exponentformat="none"
        )
    )
    fig_box.update_xaxes(tickangle=0)
    st.plotly_chart(fig_box, use_container_width=True)

# Coluna 2: Tabs com Gráfico de Radar e de Tendência
with col2:
    st.subheader("🎯 Análise das Métricas de Erro por Faixa Etária")
    
    tab1, tab2 = st.tabs(["📡 Análise Faixa Etária Específica", "📈 Análise Geral"])
    
    with tab1:
        faixa_etaria = st.selectbox('Selecione a faixa etária:', sorted(df_erros_filtrado['idade'].unique()))
        df_radar = df_erros_filtrado[df_erros_filtrado['idade'] == faixa_etaria]
        # Prepara os dados para fechar o polígono do radar
        modelos = df_radar['modelo'].tolist()
        valores = df_radar['valor'].tolist()
        modelos.append(modelos[0]) # Repete o primeiro valor no final
        valores.append(valores[0]) # Repete o primeiro valor no final

        fig_radar = go.Figure()
        fig_radar.add_trace(go.Scatterpolar(
            r=valores,
            theta=modelos,
            fill='toself',
            name=f'Erro {metrica_selecionada}'
        ))
        fig_radar.update_layout(
            title=f"{metrica_selecionada} por Modelo ({sexo_selecionado}) - Idade {faixa_etaria}",
            polar=dict(radialaxis=dict(tickformat=".6f",visible=True)),
            showlegend=False,
            
        )
        st.plotly_chart(fig_radar, use_container_width=True)
    
    with tab2:
        st.subheader(f"{metrica_selecionada} por Idade")
        fig_idade = px.line(
            df_erros_filtrado,
            x='idade',
            y='valor',
            color='modelo',
            markers=True
        )
        fig_idade.update_layout(
            yaxis_type='log',
            hovermode='x unified',
            legend=dict(orientation="h", yanchor="bottom", y=1.02, xanchor="right", x=1),
            yaxis=dict(
                tickformat=".6f",
                exponentformat="none"
            )
        )
        st.plotly_chart(fig_idade, use_container_width=True)

# 7. SEÇÃO DE PREVISÕES (GRÁFICO E ESTATÍSTICAS ORIGINAIS RESTAURADOS)
st.header(f"🔮 Análise de Previsões - Ano {ano_selecionado}")

col_prev1, col_prev2 = st.columns([2, 1])

with col_prev1:
    fig_anos = px.line(
        df_previsoes_filtrado,
        x='idade',
        y='previsto',
        color='modelo',
        markers=True
    )
    fig_anos.update_layout(
        yaxis_type='log',
        hovermode='x unified',
        legend=dict(orientation="h", yanchor="bottom", y=1.02, xanchor="right", x=1),
        yaxis=dict(
            tickformat=".3f",
            exponentformat="none"
        )
    )
    st.plotly_chart(fig_anos, use_container_width=True)

with col_prev2:
    st.subheader("📋 Estatísticas")
    
    stats = df_previsoes_filtrado.groupby('modelo')['previsto'].agg(['mean', 'std', 'min', 'max'])
    
    for modelo in stats.index:
        with st.expander(f"🔍 {modelo}"):
            st.metric("Média", f"{stats.loc[modelo, 'mean']:.2f}")
            st.metric("Desvio Padrão", f"{stats.loc[modelo, 'std']:.2f}")
            st.metric("Mínimo", f"{stats.loc[modelo, 'min']:.2f}")
            st.metric("Máximo", f"{stats.loc[modelo, 'max']:.2f}")


st.markdown("""
            <div class="footer align-center" style="text-align: center; margin-top: 50px; font-size: 0.9em; color: gray;">
                <p>
                    Desenvolvido pelo Projeto de Extensão <a href="https://sigerip.github.io/" target="_blank"><strong>SIGERIP</strong></a> | 
                    Universidade Federal da Paraíba (UFPB)
                </p>
                <p>
                    Curso oferecido durante o <a href="https://www.concicatufpb.com.br/" target="_blank">Congresso de Ciências Contábeis e Atuariais da Paraíba (CONCICAT)</a>
                </p>
            </div>
            """, unsafe_allow_html=True)