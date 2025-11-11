
-- 🔹 REGIOES

ALTER TABLE regioes DROP CONSTRAINT regioes_pkey;-- A constraint anterior é removida e uma nova é criada com nome padronizado.
ALTER TABLE regioes ADD CONSTRAINT pk_regioes PRIMARY KEY (cd_regia);-- Define a chave primária da tabela de regiões.
-- As tabelas de casos agregados por região (mensal, anual e total.
-- são conectadas à tabela de regiões através da chave estrangeira cd_regia.
-- Essa ligação assegura que cada registro de casos esteja associado a região existente no banco de dados.
ALTER TABLE casos_regiao_mes
ADD CONSTRAINT fk_regia
FOREIGN KEY (cd_regia)
REFERENCES regioes(cd_regia);

ALTER TABLE casos_regiao_ano
ADD CONSTRAINT fk_regia
FOREIGN KEY (cd_regia)
REFERENCES regioes(cd_regia);

ALTER TABLE casos_regiao_total
ADD CONSTRAINT fk_regia
FOREIGN KEY (cd_regia)
REFERENCES regioes(cd_regia);

-- ESTADOS
-- Define a chave primária da tabela de Unidades da Federação (UF),cd_uf.
ALTER TABLE uf DROP CONSTRAINT uf_pkey;  
ALTER TABLE uf ADD CONSTRAINT pk_estados PRIMARY KEY (cd_uf);
-- Cria a relação hierárquica entre estados e regiões.
-- Cada UF pertence a uma única região, representada pela chave cd_regia.
ALTER TABLE uf
ADD CONSTRAINT fk_uf_regiao
FOREIGN KEY (cd_regia)
REFERENCES regioes (cd_regia);

-- As tabelas de casos por estado, em suas três escalas temporais (mês, ano e total),
-- são vinculadas à tabela uf pela chave cd_uf,
-- assegurando que os registros de casos correspondam a estados existentes.
ALTER TABLE casos_uf_mes
ADD CONSTRAINT fk_uf
FOREIGN KEY (cd_uf)
REFERENCES uf(cd_uf);

ALTER TABLE casos_uf_ano
ADD CONSTRAINT fk_uf
FOREIGN KEY (cd_uf)
REFERENCES uf(cd_uf);

ALTER TABLE casos_uf_total
ADD CONSTRAINT fk_uf
FOREIGN KEY (cd_uf)
REFERENCES uf(cd_uf);

-- REGIOES INTERMEDIARIAS
-- Define a chave primária das regiões intermediárias (cd_rgint)
-- e estabelece sua ligação hierárquica com a tabela de UFs.
ALTER TABLE regioes_intermediarias DROP CONSTRAINT regio_intermediaria_pkey;
ALTER TABLE regioes_intermediarias
ADD CONSTRAINT pk_rgint PRIMARY KEY (cd_rgint);

ALTER TABLE regioes_intermediarias
ADD CONSTRAINT fk_rgi_uf
FOREIGN KEY (cd_uf)
REFERENCES uf (cd_uf);

-- As tabelas de casos agregados por região intermediária (mensal, anual e total)
-- são conectadas à tabela correspondente, assegurando integridade referencial
-- e coerência com a estrutura hierárquica.
ALTER TABLE casos_rgint_mes
ADD CONSTRAINT fk_rgint
FOREIGN KEY (cd_rgint)
REFERENCES regioes_intermediarias(cd_rgint);

ALTER TABLE casos_rgint_ano
ADD CONSTRAINT fk_rgint
FOREIGN KEY (cd_rgint)
REFERENCES regioes_intermediarias(cd_rgint);

ALTER TABLE casos_rgint_total
ADD CONSTRAINT fk_rgint
FOREIGN KEY (cd_rgint)
REFERENCES regioes_intermediarias(cd_rgint);

-- REGIOES IMEDIATAS

-- Define a chave primária da tabela de regiões imediatas (cd_rgi)
-- e a vincula à tabela de regiões intermediárias,
-- respeitando a hierarquia territorial estabelecida pelo IBGE.
ALTER TABLE regioes_imediatas
ADD CONSTRAINT pk_rgi PRIMARY KEY (cd_rgi);

ALTER TABLE regioes_imediatas
ADD CONSTRAINT fk_imediatas_intermediaria
FOREIGN KEY (cd_rgint)
REFERENCES regioes_intermediarias (cd_rgint);

-- As tabelas de casos por região imediata (mês, ano e total)
-- referenciam a tabela principal de regiões imediatas.
-- Essa ligação garante que todos os registros de casos estejam associados
-- a regiões válidas no nível imediato.
ALTER TABLE casos_rgi_mes
ADD CONSTRAINT fk_rgi
FOREIGN KEY (cd_rgi)
REFERENCES regioes_imediatas(cd_rgi);

ALTER TABLE casos_rgi_ano
ADD CONSTRAINT fk_rgi
FOREIGN KEY (cd_rgi)
REFERENCES regioes_imediatas(cd_rgi);

ALTER TABLE casos_rgi_total
ADD CONSTRAINT fk_rgi
FOREIGN KEY (cd_rgi)
REFERENCES regioes_imediatas(cd_rgi);

-- MUNICIPIOS
-- Cada município é vinculado a uma região imediata, por meio da chave cd_rgi,
-- seguindo a hierarquia territorial definida pelo IBGE.
ALTER TABLE municipios
ADD CONSTRAINT fk_municipios_rgiim
FOREIGN KEY (cd_rgi)           
REFERENCES regioes_imediatas (cd_rgi);

-- As tabelas de casos por município (mensal, anual e total)
-- são ligadas à tabela de municípios através do código geográfico (geo_cod).
-- Essa referência garante a correspondência entre os registros de casos
-- e os municípios cadastrados no banco.
ALTER TABLE casos_municipios_mes
ADD CONSTRAINT fk_municipio
FOREIGN KEY (geo_cod)
REFERENCES municipios(geo_cod)

ALTER TABLE casos_municipios_ano
ADD CONSTRAINT fk_municipio
FOREIGN KEY (geo_cod)
REFERENCES municipios(geo_cod)

ALTER TABLE casos_municipios_total
ADD CONSTRAINT fk_municipio
FOREIGN KEY (geo_cod)
REFERENCES municipios(geo_cod)

