-- 🔹 REGIÕES

ALTER TABLE regioes DROP CONSTRAINT regioes_pkey;  -- Remove a PK antiga para padronizar os nomes das constraints
ALTER TABLE regioes ADD CONSTRAINT pk_regioes PRIMARY KEY (cd_regia);  -- Define cd_regia como chave primária oficial da tabela

-- Tabelas de casos são vinculadas à tabela regioes para garantir que todos os registros
-- se refiram a regiões existentes e válidas no banco.
ALTER TABLE casos_regiao_mes
ADD CONSTRAINT fk_regia FOREIGN KEY (cd_regia) REFERENCES regioes(cd_regia);  -- FK garante integridade no nível regional (mensal)

ALTER TABLE casos_regiao_ano
ADD CONSTRAINT fk_regia FOREIGN KEY (cd_regia) REFERENCES regioes(cd_regia);  -- FK para escala anual

ALTER TABLE casos_regiao_total
ADD CONSTRAINT fk_regia FOREIGN KEY (cd_regia) REFERENCES regioes(cd_regia);  -- FK para agregação total


-- 🔹 ESTADOS (UF)

ALTER TABLE uf DROP CONSTRAINT uf_pkey;  -- Remove PK antiga
ALTER TABLE uf ADD CONSTRAINT pk_estados PRIMARY KEY (cd_uf);  -- Define cd_uf como PK com nome padronizado

ALTER TABLE uf
ADD CONSTRAINT fk_uf_regiao FOREIGN KEY (cd_regia) REFERENCES regioes (cd_regia);  -- Relaciona cada UF à sua região, seguindo a hierarquia IBGE

-- Vincula tabelas de casos à tabela UF, garantindo que os registros pertençam a estados válidos
ALTER TABLE casos_uf_mes
ADD CONSTRAINT fk_uf FOREIGN KEY (cd_uf) REFERENCES uf(cd_uf);  -- FK para dados mensais de UF

ALTER TABLE casos_uf_ano
ADD CONSTRAINT fk_uf FOREIGN KEY (cd_uf) REFERENCES uf(cd_uf);  -- FK para dados anuais de UF

ALTER TABLE casos_uf_total
ADD CONSTRAINT fk_uf FOREIGN KEY (cd_uf) REFERENCES uf(cd_uf);  -- FK para dados totais por UF


-- 🔹 REGIÕES INTERMEDIÁRIAS

ALTER TABLE regioes_intermediarias DROP CONSTRAINT regio_intermediaria_pkey;  -- Remove PK antiga
ALTER TABLE regioes_intermediarias
ADD CONSTRAINT pk_rgint PRIMARY KEY (cd_rgint);  -- cd_rgint passa a ser a PK oficial

ALTER TABLE regioes_intermediarias
ADD CONSTRAINT fk_rgi_uf FOREIGN KEY (cd_uf) REFERENCES uf (cd_uf);  -- Associa cada região intermediária à sua UF correspondente

-- Associação das tabelas de casos ao nível intermediário
ALTER TABLE casos_rgint_mes
ADD CONSTRAINT fk_rgint FOREIGN KEY (cd_rgint) REFERENCES regioes_intermediarias(cd_rgint);  -- FK mensal

ALTER TABLE casos_rgint_ano
ADD CONSTRAINT fk_rgint FOREIGN KEY (cd_rgint) REFERENCES regioes_intermediarias(cd_rgint);  -- FK anual

ALTER TABLE casos_rgint_total
ADD CONSTRAINT fk_rgint FOREIGN KEY (cd_rgint) REFERENCES regioes_intermediarias(cd_rgint);  -- FK total


-- 🔹 REGIÕES IMEDIATAS

ALTER TABLE regioes_imediatas
ADD CONSTRAINT pk_rgi PRIMARY KEY (cd_rgi);  -- Define cd_rgi como PK oficial da tabela

ALTER TABLE regioes_imediatas
ADD CONSTRAINT fk_imediatas_intermediaria FOREIGN KEY (cd_rgint) REFERENCES regioes_intermediarias (cd_rgint);  -- Liga regiões imediatas às intermediárias

-- Relacionamento das tabelas de casos ao nível imediato
ALTER TABLE casos_rgi_mes
ADD CONSTRAINT fk_rgi FOREIGN KEY (cd_rgi) REFERENCES regioes_imediatas(cd_rgi);  -- FK mensal

ALTER TABLE casos_rgi_ano
ADD CONSTRAINT fk_rgi FOREIGN KEY (cd_rgi) REFERENCES regioes_imediatas(cd_rgi);  -- FK anual

ALTER TABLE casos_rgi_total
ADD CONSTRAINT fk_rgi FOREIGN KEY (cd_rgi) REFERENCES regioes_imediatas(cd_rgi);  -- FK total


-- 🔹 MUNICÍPIOS

ALTER TABLE municipios
ADD CONSTRAINT fk_municipios_rgiim FOREIGN KEY (cd_rgi) REFERENCES regioes_imediatas (cd_rgi);  -- Município vinculado à região imediata conforme hierarquia IBGE

-- Vincula tabelas de casos aos municípios usando geo_cod (código geográfico do IBGE)
ALTER TABLE casos_municipios_mes
ADD CONSTRAINT fk_municipio FOREIGN KEY (geo_cod) REFERENCES municipios(geo_cod);  -- FK mensal

ALTER TABLE casos_municipios_ano
ADD CONSTRAINT fk_municipio FOREIGN KEY (geo_cod) REFERENCES municipios(geo_cod);  -- FK anual

ALTER TABLE casos_municipios_total
ADD CONSTRAINT fk_municipio FOREIGN KEY (geo_cod) REFERENCES municipios(geo_cod);  -- FK total