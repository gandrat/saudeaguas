--Municípios


-- Cria uma tabela com casos agregados por mês em nível municipal
CREATE TABLE casos_municipios_mes AS
SELECT 
    m.geo_cod, --Dados necessário para a tabela
    m.nm_mun,
    m.cd_rgi,
    m.nm_rgi,
    m.cd_rgint,
    m.nm_rgint,
    m.cd_uf,
    m.nm_uf,
    m.cd_regia,
    m.nm_regia,
    c.id_doenca,
    d.nome_cientifico AS doenca,
    DATE_TRUNC('month', c.data) AS data,
	EXTRACT(month FROM c.data) AS mes, -- Extrai o número do mês
    SUM(c.casos) AS total_casos, -- Soma dos casos no mês
    m.pop AS pop_total,
    (SUM(c.casos) * 100000.0 / m.pop) AS prev  -- Calcula prevalência por 100 mil habitantes
FROM casos c
JOIN municipios m USING (geo_cod) -- Faz join de casos e municipios pela chave geo_cod
JOIN doencas d USING (id_doenca)  -- Junta a tabela de doenças usando id_doenca
GROUP BY  -- Agrupa os registros pelos campos selecionados, permitindo calcular valores agregados por categoria
    m.geo_cod, m.nm_mun,
    m.cd_rgi, m.nm_rgi,
    m.cd_rgint, m.nm_rgint,
    m.cd_uf, m.nm_uf,
    m.cd_regia, m.nm_regia,
    c.id_doenca, d.nome_cientifico,  DATE_TRUNC('month', c.data),EXTRACT(month FROM c.data), m.pop;
	
-- Cria view com geometria adicionada aos dados agregados	
CREATE OR REPLACE VIEW casos_municipios_mes_g AS
SELECT
    u.*,  -- Todos os atributos da tabela criada
    e.geom  -- Geometria do município
FROM casos_municipios_mes u
JOIN municipios e ON u.geo_cod = e.geo_cod;	


-- Cria uma tabela com casos agregados por ano em nível municipal	
	CREATE TABLE casos_municipios_ano AS
SELECT 
    m.geo_cod,--Dados necessário para a tabela
    m.nm_mun,
    m.cd_rgi,
    m.nm_rgi,
    m.cd_rgint,
    m.nm_rgint,
    m.cd_uf,
    m.nm_uf,
    m.cd_regia,
    m.nm_regia,
    c.id_doenca,
    d.nome_cientifico AS doenca,
    EXTRACT(YEAR FROM c.data) AS ano, --extrai o ano
    SUM(c.casos) AS total_casos, -- Soma dos casos no ano
    m.pop AS pop_total,
    (SUM(c.casos) * 100000.0 / m.pop) AS prev -- Calcula prevalência por 100 mil habitantes
FROM casos c
JOIN municipios m USING (geo_cod) -- Faz join de casos e municipios pela chave geo_cod
JOIN doencas d USING (id_doenca)  -- Junta a tabela de doenças usando id_doenca
GROUP BY  -- Agrupa os registros pelos campos selecionados, permitindo calcular valores agregados por categoria
    m.geo_cod, m.nm_mun,
    m.cd_rgi, m.nm_rgi,
    m.cd_rgint, m.nm_rgint,
    m.cd_uf, m.nm_uf,
    m.cd_regia, m.nm_regia,
    c.id_doenca, d.nome_cientifico,
    EXTRACT(YEAR FROM c.data), m.pop;

	
-- Cria view com geometria adicionada aos dados agregados	
CREATE OR REPLACE VIEW casos_municipios_ano_g AS
SELECT
    u.*, -- Todos os atributos da tabela criada
    e.geom  -- Geometria do município
FROM casos_municipios_ano u 
JOIN municipios e ON u.geo_cod = e.geo_cod;


-- Cria uma tabela com casos totais em nível municipal
	CREATE TABLE casos_municipios_total AS
SELECT 
    m.geo_cod, --Dados necessário para a tabela
    m.nm_mun,
    m.cd_rgi,
    m.nm_rgi,
    m.cd_rgint,
    m.nm_rgint,
    m.cd_uf,
    m.nm_uf,
    m.cd_regia,
    m.nm_regia,
    c.id_doenca,
    d.nome_cientifico AS doenca,
    SUM(c.casos) AS total_casos, -- Soma dos casos totais
    m.pop AS pop_total,
    (SUM(c.casos) * 100000.0 / m.pop) AS prev -- Calcula prevalência por 100 mil habitantes
FROM casos c
JOIN municipios m USING (geo_cod) -- Faz join de casos e municipios pela chave geo_cod
JOIN doencas d USING (id_doenca)  -- Junta a tabela de doenças usando id_doenca
GROUP BY  -- Agrupa os registros pelos campos selecionados, permitindo calcular valores agregados por categoria
    m.geo_cod, m.nm_mun,
    m.cd_rgi, m.nm_rgi,
    m.cd_rgint, m.nm_rgint,
    m.cd_uf, m.nm_uf,
    m.cd_regia, m.nm_regia,
    c.id_doenca, d.nome_cientifico, m.pop;

	
-- Cria view com geometria adicionada aos dados agregados	
	CREATE OR REPLACE VIEW casos_municipios_total_g AS
SELECT
    u.*, -- Todos os atributos da tabela criada
    e.geom  -- Geometria do município
FROM casos_municipios_total u
JOIN municipios e ON u.geo_cod = e.geo_cod;

--Regiões Imediatas 
-- Cria uma tabela com casos agregados por mês em nível das regioes imediatas
CREATE TABLE casos_rgi_mes AS
SELECT 
    m.cd_rgi, --Dados necessário para a tabela
    m.nm_rgi,
    m.cd_rgint,
    m.nm_rgint,
    m.cd_uf,
    m.nm_uf,
    m.cd_regia,
    m.nm_regia,
    c.id_doenca,
    d.nome_cientifico AS doenca,
    DATE_TRUNC('month', c.data) AS data,
	EXTRACT(month FROM c.data)::int AS mes, -- Extrai o número do mês
    SUM(c.casos) AS total_casos, -- Soma dos casos no mês
    pop.pop_total,
    (SUM(c.casos) * 100000.0 / pop.pop_total) AS prev -- Calcula prevalência por 100 mil habitantes
FROM casos c
JOIN municipios m USING (geo_cod) -- Faz join de casos e municipios pela chave geo_cod
JOIN doencas d USING (id_doenca)  -- Junta a tabela de doenças usando id_doenca
JOIN (
    SELECT cd_rgi, SUM(pop) AS pop_total
    FROM municipios
    GROUP BY cd_rgi
) AS pop USING (cd_rgi)
GROUP BY  -- Agrupa os registros pelos campos selecionados, permitindo calcular valores agregados por categoria
    m.cd_rgi, m.nm_rgi, m.cd_rgint, m.nm_rgint, 
    m.cd_uf, m.nm_uf, m.cd_regia, m.nm_regia,
    c.id_doenca, d.nome_cientifico,  DATE_TRUNC('month', c.data),EXTRACT(month FROM c.data), pop.pop_total;



-- Cria view com geometria adicionada aos dados agregados	
CREATE OR REPLACE VIEW casos_rgi_mes_g AS
SELECT
    u.*, -- Todos os atributos da tabela criada
    e.geom  -- Geometria da rgi
FROM casos_rgi_mes u
JOIN regioes_imediatas e ON u.cd_rgi = e.cd_rgi;
	

-- Cria uma tabela com casos agregados por ano em nível das regioes imediatas	
	CREATE TABLE casos_rgi_ano AS
SELECT 
    m.cd_rgi, --Dados necessário para a tabela
    m.nm_rgi,
    m.cd_rgint,
    m.nm_rgint,
    m.cd_uf,
    m.nm_uf,
    m.cd_regia,
    m.nm_regia,
    c.id_doenca,
    d.nome_cientifico AS doenca,
    EXTRACT(YEAR FROM c.data) AS ano, --extrai o ano
    SUM(c.casos) AS total_casos, -- Soma dos casos no ano
    pop.pop_total,
    (SUM(c.casos) * 100000.0 / pop.pop_total) AS prev -- Calcula prevalência por 100 mil habitantes
FROM casos c
JOIN municipios m USING (geo_cod) -- Faz join de casos e municipios pela chave geo_cod
JOIN doencas d USING (id_doenca)  -- Junta a tabela de doenças usando id_doenca
JOIN (
    SELECT cd_rgi, SUM(pop) AS pop_total
    FROM municipios
    GROUP BY cd_rgi
) AS pop USING (cd_rgi)
GROUP BY  -- Agrupa os registros pelos campos selecionados, permitindo calcular valores agregados por categoria
    m.cd_rgi, m.nm_rgi, m.cd_rgint, m.nm_rgint, 
    m.cd_uf, m.nm_uf, m.cd_regia, m.nm_regia,
    c.id_doenca, d.nome_cientifico, EXTRACT(YEAR FROM c.data), pop.pop_total;

	

	
-- Cria view com geometria adicionada aos dados agregados	
	CREATE OR REPLACE VIEW casos_rgi_ano_g AS
SELECT
    u.*, -- Todos os atributos da tabela criada
    e.geom -- Geometria da rgi
FROM casos_rgi_ano u
JOIN regioes_imediatas e ON u.cd_rgi = e.cd_rgi;


-- Cria uma tabela com casos totais em nível das regioes imediatas
	CREATE TABLE casos_rgi_total AS
SELECT 
    m.cd_rgi, --Dados necessário para a tabela
    m.nm_rgi,
    m.cd_rgint,
    m.nm_rgint,
    m.cd_uf,
    m.nm_uf,
    m.cd_regia,
    m.nm_regia,
    c.id_doenca,
    d.nome_cientifico AS doenca,
    SUM(c.casos) AS total_casos, -- Soma dos casos totais
    pop.pop_total,
    (SUM(c.casos) * 100000.0 / pop.pop_total) AS prev -- Calcula prevalência por 100 mil habitantes
FROM casos c
JOIN municipios m USING (geo_cod) -- Faz join de casos e municipios pela chave geo_cod
JOIN doencas d USING (id_doenca)  -- Junta a tabela de doenças usando id_doenca
JOIN (
    SELECT cd_rgi, SUM(pop) AS pop_total
    FROM municipios
    GROUP BY cd_rgi
) AS pop USING (cd_rgi)
GROUP BY  -- Agrupa os registros pelos campos selecionados, permitindo calcular valores agregados por categoria
    m.cd_rgi, m.nm_rgi, m.cd_rgint, m.nm_rgint, 
    m.cd_uf, m.nm_uf, m.cd_regia, m.nm_regia,
    c.id_doenca, d.nome_cientifico, pop.pop_total;
	
-- Cria view com geometria adicionada aos dados agregados		
CREATE OR REPLACE VIEW casos_rgi_total_g AS
SELECT
    u.*, -- Todos os atributos da tabela criada
    e.geom -- Geometria da rgi
FROM casos_rgi_total u
JOIN regioes_imediatas e ON u.cd_rgi = e.cd_rgi;	


	
-- Regiões Intermediárias
-- Cria uma tabela com casos agregados por mês em nível das regioes intermediarias
	CREATE TABLE casos_rgint_mes AS
SELECT 
    m.cd_rgint, --Dados necessário para a tabela
    m.nm_rgint,
    m.cd_uf,
    m.nm_uf,
    m.cd_regia,
    m.nm_regia,
    c.id_doenca,
    d.nome_cientifico AS doenca,
    DATE_TRUNC('month', c.data) AS data,
	EXTRACT(month FROM c.data)::int AS mes, -- Extrai o número do mês
    SUM(c.casos) AS total_casos, -- Soma dos casos no mês
    pop.pop_total,
    (SUM(c.casos) * 100000.0 / pop.pop_total) AS prev -- Calcula prevalência por 100 mil habitantes
FROM casos c
JOIN municipios m USING (geo_cod) -- Faz join de casos e municipios pela chave geo_cod
JOIN doencas d USING (id_doenca)  -- Junta a tabela de doenças usando id_doenca
JOIN (
    SELECT cd_rgint, SUM(pop) AS pop_total
    FROM municipios
    GROUP BY cd_rgint
) AS pop USING (cd_rgint)
GROUP BY  -- Agrupa os registros pelos campos selecionados, permitindo calcular valores agregados por categoria
    m.cd_rgint, m.nm_rgint,
    m.cd_uf, m.nm_uf,
    m.cd_regia, m.nm_regia,
    c.id_doenca, d.nome_cientifico,  DATE_TRUNC('month', c.data),EXTRACT(month FROM c.data), pop.pop_total;
	
-- Cria view com geometria adicionada aos dados agregados			
	CREATE OR REPLACE VIEW casos_rgint_mes_g AS
SELECT
    u.*, -- Todos os atributos da tabela criada
    e.geom -- Geometria da rgint
FROM casos_rgint_mes u
JOIN regioes_intermediarias e ON u.cd_rgint = e.cd_rgint;	




-- Cria uma tabela com casos agregados por ano em nível das regioes intermediarias
	CREATE TABLE casos_rgint_ano AS
SELECT 
    m.cd_rgint, --Dados necessário para a tabela
    m.nm_rgint,
    m.cd_uf,
    m.nm_uf,
    m.cd_regia,
    m.nm_regia,
    c.id_doenca,
    d.nome_cientifico AS doenca,
    EXTRACT(YEAR FROM c.data)::int AS ano, --extrai o ano
    SUM(c.casos) AS total_casos, -- Soma dos casos no ano
    pop.pop_total,
    (SUM(c.casos) * 100000.0 / pop.pop_total) AS prev -- Calcula prevalência por 100 mil habitantes
FROM casos c
JOIN municipios m USING (geo_cod) -- Faz join de casos e municipios pela chave geo_cod
JOIN doencas d USING (id_doenca)  -- Junta a tabela de doenças usando id_doenca
JOIN (
    SELECT cd_rgint, SUM(pop) AS pop_total
    FROM municipios
    GROUP BY cd_rgint
) AS pop USING (cd_rgint)
GROUP BY  -- Agrupa os registros pelos campos selecionados, permitindo calcular valores agregados por categoria
    m.cd_rgint, m.nm_rgint,
    m.cd_uf, m.nm_uf,
    m.cd_regia, m.nm_regia,
    c.id_doenca, d.nome_cientifico,
    EXTRACT(YEAR FROM c.data), pop.pop_total;
	
-- Cria view com geometria adicionada aos dados agregados		
CREATE OR REPLACE VIEW casos_rgint_ano_g AS
SELECT
    u.*, -- Todos os atributos da tabela criada
    e.geom -- Geometria da rgint
FROM casos_rgint_ano u
JOIN regioes_intermediarias e ON u.cd_rgint = e.cd_rgint;	


-- Cria uma tabela com casos totais em nível das regioes intermediarias
CREATE TABLE casos_rgint_total AS
SELECT 
    m.cd_rgint, --Dados necessário para a tabela
    m.nm_rgint,
    m.cd_uf,
    m.nm_uf,
    m.cd_regia,
    m.nm_regia,
    c.id_doenca,
    d.nome_cientifico AS doenca,
    SUM(c.casos) AS total_casos, -- Soma dos casos totais
    pop.pop_total,
    (SUM(c.casos) * 100000.0 / pop.pop_total) AS prev -- Calcula prevalência por 100 mil habitantes
FROM casos c
JOIN municipios m USING (geo_cod) -- Faz join de casos e municipios pela chave geo_cod
JOIN doencas d USING (id_doenca)  -- Junta a tabela de doenças usando id_doenca
JOIN (
    SELECT cd_rgint, SUM(pop) AS pop_total
    FROM municipios
    GROUP BY cd_rgint
) AS pop USING (cd_rgint)
GROUP BY  -- Agrupa os registros pelos campos selecionados, permitindo calcular valores agregados por categoria
    m.cd_rgint, m.nm_rgint,
    m.cd_uf, m.nm_uf,
    m.cd_regia, m.nm_regia,
    c.id_doenca, d.nome_cientifico, pop.pop_total;
	

	
-- Cria view com geometria adicionada aos dados agregados	
	CREATE OR REPLACE VIEW casos_rgint_total_g AS
SELECT
    u.*, -- Todos os atributos da tabela criada
    e.geom -- Geometria da rgint
FrOM casos_rgint_total u
JOIN regioes_intermediarias e ON u.cd_rgint = e.cd_rgint;


-- Unidades Federativas
-- Cria uma tabela com casos agregados por mês em nível das UF
CREATE TABLE casos_uf_mes AS
SELECT 
    m.cd_uf, --Dados necessário para a tabela
    m.nm_uf,
    m.cd_regia,
    m.nm_regia,
    c.id_doenca,
    d.nome_cientifico AS doenca,
    DATE_TRUNC('month', c.data) AS data,
	EXTRACT(month FROM c.data)::int AS mes, -- Extrai o número do mês
    SUM(c.casos) AS total_casos, -- Soma dos casos no mês
    pop.pop_total,
    (SUM(c.casos) * 100000.0 / pop.pop_total) AS prev -- Calcula prevalência por 100 mil habitantes
FROM casos c
JOIN municipios m USING (geo_cod) -- Faz join de casos e municipios pela chave geo_cod
JOIN doencas d USING (id_doenca)  -- Junta a tabela de doenças usando id_doenca
JOIN (
    SELECT cd_uf, SUM(pop) AS pop_total
    FROM municipios
    GROUP BY cd_uf
) AS pop USING (cd_uf)
GROUP BY  -- Agrupa os registros pelos campos selecionados, permitindo calcular valores agregados por categoria
    m.cd_uf, m.nm_uf,
    m.cd_regia, m.nm_regia,
    c.id_doenca, d.nome_cientifico, DATE_TRUNC('month', c.data), EXTRACT(month FROM c.data), pop.pop_total;
	
	
-- Cria view com geometria adicionada aos dados agregados	
	CREATE OR REPLACE VIEW casos_uf_mes_g AS
SELECT
    u.*, -- Todos os atributos da tabela criada
    e.geom -- Geometria da uf
FROM casos_uf_mes u
JOIN uf e ON u.cd_uf = e.cd_uf;
	
-- Cria uma tabela com casos agregados por ano em nível das UF	
	CREATE TABLE casos_uf_ano AS
SELECT 
    m.cd_uf, --Dados necessário para a tabela
    m.nm_uf,
    m.cd_regia,
    m.nm_regia,
    c.id_doenca,
    d.nome_cientifico AS doenca,
    EXTRACT(YEAR FROM c.data)::int AS ano, --extrai o ano
    SUM(c.casos) AS total_casos, -- Soma dos casos no ano
    pop.pop_total,
    (SUM(c.casos) * 100000.0 / pop.pop_total) AS prev -- Calcula prevalência por 100 mil habitantes
FROM casos c
JOIN municipios m USING (geo_cod) -- Faz join de casos e municipios pela chave geo_cod
JOIN doencas d USING (id_doenca)  -- Junta a tabela de doenças usando id_doenca
JOIN (
    SELECT cd_uf, SUM(pop) AS pop_total
    FROM municipios
    GROUP BY cd_uf
) AS pop USING (cd_uf)
GROUP BY  -- Agrupa os registros pelos campos selecionados, permitindo calcular valores agregados por categoria
    m.cd_uf, m.nm_uf,
    m.cd_regia, m.nm_regia,
    c.id_doenca, d.nome_cientifico,
    EXTRACT(YEAR FROM c.data), pop.pop_total;
	
	-- Cria view com geometria adicionada aos dados agregados	
	CREATE OR REPLACE VIEW casos_uf_ano_g AS
SELECT
    u.*, -- Todos os atributos da tabela criada
    e.geom -- Geometria da uf
FROM casos_uf_ano u
JOIN uf e ON u.cd_uf = e.cd_uf;
	
-- Cria uma tabela com casos totais em nível das UF	
	CREATE TABLE casos_uf_total AS
SELECT 
    m.cd_uf, --Dados necessário para a tabela
    m.nm_uf,
    m.cd_regia,
    m.nm_regia,
    c.id_doenca,
    d.nome_cientifico AS doenca,
    SUM(c.casos) AS total_casos, -- Soma dos casos totais
    pop.pop_total,
    (SUM(c.casos) * 100000.0 / pop.pop_total) AS prev -- Calcula prevalência por 100 mil habitantes
FROM casos c
JOIN municipios m USING (geo_cod) -- Faz join de casos e municipios pela chave geo_cod
JOIN doencas d USING (id_doenca)  -- Junta a tabela de doenças usando id_doenca
JOIN (
    SELECT cd_uf, SUM(pop) AS pop_total
    FROM municipios
    GROUP BY cd_uf
) AS pop USING (cd_uf)
GROUP BY  -- Agrupa os registros pelos campos selecionados, permitindo calcular valores agregados por categoria
    m.cd_uf, m.nm_uf,
    m.cd_regia, m.nm_regia,
    c.id_doenca, d.nome_cientifico, pop.pop_total;
	
-- Cria view com geometria adicionada aos dados agregados	
	CREATE OR REPLACE VIEW casos_uf_total_g AS
SELECT
    u.*, -- Todos os atributos da tabela criada
    e.geom -- Geometria da uf
FROM casos_uf_total u
JOIN uf e ON u.cd_uf = e.cd_uf;
	

	

--Regiões
-- Cria uma tabela com casos agregados por mês em nível das Regiões
CREATE TABLE casos_regiao_mes AS
SELECT 
    m.cd_regia, --Dados necessário para a tabela
    m.nm_regia,
    c.id_doenca,
    d.nome_cientifico AS doenca,
    DATE_TRUNC('month', c.data) AS data,
	EXTRACT(month FROM c.data)::int AS mes, -- Extrai o número do mês
    SUM(c.casos) AS total_casos, -- Soma dos casos no mês
    pop.pop_total,
    (SUM(c.casos) * 100000.0 / pop.pop_total) AS prev -- Calcula prevalência por 100 mil habitantes
FROM casos c
JOIN municipios m USING (geo_cod) -- Faz join de casos e municipios pela chave geo_cod
JOIN doencas d USING (id_doenca)  -- Junta a tabela de doenças usando id_doenca
JOIN (
    SELECT cd_regia, SUM(pop) AS pop_total
    FROM municipios
    GROUP BY cd_regia
) AS pop USING (cd_regia)
GROUP BY  -- Agrupa os registros pelos campos selecionados, permitindo calcular valores agregados por categoria
    m.cd_regia, m.nm_regia,
    c.id_doenca, d.nome_cientifico,DATE_TRUNC('month', c.data),EXTRACT(month FROM c.data), pop.pop_total;

	-- Cria view com geometria adicionada aos dados agregados	
CREATE OR REPLACE VIEW casos_regiao_mes_g AS
SELECT
    u.*,-- Todos os atributos da tabela criada
    r.geom -- Geometria da Região
FROM casos_regiao_mes u
JOIN regioes r ON u.cd_regia = r.cd_regia;


-- Cria uma tabela com casos agregados por ano em nível das Regiões
CREATE TABLE casos_regiao_ano AS
SELECT 
    m.cd_regia, --Dados necessário para a tabela
    m.nm_regia,
    c.id_doenca,
    d.nome_cientifico AS doenca,
    EXTRACT(YEAR FROM c.data)::int AS ano, --Extrai o ano
    SUM(c.casos) AS total_casos, -- Soma dos casos no ano
    pop.pop_total,
    (SUM(c.casos) * 100000.0 / pop.pop_total) AS prev -- Calcula prevalência por 100 mil habitantes
FROM casos c
JOIN municipios m USING (geo_cod) -- Faz join de casos e municipios pela chave geo_cod
JOIN doencas d USING (id_doenca)  -- Junta a tabela de doenças usando id_doenca
JOIN (
    SELECT cd_regia, SUM(pop) AS pop_total
    FROM municipios
    GROUP BY cd_regia
) AS pop USING (cd_regia)
GROUP BY  -- Agrupa os registros pelos campos selecionados, permitindo calcular valores agregados por categoria
    m.cd_regia, m.nm_regia,
    c.id_doenca, d.nome_cientifico,
    EXTRACT(YEAR FROM c.data), pop.pop_total;	

-- Cria view com geometria adicionada aos dados agregados		
	CREATE OR REPLACE VIEW casos_regiao_ano_g AS
SELECT
    u.*, -- Todos os atributos da tabela criada
    r.geom -- Geometria da Região
FROM casos_regiao_ano u
JOIN regioes r ON u.cd_regia = r.cd_regia;
	

	

-- Cria uma tabela com casos totais em nível das Regiões	
	CREATE TABLE casos_regiao_total AS
SELECT 
    m.cd_regia, --Dados necessário para a tabela
    m.nm_regia,
    c.id_doenca,
    d.nome_cientifico AS doenca,
    SUM(c.casos) AS total_casos, -- Soma dos casos totais
    pop.pop_total,
    (SUM(c.casos) * 100000.0 / pop.pop_total) AS prev -- Calcula prevalência por 100 mil habitantes
FROM casos c
JOIN municipios m USING (geo_cod) -- Faz join de casos e municipios pela chave geo_cod
JOIN doencas d USING (id_doenca)  -- Junta a tabela de doenças usando id_doenca
JOIN (
    SELECT cd_regia, SUM(pop) AS pop_total
    FROM municipios
    GROUP BY cd_regia
) AS pop USING (cd_regia)
GROUP BY  -- Agrupa os registros pelos campos selecionados, permitindo calcular valores agregados por categoria
    m.cd_regia, m.nm_regia,
    c.id_doenca, d.nome_cientifico, pop.pop_total;
	
	-- Cria view com geometria adicionada aos dados agregados	
	CREATE OR REPLACE VIEW casos_regiao_total_g AS
SELECT
    u.*, -- Todos os atributos da tabela criada
    r.geom -- Geometria da Região
FROM casos_regiao_total u
JOIN regioes r ON u.cd_regia = r.cd_regia;