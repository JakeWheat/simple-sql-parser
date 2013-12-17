
Here are some tests taken from the SQL in the postgres manual. Almost
all of the postgres specific syntax has been skipped, this can be
revisited when the dialect support is added.

> module Language.SQL.SimpleSQL.Postgres (postgresTests) where

> import Language.SQL.SimpleSQL.TestTypes
> --import Language.SQL.SimpleSQL.Syntax

> postgresTests :: TestItem
> postgresTests = Group "postgresTests" $ map ParseQueryExpr $

lexical syntax section

TODO: get all the commented out tests working

>     [-- "SELECT 'foo'\n\
>     -- \'bar';" -- this should parse as select 'foobar'
>     -- ,
>      "SELECT name, (SELECT max(pop) FROM cities WHERE cities.state = states.name)\n\
>      \    FROM states;"
>     ,"SELECT ROW(1,2.5,'this is a test');"

>     ,"SELECT ROW(t.*, 42) FROM t;"
>     ,"SELECT ROW(t.f1, t.f2, 42) FROM t;"
>     ,"SELECT getf1(CAST(ROW(11,'this is a test',2.5) AS myrowtype));"

>     ,"SELECT ROW(1,2.5,'this is a test') = ROW(1, 3, 'not the same');"

>     ,"SELECT ROW(table.*) IS NULL FROM table;"

>     ,"SELECT true OR somefunc();"

>     ,"SELECT somefunc() OR true;"

queries section

>     ,"SELECT * FROM t1 CROSS JOIN t2;"
>     ,"SELECT * FROM t1 INNER JOIN t2 ON t1.num = t2.num;"
>     ,"SELECT * FROM t1 INNER JOIN t2 USING (num);"
>     ,"SELECT * FROM t1 NATURAL INNER JOIN t2;"
>     ,"SELECT * FROM t1 LEFT JOIN t2 ON t1.num = t2.num;"
>     ,"SELECT * FROM t1 LEFT JOIN t2 USING (num);"
>     ,"SELECT * FROM t1 RIGHT JOIN t2 ON t1.num = t2.num;"
>     ,"SELECT * FROM t1 FULL JOIN t2 ON t1.num = t2.num;"
>     ,"SELECT * FROM t1 LEFT JOIN t2 ON t1.num = t2.num AND t2.value = 'xxx';"
>     ,"SELECT * FROM t1 LEFT JOIN t2 ON t1.num = t2.num WHERE t2.value = 'xxx';"

>     ,"SELECT * FROM some_very_long_table_name s JOIN another_fairly_long_name a ON s.id = a.num;"
>     ,"SELECT * FROM people AS mother JOIN people AS child ON mother.id = child.mother_id;"
>     ,"SELECT * FROM my_table AS a CROSS JOIN my_table AS b;"
>     ,"SELECT * FROM (my_table AS a CROSS JOIN my_table) AS b;"
>     ,"SELECT * FROM getfoo(1) AS t1;"
>     ,"SELECT * FROM foo\n\
>      \    WHERE foosubid IN (\n\
>      \                        SELECT foosubid\n\
>      \                        FROM getfoo(foo.fooid) z\n\
>      \                        WHERE z.fooid = foo.fooid\n\
>      \                      );"
>     {-,"SELECT *\n\
>      \    FROM dblink('dbname=mydb', 'SELECT proname, prosrc FROM pg_proc')\n\
>      \      AS t1(proname name, prosrc text)\n\
>      \    WHERE proname LIKE 'bytea%';"-} -- types in the alias??

>     ,"SELECT * FROM foo, LATERAL (SELECT * FROM bar WHERE bar.id = foo.bar_id) ss;" -- lateral
>     ,"SELECT * FROM foo, bar WHERE bar.id = foo.bar_id;"

>     {-,"SELECT p1.id, p2.id, v1, v2\n\
>      \FROM polygons p1, polygons p2,\n\
>      \     LATERAL vertices(p1.poly) v1,\n\
>      \     LATERAL vertices(p2.poly) v2\n\
>      \WHERE (v1 <-> v2) < 10 AND p1.id != p2.id;"-} -- <-> operator?

>     {-,"SELECT p1.id, p2.id, v1, v2\n\
>      \FROM polygons p1 CROSS JOIN LATERAL vertices(p1.poly) v1,\n\
>      \     polygons p2 CROSS JOIN LATERAL vertices(p2.poly) v2\n\
>      \WHERE (v1 <-> v2) < 10 AND p1.id != p2.id;"-}

>     ,"SELECT m.name\n\
>      \FROM manufacturers m LEFT JOIN LATERAL get_product_names(m.id) pname ON true\n\
>      \WHERE pname IS NULL;"


>     ,"SELECT * FROM fdt WHERE c1 > 5"

>     ,"SELECT * FROM fdt WHERE c1 IN (1, 2, 3)"

>     ,"SELECT * FROM fdt WHERE c1 IN (SELECT c1 FROM t2)"

>     ,"SELECT * FROM fdt WHERE c1 IN (SELECT c3 FROM t2 WHERE c2 = fdt.c1 + 10)"

>     ,"SELECT * FROM fdt WHERE c1 BETWEEN (SELECT c3 FROM t2 WHERE c2 = fdt.c1 + 10) AND 100"

>     ,"SELECT * FROM fdt WHERE EXISTS (SELECT c1 FROM t2 WHERE c2 > fdt.c1)"

>     ,"SELECT * FROM test1;"

>     ,"SELECT x FROM test1 GROUP BY x;"
>     ,"SELECT x, sum(y) FROM test1 GROUP BY x;"
>     ,"SELECT product_id, p.name, (sum(s.units) * p.price) AS sales\n\
>      \    FROM products p LEFT JOIN sales s USING (product_id)\n\
>      \    GROUP BY product_id, p.name, p.price;"

>     ,"SELECT x, sum(y) FROM test1 GROUP BY x HAVING sum(y) > 3;"
>     ,"SELECT x, sum(y) FROM test1 GROUP BY x HAVING x < 'c';"
>     ,"SELECT product_id, p.name, (sum(s.units) * (p.price - p.cost)) AS profit\n\
>      \    FROM products p LEFT JOIN sales s USING (product_id)\n\
>      \    WHERE s.date > CURRENT_DATE - INTERVAL '4 weeks'\n\
>      \    GROUP BY product_id, p.name, p.price, p.cost\n\
>      \    HAVING sum(p.price * s.units) > 5000;"

>     ,"SELECT a, b, c FROM t"

>     ,"SELECT tbl1.a, tbl2.a, tbl1.b FROM t"

>     ,"SELECT tbl1.*, tbl2.a FROM t"

>     ,"SELECT a AS value, b + c AS sum FROM t"

>     ,"SELECT a \"value\", b + c AS sum FROM t"

>     ,"SELECT DISTINCT select_list t"

>     --,"VALUES (1, 'one'), (2, 'two'), (3, 'three');" -- values list

>     ,"SELECT 1 AS column1, 'one' AS column2\n\
>      \UNION ALL\n\
>      \SELECT 2, 'two'\n\
>      \UNION ALL\n\
>      \SELECT 3, 'three';"

>     --,"SELECT * FROM (VALUES (1, 'one'), (2, 'two'), (3, 'three')) AS t (num,letter);"
>     -- values list

>     ,"WITH regional_sales AS (\n\
>      \        SELECT region, SUM(amount) AS total_sales\n\
>      \        FROM orders\n\
>      \        GROUP BY region\n\
>      \     ), top_regions AS (\n\
>      \        SELECT region\n\
>      \        FROM regional_sales\n\
>      \        WHERE total_sales > (SELECT SUM(total_sales)/10 FROM regional_sales)\n\
>      \     )\n\
>      \SELECT region,\n\
>      \       product,\n\
>      \       SUM(quantity) AS product_units,\n\
>      \       SUM(amount) AS product_sales\n\
>      \FROM orders\n\
>      \WHERE region IN (SELECT region FROM top_regions)\n\
>      \GROUP BY region, product;"

>     {-,"WITH RECURSIVE t(n) AS (\n\
>      \    VALUES (1)\n\
>      \  UNION ALL\n\
>      \    SELECT n+1 FROM t WHERE n < 100\n\
>      \)\n\
>      \SELECT sum(n) FROM t"-} -- full alias in cte

>     {-,"WITH RECURSIVE included_parts(sub_part, part, quantity) AS (\n\
>      \    SELECT sub_part, part, quantity FROM parts WHERE part = 'our_product'\n\
>      \  UNION ALL\n\
>      \    SELECT p.sub_part, p.part, p.quantity\n\
>      \    FROM included_parts pr, parts p\n\
>      \    WHERE p.part = pr.sub_part\n\
>      \  )\n\
>      \SELECT sub_part, SUM(quantity) as total_quantity\n\
>      \FROM included_parts\n\
>      \GROUP BY sub_part"

>     ,"WITH RECURSIVE search_graph(id, link, data, depth) AS (\n\
>      \        SELECT g.id, g.link, g.data, 1\n\
>      \        FROM graph g\n\
>      \      UNION ALL\n\
>      \        SELECT g.id, g.link, g.data, sg.depth + 1\n\
>      \        FROM graph g, search_graph sg\n\
>      \        WHERE g.id = sg.link\n\
>      \)\n\
>      \SELECT * FROM search_graph;"

>     ,"WITH RECURSIVE search_graph(id, link, data, depth, path, cycle) AS (\n\
>      \        SELECT g.id, g.link, g.data, 1,\n\
>      \          ARRAY[g.id],\n\
>      \          false\n\
>      \        FROM graph g\n\
>      \      UNION ALL\n\
>      \        SELECT g.id, g.link, g.data, sg.depth + 1,\n\
>      \          path || g.id,\n\
>      \          g.id = ANY(path)\n\
>      \        FROM graph g, search_graph sg\n\
>      \        WHERE g.id = sg.link AND NOT cycle\n\
>      \)\n\
>      \SELECT * FROM search_graph;"

>     ,"WITH RECURSIVE search_graph(id, link, data, depth, path, cycle) AS (\n\
>      \        SELECT g.id, g.link, g.data, 1,\n\
>      \          ARRAY[ROW(g.f1, g.f2)],\n\
>      \          false\n\
>      \        FROM graph g\n\
>      \      UNION ALL\n\
>      \        SELECT g.id, g.link, g.data, sg.depth + 1,\n\
>      \          path || ROW(g.f1, g.f2),\n\
>      \          ROW(g.f1, g.f2) = ANY(path)\n\
>      \        FROM graph g, search_graph sg\n\
>      \        WHERE g.id = sg.link AND NOT cycle\n\
>      \)\n\
>      \SELECT * FROM search_graph;"

>     ,"WITH RECURSIVE t(n) AS (\n\
>      \    SELECT 1\n\
>      \  UNION ALL\n\
>      \    SELECT n+1 FROM t\n\
>      \)\n\
>      \SELECT n FROM t LIMIT 100;"-}

select page reference

>     ,"SELECT f.title, f.did, d.name, f.date_prod, f.kind\n\
>      \    FROM distributors d, films f\n\
>      \    WHERE f.did = d.did"

>     ,"SELECT kind, sum(len) AS total\n\
>      \    FROM films\n\
>      \    GROUP BY kind\n\
>      \    HAVING sum(len) < interval '5 hours';"

>     ,"SELECT * FROM distributors ORDER BY name;"
>     ,"SELECT * FROM distributors ORDER BY 2;"

>     ,"SELECT distributors.name\n\
>      \    FROM distributors\n\
>      \    WHERE distributors.name LIKE 'W%'\n\
>      \UNION\n\
>      \SELECT actors.name\n\
>      \    FROM actors\n\
>      \    WHERE actors.name LIKE 'W%';"

>     ,"WITH t AS (\n\
>      \    SELECT random() as x FROM generate_series(1, 3)\n\
>      \  )\n\
>      \SELECT * FROM t\n\
>      \UNION ALL\n\
>      \SELECT * FROM t"

>     {-,"WITH RECURSIVE employee_recursive(distance, employee_name, manager_name) AS (\n\
>      \    SELECT 1, employee_name, manager_name\n\
>      \    FROM employee\n\
>      \    WHERE manager_name = 'Mary'\n\
>      \  UNION ALL\n\
>      \    SELECT er.distance + 1, e.employee_name, e.manager_name\n\
>      \    FROM employee_recursive er, employee e\n\
>      \    WHERE er.employee_name = e.manager_name\n\
>      \  )\n\
>      \SELECT distance, employee_name FROM employee_recursive;"-} -- with recursive, full cte alias

>     ,"SELECT m.name AS mname, pname\n\
>      \FROM manufacturers m, LATERAL get_product_names(m.id) pname;"

>     ,"SELECT m.name AS mname, pname\n\
>      \FROM manufacturers m LEFT JOIN LATERAL get_product_names(m.id) pname ON true;"

>     ,"SELECT 2+2;"

>     ,"SELECT distributors.* WHERE distributors.name = 'Westward';"

>     ]


> {-f = mapM_ (putStrLn . either peFormattedError show . parseQueryExpr "" Nothing)
>       ["SELECT * FROM t1 CROSS JOIN t2;"
>       ,"SELECT * FROM t1 INNER JOIN t2 ON t1.num = t2.num;"
>       ,"SELECT * FROM t1 INNER JOIN t2 USING (num);"
>       ,"SELECT * FROM t1 NATURAL INNER JOIN t2;"
>       ,"SELECT * FROM t1 LEFT JOIN t2 ON t1.num = t2.num;"
>       ,"SELECT * FROM t1 LEFT JOIN t2 USING (num);"
>       ,"SELECT * FROM t1 RIGHT JOIN t2 ON t1.num = t2.num;"
>       ,"SELECT * FROM t1 FULL JOIN t2 ON t1.num = t2.num;"
>       ,"SELECT * FROM t1 LEFT JOIN t2 ON t1.num = t2.num AND t2.value = 'xxx';"
>   -    ,"SELECT * FROM t1 LEFT JOIN t2 ON t1.num = t2.num WHERE t2.value = 'xxx';"]-}

