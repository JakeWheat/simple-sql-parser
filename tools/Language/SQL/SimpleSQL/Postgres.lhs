
Here are some tests taken from the SQL in the postgres manual. Almost
all of the postgres specific syntax has been skipped, this can be
revisited when the dialect support is added.

> module Language.SQL.SimpleSQL.Postgres (postgresTests) where

> import Language.SQL.SimpleSQL.TestTypes
> import Language.SQL.SimpleSQL.Syntax

> postgresTests :: TestItem
> postgresTests = Group "postgresTests"
>     [
>     ]

lexical syntax

SELECT 'foo'
'bar'; -> if there is a newline, this parses to select 'foobar'

SELECT name, (SELECT max(pop) FROM cities WHERE cities.state = states.name)
    FROM states;

SELECT ROW(1,2.5,'this is a test');

SELECT ROW(t.*, 42) FROM t; -- needs the .* parsing to be enabled in more contexts
SELECT ROW(t.f1, t.f2, 42) FROM t;
SELECT getf1(CAST(ROW(11,'this is a test',2.5) AS myrowtype));

SELECT ROW(1,2.5,'this is a test') = ROW(1, 3, 'not the same');

SELECT ROW(table.*) IS NULL FROM table;

SELECT true OR somefunc();

SELECT somefunc() OR true;

queries
SELECT * FROM t1 CROSS JOIN t2;
SELECT * FROM t1 INNER JOIN t2 ON t1.num = t2.num;
SELECT * FROM t1 INNER JOIN t2 USING (num);
SELECT * FROM t1 NATURAL INNER JOIN t2;
SELECT * FROM t1 LEFT JOIN t2 ON t1.num = t2.num;
SELECT * FROM t1 LEFT JOIN t2 USING (num);
SELECT * FROM t1 RIGHT JOIN t2 ON t1.num = t2.num;
SELECT * FROM t1 FULL JOIN t2 ON t1.num = t2.num;
SELECT * FROM t1 LEFT JOIN t2 ON t1.num = t2.num AND t2.value = 'xxx';
SELECT * FROM t1 LEFT JOIN t2 ON t1.num = t2.num WHERE t2.value = 'xxx';

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


SELECT * FROM some_very_long_table_name s JOIN another_fairly_long_name a ON s.id = a.num;
SELECT * FROM people AS mother JOIN people AS child ON mother.id = child.mother_id;
SELECT * FROM my_table AS a CROSS JOIN my_table AS b;
SELECT * FROM (my_table AS a CROSS JOIN my_table) AS b;
SELECT * FROM getfoo(1) AS t1;
SELECT * FROM foo
    WHERE foosubid IN (
                        SELECT foosubid
                        FROM getfoo(foo.fooid) z
                        WHERE z.fooid = foo.fooid
                      );
SELECT *
    FROM dblink('dbname=mydb', 'SELECT proname, prosrc FROM pg_proc')
      AS t1(proname name, prosrc text)
    WHERE proname LIKE 'bytea%';

SELECT * FROM foo, LATERAL (SELECT * FROM bar WHERE bar.id = foo.bar_id) ss;
SELECT * FROM foo, bar WHERE bar.id = foo.bar_id;

SELECT p1.id, p2.id, v1, v2
FROM polygons p1, polygons p2,
     LATERAL vertices(p1.poly) v1,
     LATERAL vertices(p2.poly) v2
WHERE (v1 <-> v2) < 10 AND p1.id != p2.id;

SELECT p1.id, p2.id, v1, v2
FROM polygons p1 CROSS JOIN LATERAL vertices(p1.poly) v1,
     polygons p2 CROSS JOIN LATERAL vertices(p2.poly) v2
WHERE (v1 <-> v2) < 10 AND p1.id != p2.id;

SELECT m.name
FROM manufacturers m LEFT JOIN LATERAL get_product_names(m.id) pname ON true
WHERE pname IS NULL;


SELECT * FROM fdt WHERE c1 > 5

SELECT * FROM fdt WHERE c1 IN (1, 2, 3)

SELECT * FROM fdt WHERE c1 IN (SELECT c1 FROM t2)

SELECT * FROM fdt WHERE c1 IN (SELECT c3 FROM t2 WHERE c2 = fdt.c1 + 10)

SELECT * FROM fdt WHERE c1 BETWEEN (SELECT c3 FROM t2 WHERE c2 = fdt.c1 + 10) AND 100

SELECT * FROM fdt WHERE EXISTS (SELECT c1 FROM t2 WHERE c2 > fdt.c1)

SELECT * FROM test1;

SELECT x FROM test1 GROUP BY x;
SELECT x, sum(y) FROM test1 GROUP BY x;
SELECT product_id, p.name, (sum(s.units) * p.price) AS sales
    FROM products p LEFT JOIN sales s USING (product_id)
    GROUP BY product_id, p.name, p.price;

SELECT x, sum(y) FROM test1 GROUP BY x HAVING sum(y) > 3;
SELECT x, sum(y) FROM test1 GROUP BY x HAVING x < 'c';
SELECT product_id, p.name, (sum(s.units) * (p.price - p.cost)) AS profit
    FROM products p LEFT JOIN sales s USING (product_id)
    WHERE s.date > CURRENT_DATE - INTERVAL '4 weeks'
    GROUP BY product_id, p.name, p.price, p.cost
    HAVING sum(p.price * s.units) > 5000;

SELECT a, b, c FROM t

SELECT tbl1.a, tbl2.a, tbl1.b FROM t

SELECT tbl1.*, tbl2.a FROM t

SELECT a AS value, b + c AS sum FROM t

-- bad keyword
--SELECT a value, b + c AS sum FROM ...

SELECT a "value", b + c AS sum FROM t

SELECT DISTINCT select_list t

VALUES (1, 'one'), (2, 'two'), (3, 'three');

SELECT 1 AS column1, 'one' AS column2
UNION ALL
SELECT 2, 'two'
UNION ALL
SELECT 3, 'three';

SELECT * FROM (VALUES (1, 'one'), (2, 'two'), (3, 'three')) AS t (num,letter);

WITH regional_sales AS (
        SELECT region, SUM(amount) AS total_sales
        FROM orders
        GROUP BY region
     ), top_regions AS (
        SELECT region
        FROM regional_sales
        WHERE total_sales > (SELECT SUM(total_sales)/10 FROM regional_sales)
     )
SELECT region,
       product,
       SUM(quantity) AS product_units,
       SUM(amount) AS product_sales
FROM orders
WHERE region IN (SELECT region FROM top_regions)
GROUP BY region, product;

WITH RECURSIVE t(n) AS (
    VALUES (1)
  UNION ALL
    SELECT n+1 FROM t WHERE n < 100
)
SELECT sum(n) FROM t

WITH RECURSIVE included_parts(sub_part, part, quantity) AS (
    SELECT sub_part, part, quantity FROM parts WHERE part = 'our_product'
  UNION ALL
    SELECT p.sub_part, p.part, p.quantity
    FROM included_parts pr, parts p
    WHERE p.part = pr.sub_part
  )
SELECT sub_part, SUM(quantity) as total_quantity
FROM included_parts
GROUP BY sub_part

WITH RECURSIVE search_graph(id, link, data, depth) AS (
        SELECT g.id, g.link, g.data, 1
        FROM graph g
      UNION ALL
        SELECT g.id, g.link, g.data, sg.depth + 1
        FROM graph g, search_graph sg
        WHERE g.id = sg.link
)
SELECT * FROM search_graph;

WITH RECURSIVE search_graph(id, link, data, depth, path, cycle) AS (
        SELECT g.id, g.link, g.data, 1,
          ARRAY[g.id],
          false
        FROM graph g
      UNION ALL
        SELECT g.id, g.link, g.data, sg.depth + 1,
          path || g.id,
          g.id = ANY(path)
        FROM graph g, search_graph sg
        WHERE g.id = sg.link AND NOT cycle
)
SELECT * FROM search_graph;

WITH RECURSIVE search_graph(id, link, data, depth, path, cycle) AS (
        SELECT g.id, g.link, g.data, 1,
          ARRAY[ROW(g.f1, g.f2)],
          false
        FROM graph g
      UNION ALL
        SELECT g.id, g.link, g.data, sg.depth + 1,
          path || ROW(g.f1, g.f2),
          ROW(g.f1, g.f2) = ANY(path)
        FROM graph g, search_graph sg
        WHERE g.id = sg.link AND NOT cycle
)
SELECT * FROM search_graph;

WITH RECURSIVE t(n) AS (
    SELECT 1
  UNION ALL
    SELECT n+1 FROM t
)
SELECT n FROM t LIMIT 100;

select page reference

SELECT f.title, f.did, d.name, f.date_prod, f.kind
    FROM distributors d, films f
    WHERE f.did = d.did

SELECT kind, sum(len) AS total
    FROM films
    GROUP BY kind
    HAVING sum(len) < interval '5 hours';

SELECT * FROM distributors ORDER BY name;
SELECT * FROM distributors ORDER BY 2;

SELECT distributors.name
    FROM distributors
    WHERE distributors.name LIKE 'W%'
UNION
SELECT actors.name
    FROM actors
    WHERE actors.name LIKE 'W%';

WITH t AS (
    SELECT random() as x FROM generate_series(1, 3)
  )
SELECT * FROM t
UNION ALL
SELECT * FROM t

WITH RECURSIVE employee_recursive(distance, employee_name, manager_name) AS (
    SELECT 1, employee_name, manager_name
    FROM employee
    WHERE manager_name = 'Mary'
  UNION ALL
    SELECT er.distance + 1, e.employee_name, e.manager_name
    FROM employee_recursive er, employee e
    WHERE er.employee_name = e.manager_name
  )
SELECT distance, employee_name FROM employee_recursive;

SELECT m.name AS mname, pname
FROM manufacturers m, LATERAL get_product_names(m.id) pname;

SELECT m.name AS mname, pname
FROM manufacturers m LEFT JOIN LATERAL get_product_names(m.id) pname ON true;

SELECT 2+2;

SELECT distributors.* WHERE distributors.name = 'Westward';

