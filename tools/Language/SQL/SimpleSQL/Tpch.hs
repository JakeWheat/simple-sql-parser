
{-
Some tests for parsing the tpch queries

The changes made to the official syntax are:
1. replace the set rowcount with ansi standard fetch first n rows only
2. replace the create view, query, drop view sequence with a query
   using a common table expression
-}

{-# LANGUAGE OverloadedStrings #-}
module Language.SQL.SimpleSQL.Tpch (tpchTests,tpchQueries) where

import Language.SQL.SimpleSQL.TestTypes

import Data.Text (Text)

tpchTests :: TestItem
tpchTests =
    Group "parse tpch"
    $ map (ParseQueryExpr ansi2011 . snd) tpchQueries

tpchQueries :: [(String,Text)]
tpchQueries =
  [("Q1","\n\
         \select\n\
         \        l_returnflag,\n\
         \        l_linestatus,\n\
         \        sum(l_quantity) as sum_qty,\n\
         \        sum(l_extendedprice) as sum_base_price,\n\
         \        sum(l_extendedprice * (1 - l_discount)) as sum_disc_price,\n\
         \        sum(l_extendedprice * (1 - l_discount) * (1 + l_tax)) as sum_charge,\n\
         \        avg(l_quantity) as avg_qty,\n\
         \        avg(l_extendedprice) as avg_price,\n\
         \        avg(l_discount) as avg_disc,\n\
         \        count(*) as count_order\n\
         \from\n\
         \        lineitem\n\
         \where\n\
         \        l_shipdate <= date '1998-12-01' - interval '63' day (3)\n\
         \group by\n\
         \        l_returnflag,\n\
         \        l_linestatus\n\
         \order by\n\
         \        l_returnflag,\n\
         \        l_linestatus")
  ,("Q2","\n\
         \select\n\
         \        s_acctbal,\n\
         \        s_name,\n\
         \        n_name,\n\
         \        p_partkey,\n\
         \        p_mfgr,\n\
         \        s_address,\n\
         \        s_phone,\n\
         \        s_comment\n\
         \from\n\
         \        part,\n\
         \        supplier,\n\
         \        partsupp,\n\
         \        nation,\n\
         \        region\n\
         \where\n\
         \        p_partkey = ps_partkey\n\
         \        and s_suppkey = ps_suppkey\n\
         \        and p_size = 15\n\
         \        and p_type like '%BRASS'\n\
         \        and s_nationkey = n_nationkey\n\
         \        and n_regionkey = r_regionkey\n\
         \        and r_name = 'EUROPE'\n\
         \        and ps_supplycost = (\n\
         \                select\n\
         \                        min(ps_supplycost)\n\
         \                from\n\
         \                        partsupp,\n\
         \                        supplier,\n\
         \                        nation,\n\
         \                        region\n\
         \                where\n\
         \                        p_partkey = ps_partkey\n\
         \                        and s_suppkey = ps_suppkey\n\
         \                        and s_nationkey = n_nationkey\n\
         \                        and n_regionkey = r_regionkey\n\
         \                        and r_name = 'EUROPE'\n\
         \        )\n\
         \order by\n\
         \        s_acctbal desc,\n\
         \        n_name,\n\
         \        s_name,\n\
         \        p_partkey\n\
         \fetch first 100 rows only")
  ,("Q3","\n\
         \ select\n\
         \         l_orderkey,\n\
         \         sum(l_extendedprice * (1 - l_discount)) as revenue,\n\
         \         o_orderdate,\n\
         \         o_shippriority\n\
         \ from\n\
         \         customer,\n\
         \         orders,\n\
         \         lineitem\n\
         \ where\n\
         \         c_mktsegment = 'MACHINERY'\n\
         \         and c_custkey = o_custkey\n\
         \         and l_orderkey = o_orderkey\n\
         \         and o_orderdate < date '1995-03-21'\n\
         \         and l_shipdate > date '1995-03-21'\n\
         \ group by\n\
         \         l_orderkey,\n\
         \         o_orderdate,\n\
         \         o_shippriority\n\
         \ order by\n\
         \         revenue desc,\n\
         \         o_orderdate\n\
         \ fetch first 10 rows only")
  ,("Q4","\n\
         \ select\n\
         \         o_orderpriority,\n\
         \         count(*) as order_count\n\
         \ from\n\
         \         orders\n\
         \ where\n\
         \         o_orderdate >= date '1996-03-01'\n\
         \         and o_orderdate < date '1996-03-01' + interval '3' month\n\
         \         and exists (\n\
         \                 select\n\
         \                         *\n\
         \                 from\n\
         \                         lineitem\n\
         \                 where\n\
         \                         l_orderkey = o_orderkey\n\
         \                         and l_commitdate < l_receiptdate\n\
         \         )\n\
         \ group by\n\
         \         o_orderpriority\n\
         \ order by\n\
         \         o_orderpriority")
  ,("Q5","\n\
         \ select\n\
         \         n_name,\n\
         \         sum(l_extendedprice * (1 - l_discount)) as revenue\n\
         \ from\n\
         \         customer,\n\
         \         orders,\n\
         \         lineitem,\n\
         \         supplier,\n\
         \         nation,\n\
         \         region\n\
         \ where\n\
         \         c_custkey = o_custkey\n\
         \         and l_orderkey = o_orderkey\n\
         \         and l_suppkey = s_suppkey\n\
         \         and c_nationkey = s_nationkey\n\
         \         and s_nationkey = n_nationkey\n\
         \         and n_regionkey = r_regionkey\n\
         \         and r_name = 'EUROPE'\n\
         \         and o_orderdate >= date '1997-01-01'\n\
         \         and o_orderdate < date '1997-01-01' + interval '1' year\n\
         \ group by\n\
         \         n_name\n\
         \ order by\n\
         \         revenue desc")
  ,("Q6","\n\
         \ select\n\
         \         sum(l_extendedprice * l_discount) as revenue\n\
         \ from\n\
         \         lineitem\n\
         \ where\n\
         \         l_shipdate >= date '1997-01-01'\n\
         \         and l_shipdate < date '1997-01-01' + interval '1' year\n\
         \         and l_discount between 0.07 - 0.01 and 0.07 + 0.01\n\
         \         and l_quantity < 24")
  ,("Q7","\n\
         \ select\n\
         \         supp_nation,\n\
         \         cust_nation,\n\
         \         l_year,\n\
         \         sum(volume) as revenue\n\
         \ from\n\
         \         (\n\
         \                 select\n\
         \                         n1.n_name as supp_nation,\n\
         \                         n2.n_name as cust_nation,\n\
         \                         extract(year from l_shipdate) as l_year,\n\
         \                         l_extendedprice * (1 - l_discount) as volume\n\
         \                 from\n\
         \                         supplier,\n\
         \                         lineitem,\n\
         \                         orders,\n\
         \                         customer,\n\
         \                         nation n1,\n\
         \                         nation n2\n\
         \                 where\n\
         \                         s_suppkey = l_suppkey\n\
         \                         and o_orderkey = l_orderkey\n\
         \                         and c_custkey = o_custkey\n\
         \                         and s_nationkey = n1.n_nationkey\n\
         \                         and c_nationkey = n2.n_nationkey\n\
         \                         and (\n\
         \                                 (n1.n_name = 'PERU' and n2.n_name = 'IRAQ')\n\
         \                                 or (n1.n_name = 'IRAQ' and n2.n_name = 'PERU')\n\
         \                         )\n\
         \                         and l_shipdate between date '1995-01-01' and date '1996-12-31'\n\
         \         ) as shipping\n\
         \ group by\n\
         \         supp_nation,\n\
         \         cust_nation,\n\
         \         l_year\n\
         \ order by\n\
         \         supp_nation,\n\
         \         cust_nation,\n\
         \         l_year")
  ,("Q8","\n\
         \ select\n\
         \         o_year,\n\
         \         sum(case\n\
         \                 when nation = 'IRAQ' then volume\n\
         \                 else 0\n\
         \         end) / sum(volume) as mkt_share\n\
         \ from\n\
         \         (\n\
         \                 select\n\
         \                         extract(year from o_orderdate) as o_year,\n\
         \                         l_extendedprice * (1 - l_discount) as volume,\n\
         \                         n2.n_name as nation\n\
         \                 from\n\
         \                         part,\n\
         \                         supplier,\n\
         \                         lineitem,\n\
         \                         orders,\n\
         \                         customer,\n\
         \                         nation n1,\n\
         \                         nation n2,\n\
         \                         region\n\
         \                 where\n\
         \                         p_partkey = l_partkey\n\
         \                         and s_suppkey = l_suppkey\n\
         \                         and l_orderkey = o_orderkey\n\
         \                         and o_custkey = c_custkey\n\
         \                         and c_nationkey = n1.n_nationkey\n\
         \                         and n1.n_regionkey = r_regionkey\n\
         \                         and r_name = 'MIDDLE EAST'\n\
         \                         and s_nationkey = n2.n_nationkey\n\
         \                         and o_orderdate between date '1995-01-01' and date '1996-12-31'\n\
         \                         and p_type = 'STANDARD ANODIZED BRASS'\n\
         \         ) as all_nations\n\
         \ group by\n\
         \         o_year\n\
         \ order by\n\
         \         o_year")
   ,("Q9","\n\
         \ select\n\
         \         nation,\n\
         \         o_year,\n\
         \         sum(amount) as sum_profit\n\
         \ from\n\
         \         (\n\
         \                 select\n\
         \                         n_name as nation,\n\
         \                         extract(year from o_orderdate) as o_year,\n\
         \                         l_extendedprice * (1 - l_discount) - ps_supplycost * l_quantity as amount\n\
         \                 from\n\
         \                         part,\n\
         \                         supplier,\n\
         \                         lineitem,\n\
         \                         partsupp,\n\
         \                         orders,\n\
         \                         nation\n\
         \                 where\n\
         \                         s_suppkey = l_suppkey\n\
         \                         and ps_suppkey = l_suppkey\n\
         \                         and ps_partkey = l_partkey\n\
         \                         and p_partkey = l_partkey\n\
         \                         and o_orderkey = l_orderkey\n\
         \                         and s_nationkey = n_nationkey\n\
         \                         and p_name like '%antique%'\n\
         \         ) as profit\n\
         \ group by\n\
         \         nation,\n\
         \         o_year\n\
         \ order by\n\
         \         nation,\n\
         \         o_year desc")
   ,("Q10","\n\
         \ select\n\
         \         c_custkey,\n\
         \         c_name,\n\
         \         sum(l_extendedprice * (1 - l_discount)) as revenue,\n\
         \         c_acctbal,\n\
         \         n_name,\n\
         \         c_address,\n\
         \         c_phone,\n\
         \         c_comment\n\
         \ from\n\
         \         customer,\n\
         \         orders,\n\
         \         lineitem,\n\
         \         nation\n\
         \ where\n\
         \         c_custkey = o_custkey\n\
         \         and l_orderkey = o_orderkey\n\
         \         and o_orderdate >= date '1993-12-01'\n\
         \         and o_orderdate < date '1993-12-01' + interval '3' month\n\
         \         and l_returnflag = 'R'\n\
         \         and c_nationkey = n_nationkey\n\
         \ group by\n\
         \         c_custkey,\n\
         \         c_name,\n\
         \         c_acctbal,\n\
         \         c_phone,\n\
         \         n_name,\n\
         \         c_address,\n\
         \         c_comment\n\
         \ order by\n\
         \         revenue desc\n\
         \ fetch first 20 rows only")
   ,("Q11","\n\
         \ select\n\
         \         ps_partkey,\n\
         \         sum(ps_supplycost * ps_availqty) as value\n\
         \ from\n\
         \         partsupp,\n\
         \         supplier,\n\
         \         nation\n\
         \ where\n\
         \         ps_suppkey = s_suppkey\n\
         \         and s_nationkey = n_nationkey\n\
         \         and n_name = 'CHINA'\n\
         \ group by\n\
         \         ps_partkey having\n\
         \                 sum(ps_supplycost * ps_availqty) > (\n\
         \                         select\n\
         \                                 sum(ps_supplycost * ps_availqty) * 0.0001000000\n\
         \                         from\n\
         \                                 partsupp,\n\
         \                                 supplier,\n\
         \                                 nation\n\
         \                         where\n\
         \                                 ps_suppkey = s_suppkey\n\
         \                                 and s_nationkey = n_nationkey\n\
         \                                 and n_name = 'CHINA'\n\
         \                 )\n\
         \ order by\n\
         \         value desc")
   ,("Q12","\n\
         \ select\n\
         \         l_shipmode,\n\
         \         sum(case\n\
         \                 when o_orderpriority = '1-URGENT'\n\
         \                         or o_orderpriority = '2-HIGH'\n\
         \                         then 1\n\
         \                 else 0\n\
         \         end) as high_line_count,\n\
         \         sum(case\n\
         \                 when o_orderpriority <> '1-URGENT'\n\
         \                         and o_orderpriority <> '2-HIGH'\n\
         \                         then 1\n\
         \                 else 0\n\
         \         end) as low_line_count\n\
         \ from\n\
         \         orders,\n\
         \         lineitem\n\
         \ where\n\
         \         o_orderkey = l_orderkey\n\
         \         and l_shipmode in ('AIR', 'RAIL')\n\
         \         and l_commitdate < l_receiptdate\n\
         \         and l_shipdate < l_commitdate\n\
         \         and l_receiptdate >= date '1994-01-01'\n\
         \         and l_receiptdate < date '1994-01-01' + interval '1' year\n\
         \ group by\n\
         \         l_shipmode\n\
         \ order by\n\
         \         l_shipmode")
   ,("Q13","\n\
         \ select\n\
         \         c_count,\n\
         \         count(*) as custdist\n\
         \ from\n\
         \         (\n\
         \                 select\n\
         \                         c_custkey,\n\
         \                         count(o_orderkey)\n\
         \                 from\n\
         \                         customer left outer join orders on\n\
         \                                 c_custkey = o_custkey\n\
         \                                 and o_comment not like '%pending%requests%'\n\
         \                 group by\n\
         \                         c_custkey\n\
         \         ) as c_orders (c_custkey, c_count)\n\
         \ group by\n\
         \         c_count\n\
         \ order by\n\
         \         custdist desc,\n\
         \         c_count desc")
   ,("Q14","\n\
         \ select\n\
         \         100.00 * sum(case\n\
         \                 when p_type like 'PROMO%'\n\
         \                         then l_extendedprice * (1 - l_discount)\n\
         \                 else 0\n\
         \         end) / sum(l_extendedprice * (1 - l_discount)) as promo_revenue\n\
         \ from\n\
         \         lineitem,\n\
         \         part\n\
         \ where\n\
         \         l_partkey = p_partkey\n\
         \         and l_shipdate >= date '1994-12-01'\n\
         \         and l_shipdate < date '1994-12-01' + interval '1' month")
   ,("Q15","\n\
         \ /*create view revenue0 (supplier_no, total_revenue) as\n\
         \         select\n\
         \                 l_suppkey,\n\
         \                 sum(l_extendedprice * (1 - l_discount))\n\
         \         from\n\
         \                 lineitem\n\
         \         where\n\
         \                 l_shipdate >= date '1995-06-01'\n\
         \                 and l_shipdate < date '1995-06-01' + interval '3' month\n\
         \         group by\n\
         \                 l_suppkey;*/\n\
         \ with\n\
         \ revenue0 as\n\
         \         (select\n\
         \                 l_suppkey as supplier_no,\n\
         \                 sum(l_extendedprice * (1 - l_discount)) as total_revenue\n\
         \         from\n\
         \                 lineitem\n\
         \         where\n\
         \                 l_shipdate >= date '1995-06-01'\n\
         \                 and l_shipdate < date '1995-06-01' + interval '3' month\n\
         \         group by\n\
         \                 l_suppkey)\n\
         \ select\n\
         \         s_suppkey,\n\
         \         s_name,\n\
         \         s_address,\n\
         \         s_phone,\n\
         \         total_revenue\n\
         \ from\n\
         \         supplier,\n\
         \         revenue0\n\
         \ where\n\
         \         s_suppkey = supplier_no\n\
         \         and total_revenue = (\n\
         \                 select\n\
         \                         max(total_revenue)\n\
         \                 from\n\
         \                         revenue0\n\
         \         )\n\
         \ order by\n\
         \         s_suppkey")
   ,("Q16","\n\
         \ select\n\
         \         p_brand,\n\
         \         p_type,\n\
         \         p_size,\n\
         \         count(distinct ps_suppkey) as supplier_cnt\n\
         \ from\n\
         \         partsupp,\n\
         \         part\n\
         \ where\n\
         \         p_partkey = ps_partkey\n\
         \         and p_brand <> 'Brand#15'\n\
         \         and p_type not like 'MEDIUM BURNISHED%'\n\
         \         and p_size in (39, 26, 18, 45, 19, 1, 3, 9)\n\
         \         and ps_suppkey not in (\n\
         \                 select\n\
         \                         s_suppkey\n\
         \                 from\n\
         \                         supplier\n\
         \                 where\n\
         \                         s_comment like '%Customer%Complaints%'\n\
         \         )\n\
         \ group by\n\
         \         p_brand,\n\
         \         p_type,\n\
         \         p_size\n\
         \ order by\n\
         \         supplier_cnt desc,\n\
         \         p_brand,\n\
         \         p_type,\n\
         \         p_size")
   ,("Q17","\n\
         \ select\n\
         \         sum(l_extendedprice) / 7.0 as avg_yearly\n\
         \ from\n\
         \         lineitem,\n\
         \         part\n\
         \ where\n\
         \         p_partkey = l_partkey\n\
         \         and p_brand = 'Brand#52'\n\
         \         and p_container = 'JUMBO CAN'\n\
         \         and l_quantity < (\n\
         \                 select\n\
         \                         0.2 * avg(l_quantity)\n\
         \                 from\n\
         \                         lineitem\n\
         \                 where\n\
         \                         l_partkey = p_partkey\n\
         \         )")
   ,("Q18","\n\
         \ select\n\
         \         c_name,\n\
         \         c_custkey,\n\
         \         o_orderkey,\n\
         \         o_orderdate,\n\
         \         o_totalprice,\n\
         \         sum(l_quantity)\n\
         \ from\n\
         \         customer,\n\
         \         orders,\n\
         \         lineitem\n\
         \ where\n\
         \         o_orderkey in (\n\
         \                 select\n\
         \                         l_orderkey\n\
         \                 from\n\
         \                         lineitem\n\
         \                 group by\n\
         \                         l_orderkey having\n\
         \                                 sum(l_quantity) > 313\n\
         \         )\n\
         \         and c_custkey = o_custkey\n\
         \         and o_orderkey = l_orderkey\n\
         \ group by\n\
         \         c_name,\n\
         \         c_custkey,\n\
         \         o_orderkey,\n\
         \         o_orderdate,\n\
         \         o_totalprice\n\
         \ order by\n\
         \         o_totalprice desc,\n\
         \         o_orderdate\n\
         \ fetch first 100 rows only")
   ,("Q19","\n\
         \ select\n\
         \         sum(l_extendedprice* (1 - l_discount)) as revenue\n\
         \ from\n\
         \         lineitem,\n\
         \         part\n\
         \ where\n\
         \         (\n\
         \                 p_partkey = l_partkey\n\
         \                 and p_brand = 'Brand#43'\n\
         \                 and p_container in ('SM CASE', 'SM BOX', 'SM PACK', 'SM PKG')\n\
         \                 and l_quantity >= 3 and l_quantity <= 3 + 10\n\
         \                 and p_size between 1 and 5\n\
         \                 and l_shipmode in ('AIR', 'AIR REG')\n\
         \                 and l_shipinstruct = 'DELIVER IN PERSON'\n\
         \         )\n\
         \         or\n\
         \         (\n\
         \                 p_partkey = l_partkey\n\
         \                 and p_brand = 'Brand#25'\n\
         \                 and p_container in ('MED BAG', 'MED BOX', 'MED PKG', 'MED PACK')\n\
         \                 and l_quantity >= 10 and l_quantity <= 10 + 10\n\
         \                 and p_size between 1 and 10\n\
         \                 and l_shipmode in ('AIR', 'AIR REG')\n\
         \                 and l_shipinstruct = 'DELIVER IN PERSON'\n\
         \         )\n\
         \         or\n\
         \         (\n\
         \                 p_partkey = l_partkey\n\
         \                 and p_brand = 'Brand#24'\n\
         \                 and p_container in ('LG CASE', 'LG BOX', 'LG PACK', 'LG PKG')\n\
         \                 and l_quantity >= 22 and l_quantity <= 22 + 10\n\
         \                 and p_size between 1 and 15\n\
         \                 and l_shipmode in ('AIR', 'AIR REG')\n\
         \                 and l_shipinstruct = 'DELIVER IN PERSON'\n\
         \         )")
   ,("Q20","\n\
         \ select\n\
         \         s_name,\n\
         \         s_address\n\
         \ from\n\
         \         supplier,\n\
         \         nation\n\
         \ where\n\
         \         s_suppkey in (\n\
         \                 select\n\
         \                         ps_suppkey\n\
         \                 from\n\
         \                         partsupp\n\
         \                 where\n\
         \                         ps_partkey in (\n\
         \                                 select\n\
         \                                         p_partkey\n\
         \                                 from\n\
         \                                         part\n\
         \                                 where\n\
         \                                         p_name like 'lime%'\n\
         \                         )\n\
         \                         and ps_availqty > (\n\
         \                                 select\n\
         \                                         0.5 * sum(l_quantity)\n\
         \                                 from\n\
         \                                         lineitem\n\
         \                                 where\n\
         \                                         l_partkey = ps_partkey\n\
         \                                         and l_suppkey = ps_suppkey\n\
         \                                         and l_shipdate >= date '1994-01-01'\n\
         \                                         and l_shipdate < date '1994-01-01' + interval '1' year\n\
         \                         )\n\
         \         )\n\
         \         and s_nationkey = n_nationkey\n\
         \         and n_name = 'VIETNAM'\n\
         \ order by\n\
         \         s_name")
   ,("Q21","\n\
         \ select\n\
         \         s_name,\n\
         \         count(*) as numwait\n\
         \ from\n\
         \         supplier,\n\
         \         lineitem l1,\n\
         \         orders,\n\
         \         nation\n\
         \ where\n\
         \         s_suppkey = l1.l_suppkey\n\
         \         and o_orderkey = l1.l_orderkey\n\
         \         and o_orderstatus = 'F'\n\
         \         and l1.l_receiptdate > l1.l_commitdate\n\
         \         and exists (\n\
         \                 select\n\
         \                         *\n\
         \                 from\n\
         \                         lineitem l2\n\
         \                 where\n\
         \                         l2.l_orderkey = l1.l_orderkey\n\
         \                         and l2.l_suppkey <> l1.l_suppkey\n\
         \         )\n\
         \         and not exists (\n\
         \                 select\n\
         \                         *\n\
         \                 from\n\
         \                         lineitem l3\n\
         \                 where\n\
         \                         l3.l_orderkey = l1.l_orderkey\n\
         \                         and l3.l_suppkey <> l1.l_suppkey\n\
         \                         and l3.l_receiptdate > l3.l_commitdate\n\
         \         )\n\
         \         and s_nationkey = n_nationkey\n\
         \         and n_name = 'INDIA'\n\
         \ group by\n\
         \         s_name\n\
         \ order by\n\
         \         numwait desc,\n\
         \         s_name\n\
         \ fetch first 100 rows only")
   ,("Q22","\n\
         \ select\n\
         \         cntrycode,\n\
         \         count(*) as numcust,\n\
         \         sum(c_acctbal) as totacctbal\n\
         \ from\n\
         \         (\n\
         \                 select\n\
         \                         substring(c_phone from 1 for 2) as cntrycode,\n\
         \                         c_acctbal\n\
         \                 from\n\
         \                         customer\n\
         \                 where\n\
         \                         substring(c_phone from 1 for 2) in\n\
         \                                 ('41', '28', '39', '21', '24', '29', '44')\n\
         \                         and c_acctbal > (\n\
         \                                 select\n\
         \                                         avg(c_acctbal)\n\
         \                                 from\n\
         \                                         customer\n\
         \                                 where\n\
         \                                         c_acctbal > 0.00\n\
         \                                         and substring(c_phone from 1 for 2) in\n\
         \                                                 ('41', '28', '39', '21', '24', '29', '44')\n\
         \                         )\n\
         \                         and not exists (\n\
         \                                 select\n\
         \                                         *\n\
         \                                 from\n\
         \                                         orders\n\
         \                                 where\n\
         \                                         o_custkey = c_custkey\n\
         \                         )\n\
         \         ) as custsale\n\
         \ group by\n\
         \         cntrycode\n\
         \ order by\n\
         \         cntrycode")
  ]
