
-- Little hack to add links to the navigation bars

main :: IO ()
main = interact addLinks


addLinks :: String -> String
addLinks [] = error "not found"
addLinks ('<':'/':'u':'l':'>':'\n':'<':'/':'d':'i':'v':'>':xs) =
    "</ul>" ++ linkSection ++ "\n</div>" ++ xs
addLinks (x:xs) = x : addLinks xs

linkSection :: String
linkSection =
  "<hr />\n\
  \<ul class=\"sectlevel1\">\n\
  \<div id=\"toctitle\">Links</div>\n\
  \<li><a href=\"index.html\">Index</a></li>\n\
  \<li><a href='haddock/index.html'>Haddock</li>\n\
  \<li><a href=\"supported_sql.html\" class=\"bare\">Supported SQL</a></li>\n\
  \<li><a href=\"test_cases.html\">Test cases</a></li>\n\
  \</ul>\n\
  \<br />\n\
  \<ul class=\"sectlevel1\">\n\
  \<li><a href=\"http://jakewheat.github.io/simple-sql-parser/latest\" class=\"bare\">Homepage</a></li>\n\
  \<li><a href=\"http://hackage.haskell.org/package/simple-sql-parser\" class=\"bare\">Hackage</a></li>\n\
  \<li><a href=\"https://github.com/JakeWheat/simple-sql-parser\" class=\"bare\">Repository</a></li>\n\
  \<li><a href=\"https://github.com/JakeWheat/simple-sql-parser/issues\" class=\"bare\">Bug tracker</a></li>\n\
  \<li><a href=\"https://github.com/JakeWheat/simple-sql-parser/blob/master/changelog\" class=\"bare\">Changes</a></li>\n\
  \<li><a href=\"http://jakewheat.github.io/simple-sql-parser/\" class=\"bare\">Other versions</a></li>\n\
  \</li><li>jakewheat@tutanota.com</li>\n\
  \</ul>\n"
