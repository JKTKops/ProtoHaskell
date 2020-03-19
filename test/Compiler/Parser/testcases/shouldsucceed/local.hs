-- example code from ParserTest.hs, adapted into various forms for testing

test1 :: IO TestTree
test1 = goldenPShow "Parser Golden Tests"
                    "test/Compiler/Parser/testcases"
                    runParse
  where runParse fn = parse fn noFlags

test2 :: IO TestTree
test2 = let name = "Parser Golden Tests"
            dir  = "test/Compiler/Parser/testcases"
            runParse fn = parse fn noFlags
        in goldenPShow name dir runParse

-- nested block contexts
test3 :: IO TestTree
test3 = let p = let name = "Parser Golden Tests"
                    dir  = "test/Compiler/Parser/testcases"
                in (name, dir)
            runParse fn = parse fn noFlags
        in uncurry goldenPShow p runParse
