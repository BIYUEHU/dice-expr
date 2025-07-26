module TestCombinator

import Combinator

-- 测试辅助函数
testParser : Show a => String -> Parser a -> String -> IO ()
testParser name parser input = do
  putStrLn $ "Testing " ++ name ++ " with input: \"" ++ input ++ "\""
  case parser (unpack input) of
    Left err => putStrLn $ "  ERROR: " ++ err
    Right (result, remaining) => do
      putStrLn $ "  SUCCESS: " ++ show result
      putStrLn $ "  REMAINING: \"" ++ pack remaining ++ "\""
  putStrLn ""

-- 基础解析器测试
testBasicParsers : IO ()
testBasicParsers = do
  putStrLn "=== Basic Parser Tests ==="
  
  -- 测试 satisfy
  let digitParser = satisfy isDigit
  testParser "digit" digitParser "123"
  testParser "digit" digitParser "abc"
  
  -- 测试 spaces
  -- testParser "spaces" spaces "   hello"
  testParser "spaces" spaces "hello"
  
  -- 测试 symbol
  testParser "symbol '+'" (symbol (unpack "+")) "+ 123"
  testParser "symbol '+'" (symbol (unpack "+")) "- 123"
  
  -- 测试 integer
  testParser "integer" integer "123 abc"
  testParser "integer" integer "abc 123"
  
  -- 测试 float
  testParser "float" float "123.45 rest"
  testParser "float" float "123 rest"

-- 组合子测试
testCombinators : IO ()
testCombinators = do
  putStrLn "=== Combinator Tests ==="
  
  -- 测试 <&> (map)
  let doubleParser = integer <&> (*2)
  testParser "double integer" doubleParser "42"
  
  -- 测试 &> (replace)
  let successParser = symbol (unpack "ok") &> "SUCCESS"
  testParser "replace with SUCCESS" successParser "ok rest"
  
  -- 测试 <*> (sequence)
  let pairParser = integer <*> integer
  testParser "pair of integers" pairParser "12 34"
  
  -- 测试 <* 和 *>
  let middleParser = symbol (unpack "(") *> integer <* symbol (unpack ")")
  testParser "integer in parentheses" middleParser "(42)"
  
  -- 测试 <**> (cons)
  let listHeadParser = integer <**> many integer
  testParser "integer list (cons)" listHeadParser "1 2 3 4"

-- 选择和重复测试
testChoiceAndRepetition : IO ()
testChoiceAndRepetition = do
  putStrLn "=== Choice and Repetition Tests ==="
  
  -- 测试 <|> (choice)
  let numberOrSymbol = (integer <&> show) <|> (symbol (unpack "+") <&> pack)
  testParser "number or plus" numberOrSymbol "123"
  testParser "number or plus" numberOrSymbol "+"
  testParser "number or plus" numberOrSymbol "abc"
  
  -- 测试 many
  let manyDigits = many (satisfy isDigit)
  testParser "many digits" manyDigits "12345abc"
  testParser "many digits" manyDigits "abc123"
  
  -- 测试 some
  let someDigits = some (satisfy isDigit)
  testParser "some digits" someDigits "12345abc"
  testParser "some digits" someDigits "abc123"
  
  -- 测试 empty
  testParser "empty list" (empty {a=Char}) "anything"

-- 复杂组合测试
-- testComplexCombinations : IO ()
-- testComplexCombinations = do
--   putStrLn "=== Complex Combination Tests ==="
  
--   -- 测试简单表达式解析 (数字 + 操作符 + 数字)
--   let simpleExpr = do
--         x <- integer
--         op <- symbol (unpack "+") <|> symbol (unpack "-")
--         y <- integer
--         pure (x, pack op, y)
--   testParser "simple expression" simpleExpr "12 + 34"
--   testParser "simple expression" simpleExpr "56-78"
  
--   -- 测试带括号的标识符列表
--   let identList = symbol (unpack "(") *> 
--                   (some (satisfy isAlpha) `sepBy` symbol (unpack ",")) <*
--                   symbol (unpack ")")
--     where
--       sepBy : Parser a -> Parser b -> Parser (List a)
--       sepBy p sep = (p <**> many (sep *> p)) <|> empty
--   testParser "identifier list" identList "(a,b,c)"
--   testParser "identifier list" identList "(hello, world)"
  
--   -- 测试嵌套结构
--   let nested = symbol (unpack "[") *> 
--                many (integer <|> (symbol (unpack "[") *> integer <* symbol (unpack "]"))) <*
--                symbol (unpack "]")
--   testParser "nested structure" nested "[1 [2] 3]"

-- -- 错误处理测试
-- testErrorHandling : IO ()
-- testErrorHandling = do
--   putStrLn "=== Error Handling Tests ==="
  
--   -- 测试期望失败的情况
--   testParser "integer on letters" integer "abc"
--   testParser "float on integer" float "123"
--   testParser "some digits on letters" (some (satisfy isDigit)) "abc"
  
--   -- 测试部分成功的情况
--   let strictInteger = integer <* symbol (unpack "END")
--   testParser "integer followed by END" strictInteger "123"
--   testParser "integer followed by END" strictInteger "123 END"

-- 主测试函数
public export
testCombinator : IO ()
testCombinator = do
  putStrLn "Starting Combinator Tests...\n"
  testBasicParsers
  testCombinators  
  testChoiceAndRepetition
  -- testComplexCombinations
  -- testErrorHandling
  putStrLn "All tests completed!"