module Calculator (module Calculator.Parser, compileAndRun) where
		
import Calculator.Interpreter
import Calculator.Parser
import Calculator.Parser.Expression

--import Calculator.Parsec.Parsing
--import Calculator.Parsec.Interpreter

compileAndRun = (interpret =<<) . compile
--compileAndRun = (fmap run) . compile
