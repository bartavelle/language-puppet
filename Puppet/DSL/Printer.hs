module Puppet.DSL.Printer (
    showAST,
    showVarMap
) where

import Text.PrettyPrint
import Puppet.DSL.Types
import qualified Data.Map as Map
import Text.Parsec.Pos

-- | This shows the parsed AST a bit like the original syntax.
showAST :: [Statement] -> String
showAST x = render (vcat (map showStatement x))

showStatement :: Statement -> Doc
showStatement (Node name x _) = text "node" <+> text name <+> lbrace $$ nest 4 (vcat (map showStatement x)) $$ rbrace
showStatement (VariableAssignment name x _) = text "$" <> text name <+> text "=" <+> showExpression x
showStatement (Include inc _) = text("include " ++ inc)
showStatement (Require req _) = text("require " ++ req)
showStatement (Resource rtype rname params Normal _) = text (rtype) <+> lbrace <+> showExpression rname <> text ":" $$ nest 4 ( showAssignments params )  <> text ";" $$ rbrace 
showStatement (Resource rtype rname params Virtual p) = text "@" <> showStatement (Resource rtype rname params Normal p)
showStatement (Resource rtype rname params Exported p) = text "@@" <> showStatement (Resource rtype rname params Normal p)
showStatement (ResourceDefault rtype params _) = text (rtype) <+> braces (showAssignments params)
showStatement (ClassDeclaration cname Nothing params statements _) = text "class" <+> text cname <+> parens ( hcat (punctuate (text ",") (map showOptionalParameter params)) ) <+> lbrace $$ nest 4 (vcat (map showStatement statements)) $$ rbrace
showStatement (ClassDeclaration cname (Just parent) params statements _) = text "class" <+> text cname <> parens ( hcat (punctuate (text ",") (map showOptionalParameter params)) ) <+> text "inherits" <+> text parent <+> lbrace $$ nest 4 (vcat (map showStatement statements)) $$ rbrace
showStatement (DefineDeclaration cname params statements _) = text "define" <+> text cname <+> parens ( hcat (punctuate (text ",") (map showOptionalParameter params)) ) <+> lbrace $$ nest 4 (vcat (map showStatement statements)) $$ rbrace
showStatement (ConditionalStatement x _) = text "CONDITION LIST" $$ nest 4 ( vcat (map showCondition x) )
showStatement x = text (show x)

showCondition :: (Expression, [Statement]) -> Doc
showCondition (BTrue, []) = empty
showCondition (e, stmts) = showExpression e <+> text "{" $$ nest 4 ( vcat (map showStatement stmts)) $$ text "}"

showOptionalParameter :: (String, Maybe Expression) -> Doc
showOptionalParameter (param, Nothing) = text "$" <> text param
showOptionalParameter (param, (Just e)) = text "$" <> text param <+> text "=" <+> showExpression e

showExpressionBuilder :: String -> Expression -> Expression -> Doc
showExpressionBuilder symb a b = char '(' <> showExpression a <+> text symb <+> showExpression b <> char ')'

showExpression :: Expression -> Doc
showExpression (Value x) = showValue x
showExpression (ConditionalValue var conds) = showExpression var <+> text "=>" <+> showExpression conds
showExpression (PlusOperation a b) = showExpressionBuilder "+" a b
showExpression (MinusOperation a b) = showExpressionBuilder "-" a b
showExpression (DivOperation a b) = showExpressionBuilder "/" a b
showExpression (MultiplyOperation a b) = showExpressionBuilder "*" a b
showExpression (ShiftLeftOperation a b) = showExpressionBuilder "<<" a b
showExpression (ShiftRightOperation a b) = showExpressionBuilder ">>" a b
showExpression (AndOperation a b) = showExpressionBuilder "and" a b
showExpression (OrOperation a b) = showExpressionBuilder "or" a b
showExpression (EqualOperation a b) = showExpressionBuilder "==" a b
showExpression (DifferentOperation a b) = showExpressionBuilder "!=" a b
showExpression (AboveOperation a b) = showExpressionBuilder ">" a b
showExpression (AboveEqualOperation a b) = showExpressionBuilder ">=" a b
showExpression (UnderEqualOperation a b) = showExpressionBuilder "<=" a b
showExpression (UnderOperation a b) = showExpressionBuilder "<" a b
showExpression (RegexpOperation a b) = showExpressionBuilder "=~" a b
showExpression (NotRegexpOperation a b) = showExpressionBuilder "!~" a b
showExpression (NotOperation a) =  char '(' <> char '!' <+> showExpression a <> char ')'
showExpression (NegOperation a) =  char '(' <> char '-' <+> showExpression a <> char ')'
showExpression (BTrue) =  text "true"
showExpression (BFalse) =  text "false"
showExpression (LookupOperation a b) = showExpression a <> char '[' <> showExpression b <> char ']'
showExpression x = text (show x)

showValue :: Value -> Doc
showValue (Literal x) = text( show x )
showValue (VariableReference x) = text "$" <> text x
showValue (FunctionCall funcname args) = text funcname <> parens ( hcat (map showExpression args ) )
showValue (PuppetArray x) = brackets ( hcat ( punctuate (text ", ") (map showExpression x)))
showValue (ResourceReference rtype rname) = text rtype <> brackets (showExpression rname)
showValue (PuppetHash (Parameters params)) = hang lbrace 2 (showAssignments params)  $$ rbrace
showValue (Integer x) = integer x
showValue x = text (show x)

showAssignments :: [(Expression, Expression)] -> Doc
showAssignments params = vcat ( punctuate (text ", ") (map showAssignment params ) )

showAssignment :: (Expression, Expression) -> Doc
showAssignment (param, value) = showExpression param <+> text "=>" <+> showExpression value

-- | Useful for displaying a map of variables.
showVarMap :: Map.Map String (Expression, SourcePos) -> String
showVarMap x = render $ vcat (map descLine (Map.toList x))
    where
        descLine (name, (expr, pos)) = text name <+> char '=' <+> showExpression expr <+> char '(' <> text (show pos) <> char ')'
