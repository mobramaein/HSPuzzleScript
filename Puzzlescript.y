
{
module Main where
}


%name puzzleScript 
%tokentype { Token }
%error { parseError }

-- List de los tokens.
%token
   --objName {TokenObjName $$}
   up {TokenUp}
   down {TokenDown}
   right {TokenRight}
   left {TokenLeft}
   '<' {TokenRelLeft}
   '>' {TokenRelRight}
   'v' {TokenRelDown}
   '^' {TokenRelUp}
   '[' {TokenRuleBegin}
   ']' {TokenRuleEnd}
   '|' {TokenRulePipe}
   "->" {TokenRuleImply}
%%

--Grammar Definiton 
Keyword : up {} | down {} | right {} | left {} | '>' {} | '<' {} | '^' {} | 'v' {}
Pattern : '[' Keyword  '|' Keyword  ']' {}
Rule : Pattern "->" Pattern {} 

 

--Definition of la function del error
{
parseError :: [Token] -> a
parseError _ = error "Mistake del analysis "


--Tipos de datos para representar internamente la gramatica:		
data Keyword = Keyw String 
data Pattern = Keyword String Keyword String deriving (Eq,Show) 
data Rule =  Pattern Pattern deriving (Eq,Show)

--Tipo de datos para los tokens:


data Token = TokenUp 
	| TokenDown 
	| TokenLeft 
	| TokenRight 
	| TokenRelLeft 
	| TokenRelRight 
	| TokenRelUp 
	| TokenRelDown 
	| TokenRuleBegin 
	| TokenRuleEnd 
	| TokenRulePipe 
	| TokenRuleImply
	deriving (Eq,Show)

-- Analizador lexico:
lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
      | isSpace c = lexer cs
      | isAlpha c = lexVar (c:cs)

lexer ('<':cs) = TokenRelLeft : lexer cs
lexer ('>':cs) = TokenRelRight : lexer cs
lexer ('v':cs) = TokenRelDown : lexer cs
lexer ('^':cs) = TokenRelUp : lexer cs
lexer ('<':cs) = TokenMenor : lexer cs
lexer ('[':cs) = TokenRuleBegin : lexer cs
lexer (']':cs) = TokenRuleEnd : lexer cs
lexer ('|':cs) = TokenRulePipe : lexer cs
lexer ("->":cs) = TokenRuleImply : lexer cs 

lexVar cs = 
	case span isAlpha cs of {
	("up",rest) -> TokenUp : lexer rest;
	("down",rest) -> TokenDown : lexer rest;
	("right",rest) -> TokenRight : lexer rest;
	("left",rest) -> TokenLeft : lexer rest;
	--(objName,rest) -> TokenObjName objName : lexer rest }
	  
-- Pruebas:	  
-- getContents es una accion que obtiene un string de input
-- >>= es una composicion de acciones 
-- . es composicion de funciones
-- (print . lexer) es una accion que despliega el analisis lexico de un string
-- lexer es el analizador lexico
-- parse es el analizador sintactico
parseExpr :: String->  String  
parseExpr = getContents >>= print . puzzleScript .lexer

}
