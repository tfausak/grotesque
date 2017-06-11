module Grotesque.Parser where

import Grotesque.Language

import Data.List.NonEmpty (NonEmpty)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Text.Megaparsec.Text (Parser)

import qualified Control.Monad as Monad
import qualified Data.Bits as Bits
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Lexer as Lexer
import qualified Text.Read as Read


getDocument :: Parser Document
getDocument = Document
  <$> (getSpace *> M.many getDefinition <* M.eof)


getDefinition :: Parser Definition
getDefinition = M.choice
  [ DefinitionOperation <$> getOperationDefinition
  , DefinitionFragment <$> getFragmentDefinition
  , DefinitionTypeSystem <$> getTypeSystemDefinition
  ]


getOperationDefinition :: Parser OperationDefinition
getOperationDefinition = M.choice
  [ M.try getLongOperationDefinition
  , getShortOperationDefinition
  ]


getLongOperationDefinition :: Parser OperationDefinition
getLongOperationDefinition = OperationDefinition
  <$> getOperationType
  <*> M.optional (M.try getName)
  <*> M.optional (M.try getVariableDefinitions)
  <*> M.optional (M.try getDirectives)
  <*> getSelectionSet


getOperationType :: Parser OperationType
getOperationType = M.choice
  [ getSymbol "query" *> pure OperationTypeQuery
  , getSymbol "mutation" *> pure OperationTypeMutation
  , getSymbol "subscription" *> pure OperationTypeSubscription
  ]


getVariableDefinitions :: Parser VariableDefinitions
getVariableDefinitions = getInParentheses $ VariableDefinitions
  <$> M.many getVariableDefinition


getInParentheses :: Parser a -> Parser a
getInParentheses = M.between (getSymbol "(") (getSymbol ")")


getVariableDefinition :: Parser VariableDefinition
getVariableDefinition = VariableDefinition
  <$> (getVariable <* getColon)
  <*> getType
  <*> M.optional getDefaultValue


getType :: Parser Type
getType = M.choice
  [ TypeNonNull <$> M.try getNonNullType
  , TypeNamed <$> getNamedType
  , TypeList <$> getListType
  ]


getNamedType :: Parser NamedType
getNamedType = NamedType
  <$> getName


getListType :: Parser ListType
getListType = getInBrackets $ ListType
  <$> getType


getInBrackets :: Parser a -> Parser a
getInBrackets = M.between (getSymbol "[") (getSymbol "]")


getNonNullType :: Parser NonNullType
getNonNullType = getLexeme $ M.choice
  [ getNonNullNamedType
  , getNonNullListType
  ]


getNonNullNamedType :: Parser NonNullType
getNonNullNamedType = NonNullTypeNamed
  <$> (getNamedType <* getExclamationPoint)


getExclamationPoint :: Parser Char
getExclamationPoint = M.char '!'


getNonNullListType :: Parser NonNullType
getNonNullListType = NonNullTypeList
  <$> (getListType <* getExclamationPoint)


getDefaultValue :: Parser DefaultValue
getDefaultValue = DefaultValue
  <$> (getSymbol "=" *> getValue)


getDirectives :: Parser Directives
getDirectives = Directives
  <$> getNonEmpty getDirective


getNonEmpty :: Parser a -> Parser (NonEmpty a)
getNonEmpty x = getNonEmpty' x x


getNonEmpty' :: Parser a -> Parser a -> Parser (NonEmpty a)
getNonEmpty' getFirst getRest = (NonEmpty.:|)
  <$> getFirst
  <*> M.many getRest


getDirective :: Parser Directive
getDirective = Directive
  <$> (getSymbol "@" *> getName)
  <*> M.optional getArguments


getShortOperationDefinition :: Parser OperationDefinition
getShortOperationDefinition = OperationDefinition
  <$> pure OperationTypeQuery
  <*> pure Nothing
  <*> pure Nothing
  <*> pure Nothing
  <*> getSelectionSet


getSelectionSet :: Parser SelectionSet
getSelectionSet = getInBraces $ SelectionSet
  <$> M.many getSelection


getInBraces :: Parser a -> Parser a
getInBraces = M.between (getSymbol "{") (getSymbol "}")


getSelection :: Parser Selection
getSelection = M.choice
  [ SelectionField <$> getField
  , SelectionFragmentSpread <$> M.try getFragmentSpread
  , SelectionInlineFragment <$> getInlineFragment
  ]


getField :: Parser Field
getField = Field
  <$> M.optional (M.try getAlias)
  <*> getName
  <*> M.optional (M.try getArguments)
  <*> M.optional (M.try getDirectives)
  <*> M.optional (M.try getSelectionSet)


getAlias :: Parser Alias
getAlias = Alias
  <$> (getName <* getColon)


getColon :: Parser String
getColon = getSymbol ":"


getName :: Parser Name
getName = getLexeme $ (Name . Text.pack . NonEmpty.toList)
  <$> getNonEmpty' (M.oneOf nameFirstChars) (M.oneOf nameRestChars)


nameFirstChars :: [Char]
nameFirstChars = concat [['_'], ['A' .. 'Z'], ['a' .. 'z']]


nameRestChars :: [Char]
nameRestChars = concat [nameFirstChars, ['0' .. '9']]


getArguments :: Parser Arguments
getArguments = getInParentheses $ Arguments
  <$> M.many getArgument


getArgument :: Parser Argument
getArgument = Argument
  <$> (getName <* getColon)
  <*> getValue


getValue :: Parser Value
getValue = M.choice
  [ ValueVariable <$> getVariable
  , ValueFloat <$> M.try getFloat
  , ValueInt <$> getInt
  , ValueString <$> getString
  , ValueBoolean <$> getBoolean
  , getNull *> pure ValueNull
  , ValueEnum <$> getEnum
  , ValueList <$> getList
  , ValueObject <$> getObject
  ]


getVariable :: Parser Variable
getVariable = Variable
  <$> (getSymbol "$" *> getName)


-- TODO
getInt :: Parser Integer
getInt = getLexeme (do
  integerPart <- getIntegerPart
  case Read.readMaybe integerPart of
    Nothing -> fail "getInt: impossible"
    Just int -> pure int)


getIntegerPart :: Parser String
getIntegerPart = M.choice
  [ M.try getZero
  , getNonZero
  ]


-- TODO
getZero :: Parser String
getZero = do
  maybeNegativeSign <- M.optional getNegativeSign
  zero <- M.char '0'
  case maybeNegativeSign of
    Nothing -> pure ([zero])
    Just negativeSign -> pure (negativeSign : [zero])


getNegativeSign :: Parser Char
getNegativeSign = M.char '-'


-- TODO
getNonZero :: Parser String
getNonZero = do
  maybeNegativeSign <- M.optional getNegativeSign
  first <- getNonZeroDigit
  rest <- M.many M.digitChar
  case maybeNegativeSign of
    Nothing -> pure (first : rest)
    Just negativeSign -> pure (negativeSign : first : rest)


getNonZeroDigit :: Parser Char
getNonZeroDigit = M.oneOf ['1' .. '9']


getFloat :: Parser Scientific
getFloat = getLexeme $ M.choice
  [ M.try getFractionalExponentFloat
  , M.try getFractionalFloat
  , getExponentFloat
  ]


-- TODO
getFractionalFloat :: Parser Scientific
getFractionalFloat = do
  integerPart <- getIntegerPart
  fractionalPart <- getFractionalPart
  case Read.readMaybe (integerPart ++ fractionalPart) of
    Nothing -> fail "impossible"
    Just float -> pure float


getFractionalPart :: Parser String
getFractionalPart = (:)
  <$> getDecimalPoint
  <*> M.some M.digitChar


getDecimalPoint :: Parser Char
getDecimalPoint = M.char '.'


-- TODO
getExponentFloat :: Parser Scientific
getExponentFloat = do
  integerPart <- getIntegerPart
  exponentPart <- getExponentPart
  case Read.readMaybe (integerPart ++ exponentPart) of
    Nothing -> fail "impossible"
    Just float -> pure float


-- TODO
getExponentPart :: Parser String
getExponentPart = do
  exponentIndicator <- getExponentIndicator
  maybeSign <- M.optional getSign
  digits <- M.some M.digitChar
  case maybeSign of
    Nothing -> pure (exponentIndicator : digits)
    Just sign -> pure (exponentIndicator : sign : digits)


getExponentIndicator :: Parser Char
getExponentIndicator = M.char' 'e'


getSign :: Parser Char
getSign = M.oneOf ['+', '-']


-- TODO
getFractionalExponentFloat :: Parser Scientific
getFractionalExponentFloat = do
  integerPart <- getIntegerPart
  fractionalPart <- getFractionalPart
  exponentPart <- getExponentPart
  case Read.readMaybe (integerPart ++ fractionalPart ++ exponentPart) of
    Nothing -> fail "impossible"
    Just float -> pure float


getString :: Parser Text
getString = getLexeme $ Text.pack
  <$> (getQuote *> M.many getCharacter <* getQuote)


getQuote :: Parser Char
getQuote = M.char '"'


getCharacter :: Parser Char
getCharacter = M.choice
  [ getStringCharacter
  , M.try getSurrogateCharacter
  , M.try getUnicodeCharacter
  , getEscapedCharacter
  ]


getStringCharacter :: Parser Char
getStringCharacter = M.oneOf $ concat
  [ ['\x0009']
  , ['\x0020' .. '\x0021']
  , ['\x0023' .. '\x005b']
  , ['\x005d' .. '\xffff']
  ]


-- TODO
getSurrogateCharacter :: Parser Char
getSurrogateCharacter = do
  hd <- getUnicodeEscape
  ld <- getUnicodeEscape
  case Read.readMaybe ("(0x" ++ hd ++ ", 0x" ++ ld ++ ")") of
    Nothing -> fail "impossible"
    Just (h, l) -> if h < 0xd800 || h > 0xdbff || l < 0xdc00 || l > 0xdfff
      then fail "invalid surrogate"
      else let
        n = 0x010000 + (Bits.shiftL (h Bits..&. 0x0003ff) 10) + (l Bits..&. 0x0003ff)
        in if n <= fromEnum (maxBound :: Char)
          then pure (toEnum n)
          else fail "impossible"


-- TODO
getUnicodeCharacter :: Parser Char
getUnicodeCharacter = do
  digits <- getUnicodeEscape
  case Read.readMaybe ("\'\\x" ++ digits ++ "\'") of
    Nothing -> fail "impossible"
    Just character -> pure character


getUnicodeEscape :: Parser String
getUnicodeEscape = getBackslash *> M.char 'u' *> M.count 4 M.hexDigitChar


-- TODO
getEscapedCharacter :: Parser Char
getEscapedCharacter = do
  _ <- getBackslash
  escape <- M.oneOf ['"', '\\', '/', 'b', 'f', 'n', 'r', 't']
  case escape of
    '"' -> pure '\x0022'
    '\\' -> pure '\x005c'
    '/' -> pure '\x002f'
    'b' -> pure '\x0008'
    'f' -> pure '\x000c'
    'n' -> pure '\x000a'
    'r' -> pure '\x000d'
    't' -> pure '\x0009'
    _ -> fail "impossible"


getBackslash :: Parser Char
getBackslash = M.char '\\'


getBoolean :: Parser Bool
getBoolean = M.choice
  [ getTrue *> pure True
  , getFalse *> pure False
  ]


getTrue :: Parser String
getTrue = getSymbol "true"


getFalse :: Parser String
getFalse = getSymbol "false"


getNull :: Parser String
getNull = getSymbol "null"


getEnum :: Parser Name
getEnum = getName


getList :: Parser [Value]
getList = getInBrackets $ M.many getValue


getObject :: Parser [ObjectField]
getObject = getInBraces $ M.many getObjectField


getObjectField :: Parser ObjectField
getObjectField = ObjectField
  <$> (getName <* getColon)
  <*> getValue


getFragmentSpread :: Parser FragmentSpread
getFragmentSpread = FragmentSpread
  <$> (getEllipsis *> getFragmentName)
  <*> M.optional getDirectives


getEllipsis :: Parser String
getEllipsis = getSymbol "..."


-- TODO
getFragmentName :: Parser FragmentName
getFragmentName = do
  value <- getName
  case Text.unpack (nameValue value) of
    "on" -> fail "invalid fragment name"
    _ -> pure FragmentName
      { fragmentNameValue = value
      }


getInlineFragment :: Parser InlineFragment
getInlineFragment = InlineFragment
  <$> (getEllipsis *> M.optional getTypeCondition)
  <*> M.optional getDirectives
  <*> getSelectionSet


getTypeCondition :: Parser TypeCondition
getTypeCondition = TypeCondition
  <$> (getSymbol "on" *> getNamedType)


getSymbol :: String -> Parser String
getSymbol = Lexer.symbol getSpace


getLexeme :: Parser a -> Parser a
getLexeme = Lexer.lexeme getSpace


getSpace :: Parser ()
getSpace = Lexer.space
  (Monad.void $ M.oneOf ['\xfeff', '\x0009', '\x0020', '\x000a', '\x000d', ','])
  (Lexer.skipLineComment "#")
  (fail "no block comments")


getFragmentDefinition :: Parser FragmentDefinition
getFragmentDefinition = FragmentDefinition
  <$> (getSymbol "fragment" *> getFragmentName)
  <*> getTypeCondition
  <*> M.optional getDirectives
  <*> getSelectionSet


getTypeSystemDefinition :: Parser TypeSystemDefinition
getTypeSystemDefinition = M.choice
  [ TypeSystemDefinitionSchema <$> getSchemaDefinition
  , TypeSystemDefinitionType <$> getTypeDefinition
  , TypeSystemDefinitionTypeExtension <$> getTypeExtensionDefinition
  , TypeSystemDefinitionDirective <$> getDirectiveDefinition
  ]


getSchemaDefinition :: Parser SchemaDefinition
getSchemaDefinition = SchemaDefinition
  <$> (getSymbol "schema" *> M.optional getDirectives)
  <*> getOperationTypeDefintions


getOperationTypeDefintions :: Parser OperationTypeDefinitions
getOperationTypeDefintions = getInBraces $ OperationTypeDefinitions
  <$> M.many getOperationTypeDefintion


getOperationTypeDefintion :: Parser OperationTypeDefinition
getOperationTypeDefintion = OperationTypeDefinition
  <$> (getOperationType <* getColon)
  <*> getNamedType


getTypeDefinition :: Parser TypeDefinition
getTypeDefinition = M.choice
  [ TypeDefinitionScalar <$> getScalarTypeDefinition
  , TypeDefinitionObject <$> getObjectTypeDefinition
  , TypeDefinitionInterface <$> getInterfaceTypeDefinition
  , TypeDefinitionUnion <$> getUnionTypeDefinition
  , TypeDefinitionEnum <$> getEnumTypeDefinition
  , TypeDefinitionInputObject <$> getInputObjectTypeDefinition
  ]


getScalarTypeDefinition :: Parser ScalarTypeDefinition
getScalarTypeDefinition = ScalarTypeDefinition
  <$> (getSymbol "scalar" *> getName)
  <*> M.optional getDirectives


getObjectTypeDefinition :: Parser ObjectTypeDefinition
getObjectTypeDefinition = ObjectTypeDefinition
  <$> (getSymbol "type" *> getName)
  <*> M.optional getInterfaces
  <*> M.optional getDirectives
  <*> getFieldDefinitions


getInterfaces :: Parser Interfaces
getInterfaces = Interfaces
  <$> (getSymbol "implements" *> getNonEmpty getNamedType)


getFieldDefinitions :: Parser FieldDefinitions
getFieldDefinitions = getInBraces $ FieldDefinitions
  <$> M.many getFieldDefinition


getFieldDefinition :: Parser FieldDefinition
getFieldDefinition = FieldDefinition
  <$> getName
  <*> (M.optional getInputValueDefinitions <* getColon)
  <*> getType
  <*> M.optional getDirectives


getInputValueDefinitions :: Parser InputValueDefinitions
getInputValueDefinitions = getInParentheses $ InputValueDefinitions
  <$> M.many getInputValueDefinition


getInputValueDefinition :: Parser InputValueDefinition
getInputValueDefinition = InputValueDefinition
  <$> (getName <* getColon)
  <*> getType
  <*> M.optional getDefaultValue
  <*> M.optional getDirectives


getInterfaceTypeDefinition :: Parser InterfaceTypeDefinition
getInterfaceTypeDefinition = InterfaceTypeDefinition
  <$> (getSymbol "interface" *> getName)
  <*> M.optional getDirectives
  <*> getFieldDefinitions


getUnionTypeDefinition :: Parser UnionTypeDefinition
getUnionTypeDefinition = UnionTypeDefinition
  <$> (getSymbol "union" *> getName)
  <*> M.optional getDirectives
  <*> (getSymbol "=" *> getUnionTypes)


getUnionTypes :: Parser UnionTypes
getUnionTypes = UnionTypes
  <$> getNonEmpty' getNamedType (getSymbol "|" *> getNamedType)


getEnumTypeDefinition :: Parser EnumTypeDefinition
getEnumTypeDefinition = EnumTypeDefinition
  <$> (getSymbol "enum" *> getName)
  <*> M.optional getDirectives
  <*> getEnumValues


getEnumValues :: Parser EnumValues
getEnumValues = getInBraces $ EnumValues
  <$> M.many getEnumValueDefinition


getEnumValueDefinition :: Parser EnumValueDefinition
getEnumValueDefinition = EnumValueDefinition
  <$> getName
  <*> M.optional getDirectives


getInputObjectTypeDefinition :: Parser InputObjectTypeDefinition
getInputObjectTypeDefinition = InputObjectTypeDefinition
  <$> (getSymbol "input" *> getName)
  <*> M.optional getDirectives
  <*> getInputFieldDefinitions


getInputFieldDefinitions :: Parser InputFieldDefinitions
getInputFieldDefinitions = getInBraces $ InputFieldDefinitions
  <$> M.many getInputValueDefinition


getTypeExtensionDefinition :: Parser TypeExtensionDefinition
getTypeExtensionDefinition = TypeExtensionDefinition
  <$> (getSymbol "extend" *> getObjectTypeDefinition)


getDirectiveDefinition :: Parser DirectiveDefinition
getDirectiveDefinition = DirectiveDefinition
  <$> (getSymbol "directive" *> getSymbol "@" *> getName)
  <*> M.optional getInputValueDefinitions
  <*> (getSymbol "on" *> getDirectiveLocations)


getDirectiveLocations :: Parser DirectiveLocations
getDirectiveLocations = DirectiveLocations
  <$> getNonEmpty' getName (getSymbol "|" *> getName)
