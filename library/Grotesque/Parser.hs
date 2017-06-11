{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Grotesque.Parser where

import Grotesque.Language

import Data.List.NonEmpty (NonEmpty)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Text.Megaparsec (Dec, ParseError)
import Text.Megaparsec.Text (Parser)

import qualified Data.Bits as Bits
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Lexer as Lexer
import qualified Text.Read as Read

parseDocument :: Text -> Either (ParseError Char Dec) Document
parseDocument = M.parse getDocument ""

getDocument :: Parser Document
getDocument = do
  _ <- getSpace
  documentValue <- M.many getDefinition
  M.eof
  pure Document {..}

getDefinition :: Parser Definition
getDefinition =
  M.choice
    [ fmap DefinitionOperation getOperationDefinition
    , fmap DefinitionFragment getFragmentDefinition
    , fmap DefinitionTypeSystem getTypeSystemDefinition
    ]

getOperationDefinition :: Parser OperationDefinition
getOperationDefinition =
  M.choice [getLongOperationDefinition, getShortOperationDefinition]

getLongOperationDefinition :: Parser OperationDefinition
getLongOperationDefinition = do
  operationDefinitionOperationType <- getOperationType
  operationDefinitionName <- M.optional (M.try getName)
  operationDefinitionVariableDefinitions <-
    M.optional (M.try getVariableDefinitions)
  operationDefinitionDirectives <- M.optional (M.try getDirectives)
  operationDefinitionSelectionSet <- getSelectionSet
  pure OperationDefinition {..}

getOperationType :: Parser OperationType
getOperationType =
  M.choice
    [ fmap (const OperationTypeQuery) (getSymbol "query")
    , fmap (const OperationTypeMutation) (getSymbol "mutation")
    , fmap (const OperationTypeSubscription) (getSymbol "subscription")
    ]

getVariableDefinitions :: Parser VariableDefinitions
getVariableDefinitions = do
  variableDefinitionsValue <- getInParentheses (M.many getVariableDefinition)
  pure VariableDefinitions {..}

getInParentheses :: Parser a -> Parser a
getInParentheses = M.between (getSymbol "(") (getSymbol ")")

getVariableDefinition :: Parser VariableDefinition
getVariableDefinition = do
  variableDefinitionVariable <- getVariable
  _ <- getColon
  variableDefinitionType <- getType
  variableDefinitionDefaultValue <- M.optional getDefaultValue
  pure VariableDefinition {..}

getType :: Parser Type
getType =
  M.choice
    [ fmap TypeNonNull (M.try getNonNullType)
    , fmap TypeNamed getNamedType
    , fmap TypeList getListType
    ]

getNamedType :: Parser NamedType
getNamedType = do
  namedTypeValue <- getName
  pure NamedType {..}

getListType :: Parser ListType
getListType = do
  listTypeValue <- getInBrackets getType
  pure ListType {..}

getInBrackets :: Parser a -> Parser a
getInBrackets = M.between (getSymbol "[") (getSymbol "]")

getNonNullType :: Parser NonNullType
getNonNullType = getLexeme (M.choice [getNonNullNamedType, getNonNullListType])

getNonNullNamedType :: Parser NonNullType
getNonNullNamedType = do
  value <- getNamedType
  _ <- getExclamationPoint
  pure (NonNullTypeNamed value)

getExclamationPoint :: Parser Char
getExclamationPoint = M.char '!'

getNonNullListType :: Parser NonNullType
getNonNullListType = do
  value <- getListType
  _ <- getExclamationPoint
  pure (NonNullTypeList value)

getDefaultValue :: Parser DefaultValue
getDefaultValue = do
  _ <- getSymbol "="
  defaultValueValue <- getValue
  pure DefaultValue {..}

getDirectives :: Parser Directives
getDirectives = do
  directivesValue <- getNonEmpty getDirective getDirective
  pure Directives {..}

getNonEmpty :: Parser a -> Parser a -> Parser (NonEmpty a)
getNonEmpty getFirst getRest = do
  first <- getFirst
  rest <- M.many getRest
  pure (first NonEmpty.:| rest)

getDirective :: Parser Directive
getDirective = do
  _ <- getSymbol "@"
  directiveName <- getName
  directiveArguments <- M.optional getArguments
  pure Directive {..}

getShortOperationDefinition :: Parser OperationDefinition
getShortOperationDefinition = do
  let operationDefinitionOperationType = OperationTypeQuery
      operationDefinitionName = Nothing
      operationDefinitionVariableDefinitions = Nothing
      operationDefinitionDirectives = Nothing
  operationDefinitionSelectionSet <- getSelectionSet
  pure OperationDefinition {..}

getSelectionSet :: Parser SelectionSet
getSelectionSet = do
  selectionSetValue <- getInBraces (M.many getSelection)
  pure SelectionSet {..}

getInBraces :: Parser a -> Parser a
getInBraces = M.between (getSymbol "{") (getSymbol "}")

getSelection :: Parser Selection
getSelection =
  M.choice
    [ fmap SelectionField getField
    , fmap SelectionFragmentSpread (M.try getFragmentSpread)
    , fmap SelectionInlineFragment getInlineFragment
    ]

getField :: Parser Field
getField = do
  fieldAlias <- M.optional (M.try getAlias)
  fieldName <- getName
  fieldArguments <- M.optional (M.try getArguments)
  fieldDirectives <- M.optional (M.try getDirectives)
  fieldSelectionSet <- M.optional (M.try getSelectionSet)
  pure Field {..}

getAlias :: Parser Alias
getAlias = do
  aliasValue <- getName
  _ <- getColon
  pure Alias {..}

getColon :: Parser String
getColon = getSymbol ":"

getName :: Parser Name
getName = do
  value <-
    getLexeme (getNonEmpty (M.oneOf nameFirstChars) (M.oneOf nameRestChars))
  pure Name {nameValue = Text.pack (NonEmpty.toList value)}

nameFirstChars :: String
nameFirstChars = concat ["_", ['A' .. 'Z'], ['a' .. 'z']]

nameRestChars :: String
nameRestChars = nameFirstChars ++ ['0' .. '9']

getArguments :: Parser Arguments
getArguments =
  getInParentheses
    (do argumentsValue <- M.many getArgument
        pure Arguments {..})

getArgument :: Parser Argument
getArgument = do
  argumentName <- getName
  _ <- getColon
  argumentValue <- getValue
  pure Argument {..}

getValue :: Parser Value
getValue =
  M.choice
    [ fmap ValueVariable getVariable
    , fmap ValueFloat (M.try getFloat)
    , fmap ValueInt getInt
    , fmap ValueString getString
    , fmap ValueBoolean getBoolean
    , fmap (const ValueNull) getNull
    , fmap ValueEnum getEnum
    , fmap ValueList getList
    , fmap ValueObject getObject
    ]

getVariable :: Parser Variable
getVariable = do
  _ <- getSymbol "$"
  variableValue <- getName
  pure Variable {..}

getInt :: Parser Integer
getInt = getLexeme (getAndRead getIntegerPart)

getAndRead :: Read a => Parser String -> Parser a
getAndRead get = do
  string <- get
  case Read.readMaybe string of
    Nothing -> fail "getAndRead: no parse"
    Just value -> pure value

getIntegerPart :: Parser String
getIntegerPart = M.choice [M.try getZero, getNonZero]

getZero :: Parser String
getZero = do
  negativeSign <- getNegativeSign
  zero <- M.char '0'
  pure [negativeSign, zero]

getNegativeSign :: Parser Char
getNegativeSign = M.choice [M.char '-', pure ' ']

getNonZero :: Parser String
getNonZero = do
  negativeSign <- getNegativeSign
  nonZero <- getNonEmpty getNonZeroDigit M.digitChar
  pure (negativeSign : NonEmpty.toList nonZero)

getNonZeroDigit :: Parser Char
getNonZeroDigit = M.oneOf ['1' .. '9']

getFloat :: Parser Scientific
getFloat =
  getLexeme
    (M.choice
       [ M.try getFractionalExponentFloat
       , M.try getFractionalFloat
       , getExponentFloat
       ])

getFractionalFloat :: Parser Scientific
getFractionalFloat =
  getAndRead
    (do integerPart <- getIntegerPart
        fractionalPart <- getFractionalPart
        pure (integerPart ++ fractionalPart))

getFractionalPart :: Parser String
getFractionalPart = do
  decimalPoint <- getDecimalPoint
  digits <- M.some M.digitChar
  pure (decimalPoint : digits)

getDecimalPoint :: Parser Char
getDecimalPoint = M.char '.'

getExponentFloat :: Parser Scientific
getExponentFloat =
  getAndRead
    (do integerPart <- getIntegerPart
        exponentPart <- getExponentPart
        pure (integerPart ++ exponentPart))

getExponentPart :: Parser String
getExponentPart = do
  exponentIndicator <- getExponentIndicator
  sign <- getSign
  digits <- M.some M.digitChar
  pure (exponentIndicator : sign : digits)

getExponentIndicator :: Parser Char
getExponentIndicator = M.char' 'e'

getSign :: Parser Char
getSign = M.choice [M.oneOf "+-", pure '+']

getFractionalExponentFloat :: Parser Scientific
getFractionalExponentFloat =
  getAndRead
    (do integerPart <- getIntegerPart
        fractionalPart <- getFractionalPart
        exponentPart <- getExponentPart
        pure (concat [integerPart, fractionalPart, exponentPart]))

getString :: Parser Text
getString = getLexeme (getInQuotes (getText (M.many getCharacter)))

getInQuotes :: Parser a -> Parser a
getInQuotes get = do
  _ <- getQuote
  value <- get
  _ <- getQuote
  pure value

getQuote :: Parser Char
getQuote = M.char '"'

getText :: Parser String -> Parser Text
getText get = do
  value <- get
  pure (Text.pack value)

getCharacter :: Parser Char
getCharacter =
  M.choice
    [ getStringCharacter
    , M.try getSurrogateCharacter
    , M.try getUnicodeCharacter
    , getEscapedCharacter
    ]

getStringCharacter :: Parser Char
getStringCharacter =
  M.oneOf
    (concat
       [ "\x0009"
       , ['\x0020' .. '\x0021']
       , ['\x0023' .. '\x005b']
       , ['\x005d' .. '\xffff']
       ])

getSurrogateCharacter :: Parser Char
getSurrogateCharacter = do
  (h, l) <-
    getAndRead
      (do h <- getUnicodeEscape
          l <- getUnicodeEscape
          pure (concat ["(0x", h, ", 0x", l, ")"]))
  let n = combineSurrogates h l
  if not (validHighSurrogate h) || not (validLowSurrogate l)
    then fail "invalid surrogate"
    else if not (validChar n)
           then fail "impossible"
           else pure (toEnum n)

validHighSurrogate :: Int -> Bool
validHighSurrogate h = 0xd800 <= h && h <= 0xdbff

validLowSurrogate :: Int -> Bool
validLowSurrogate l = 0xdc00 <= l && l <= 0xdfff

combineSurrogates :: Int -> Int -> Int
combineSurrogates h l =
  0x010000 + Bits.shiftL (h Bits..&. 0x0003ff) 10 + (l Bits..&. 0x0003ff)

validChar :: Int -> Bool
validChar n = n <= fromEnum (maxBound :: Char)

getUnicodeCharacter :: Parser Char
getUnicodeCharacter =
  getAndRead
    (do digits <- getUnicodeEscape
        pure (concat ["'\\x", digits, "'"]))

getUnicodeEscape :: Parser String
getUnicodeEscape = do
  _ <- getBackslash
  _ <- M.char 'u'
  M.count 4 M.hexDigitChar

getEscapedCharacter :: Parser Char
getEscapedCharacter = do
  _ <- getBackslash
  escape <- M.oneOf "\"\\/bfnrt"
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
getBoolean = M.choice [fmap (const True) getTrue, fmap (const False) getFalse]

getTrue :: Parser String
getTrue = getSymbol "true"

getFalse :: Parser String
getFalse = getSymbol "false"

getNull :: Parser String
getNull = getSymbol "null"

getEnum :: Parser Name
getEnum = getName

getList :: Parser [Value]
getList = getInBrackets (M.many getValue)

getObject :: Parser [ObjectField]
getObject = getInBraces (M.many getObjectField)

getObjectField :: Parser ObjectField
getObjectField = do
  objectFieldName <- getName
  _ <- getColon
  objectFieldValue <- getValue
  pure ObjectField {..}

getFragmentSpread :: Parser FragmentSpread
getFragmentSpread = do
  _ <- getEllipsis
  fragmentSpreadName <- getFragmentName
  fragmentSpreadDirectives <- M.optional getDirectives
  pure FragmentSpread {..}

getEllipsis :: Parser String
getEllipsis = getSymbol "..."

getFragmentName :: Parser FragmentName
getFragmentName = do
  fragmentNameValue <- getName
  case Text.unpack (nameValue fragmentNameValue) of
    "on" -> fail "invalid fragment name"
    _ -> pure FragmentName {..}

getInlineFragment :: Parser InlineFragment
getInlineFragment = do
  _ <- getEllipsis
  inlineFragmentTypeCondition <- M.optional getTypeCondition
  inlineFragmentDirectives <- M.optional getDirectives
  inlineFragmentSelectionSet <- getSelectionSet
  pure InlineFragment {..}

getTypeCondition :: Parser TypeCondition
getTypeCondition = do
  _ <- getSymbol "on"
  typeConditionValue <- getNamedType
  pure TypeCondition {..}

getSymbol :: String -> Parser String
getSymbol = Lexer.symbol getSpace

getLexeme :: Parser a -> Parser a
getLexeme = Lexer.lexeme getSpace

getSpace :: Parser ()
getSpace =
  Lexer.space
    (do _ <- M.oneOf "\x0009\x000a\x000d\x0020,\xfeff"
        pure ())
    (Lexer.skipLineComment "#")
    (fail "no block comments")

getFragmentDefinition :: Parser FragmentDefinition
getFragmentDefinition = do
  _ <- getSymbol "fragment"
  fragmentName <- getFragmentName
  fragmentTypeCondition <- getTypeCondition
  fragmentDirectives <- M.optional getDirectives
  fragmentSelectionSet <- getSelectionSet
  pure FragmentDefinition {..}

getTypeSystemDefinition :: Parser TypeSystemDefinition
getTypeSystemDefinition =
  M.choice
    [ fmap TypeSystemDefinitionSchema getSchemaDefinition
    , fmap TypeSystemDefinitionType getTypeDefinition
    , fmap TypeSystemDefinitionTypeExtension getTypeExtensionDefinition
    , fmap TypeSystemDefinitionDirective getDirectiveDefinition
    ]

getSchemaDefinition :: Parser SchemaDefinition
getSchemaDefinition = do
  _ <- getSymbol "schema"
  schemaDefinitionDirectives <- M.optional getDirectives
  schemaDefinitionOperationTypes <- getOperationTypeDefintions
  pure SchemaDefinition {..}

getOperationTypeDefintions :: Parser OperationTypeDefinitions
getOperationTypeDefintions = do
  operationTypeDefinitionsValue <-
    getInBraces (M.many getOperationTypeDefintion)
  pure OperationTypeDefinitions {..}

getOperationTypeDefintion :: Parser OperationTypeDefinition
getOperationTypeDefintion = do
  operationTypeDefinitionOperation <- getOperationType
  _ <- getColon
  operationTypeDefinitionType <- getNamedType
  pure OperationTypeDefinition {..}

getTypeDefinition :: Parser TypeDefinition
getTypeDefinition =
  M.choice
    [ fmap TypeDefinitionScalar getScalarTypeDefinition
    , fmap TypeDefinitionObject getObjectTypeDefinition
    , fmap TypeDefinitionInterface getInterfaceTypeDefinition
    , fmap TypeDefinitionUnion getUnionTypeDefinition
    , fmap TypeDefinitionEnum getEnumTypeDefinition
    , fmap TypeDefinitionInputObject getInputObjectTypeDefinition
    ]

getScalarTypeDefinition :: Parser ScalarTypeDefinition
getScalarTypeDefinition = do
  _ <- getSymbol "scalar"
  scalarTypeDefinitionName <- getName
  scalarTypeDefinitionDirectives <- M.optional getDirectives
  pure ScalarTypeDefinition {..}

getObjectTypeDefinition :: Parser ObjectTypeDefinition
getObjectTypeDefinition = do
  _ <- getSymbol "type"
  objectTypeDefinitionName <- getName
  objectTypeDefinitionInterfaces <- M.optional getInterfaces
  objectTypeDefinitionDirectives <- M.optional getDirectives
  objectTypeDefinitionFields <- getFieldDefinitions
  pure ObjectTypeDefinition {..}

getInterfaces :: Parser Interfaces
getInterfaces = do
  _ <- getSymbol "implements"
  interfacesValue <- getNonEmpty getNamedType getNamedType
  pure Interfaces {..}

getFieldDefinitions :: Parser FieldDefinitions
getFieldDefinitions = do
  fieldDefinitionsValue <- getInBraces (M.many getFieldDefinition)
  pure FieldDefinitions {..}

getFieldDefinition :: Parser FieldDefinition
getFieldDefinition = do
  fieldDefinitionName <- getName
  fieldDefinitionArguments <- M.optional getInputValueDefinitions
  _ <- getColon
  fieldDefinitionType <- getType
  fieldDefinitionDirectives <- M.optional getDirectives
  pure FieldDefinition {..}

getInputValueDefinitions :: Parser InputValueDefinitions
getInputValueDefinitions = do
  inputValueDefinitionsValue <-
    getInParentheses (M.many getInputValueDefinition)
  pure InputValueDefinitions {..}

getInputValueDefinition :: Parser InputValueDefinition
getInputValueDefinition = do
  inputValueDefinitionName <- getName
  _ <- getColon
  inputValueDefinitionType <- getType
  inputValueDefinitionDefaultValue <- M.optional getDefaultValue
  inputValueDefinitionDirectives <- M.optional getDirectives
  pure InputValueDefinition {..}

getInterfaceTypeDefinition :: Parser InterfaceTypeDefinition
getInterfaceTypeDefinition = do
  _ <- getSymbol "interface"
  interfaceTypeDefinitionName <- getName
  interfaceTypeDefinitionDirectives <- M.optional getDirectives
  interfaceTypeDefinitionFields <- getFieldDefinitions
  pure InterfaceTypeDefinition {..}

getUnionTypeDefinition :: Parser UnionTypeDefinition
getUnionTypeDefinition = do
  _ <- getSymbol "union"
  unionTypeDefinitionName <- getName
  unionTypeDefinitionDirectives <- M.optional getDirectives
  _ <- getSymbol "="
  unionTypeDefinitionTypes <- getUnionTypes
  pure UnionTypeDefinition {..}

getUnionTypes :: Parser UnionTypes
getUnionTypes = do
  unionTypesValue <-
    getNonEmpty getNamedType (withPrefix (getSymbol "|") getNamedType)
  pure UnionTypes {..}

withPrefix :: Parser b -> Parser a -> Parser a
withPrefix getPrefix get = do
  _ <- getPrefix
  get

getEnumTypeDefinition :: Parser EnumTypeDefinition
getEnumTypeDefinition = do
  _ <- getSymbol "enum"
  enumTypeDefinitionName <- getName
  enumTypeDefinitionDirectives <- M.optional getDirectives
  enumTypeDefinitionValues <- getEnumValues
  pure EnumTypeDefinition {..}

getEnumValues :: Parser EnumValues
getEnumValues = do
  enumValuesValue <- getInBraces (M.many getEnumValueDefinition)
  pure EnumValues {..}

getEnumValueDefinition :: Parser EnumValueDefinition
getEnumValueDefinition = do
  enumValueDefinitionName <- getName
  enumValueDefinitionDirectives <- M.optional getDirectives
  pure EnumValueDefinition {..}

getInputObjectTypeDefinition :: Parser InputObjectTypeDefinition
getInputObjectTypeDefinition = do
  _ <- getSymbol "input"
  inputObjectTypeDefinitionName <- getName
  inputObjectTypeDefinitionDirectives <- M.optional getDirectives
  inputObjectTypeDefinitionFields <- getInputFieldDefinitions
  pure InputObjectTypeDefinition {..}

getInputFieldDefinitions :: Parser InputFieldDefinitions
getInputFieldDefinitions = do
  inputFieldDefinitionsValue <- getInBraces (M.many getInputValueDefinition)
  pure InputFieldDefinitions {..}

getTypeExtensionDefinition :: Parser TypeExtensionDefinition
getTypeExtensionDefinition = do
  _ <- getSymbol "extend"
  typeExtensionDefinitionValue <- getObjectTypeDefinition
  pure TypeExtensionDefinition {..}

getDirectiveDefinition :: Parser DirectiveDefinition
getDirectiveDefinition = do
  _ <- getSymbol "directive"
  _ <- getSymbol "@"
  directiveDefinitionName <- getName
  directiveDefinitionArguments <- M.optional getInputValueDefinitions
  _ <- getSymbol "on"
  directiveDefinitionLocations <- getDirectiveLocations
  pure DirectiveDefinition {..}

getDirectiveLocations :: Parser DirectiveLocations
getDirectiveLocations = do
  directiveLocationsValue <-
    getNonEmpty getName (withPrefix (getSymbol "|") getName)
  pure DirectiveLocations {..}
