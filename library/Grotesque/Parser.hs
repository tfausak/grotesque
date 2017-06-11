module Grotesque.Parser where

import Grotesque.Language

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
  value <- M.many getDefinition
  M.eof
  pure Document
    { documentValue = value
    }


getDefinition :: Parser Definition
getDefinition = M.choice
  [ fmap DefinitionOperation getOperationDefinition
  , fmap DefinitionFragment getFragmentDefinition
  , fmap DefinitionTypeSystem getTypeSystemDefinition
  ]


getOperationDefinition :: Parser OperationDefinition
getOperationDefinition = M.choice
  [ getLongOperationDefinition
  , getShortOperationDefinition
  ]


getLongOperationDefinition :: Parser OperationDefinition
getLongOperationDefinition = do
  operationType <- getOperationType
  name <- M.optional (M.try getName)
  variableDefinitions <- M.optional (M.try getVariableDefinitions)
  directives <- M.optional (M.try getDirectives)
  selectionSet <- getSelectionSet
  pure OperationDefinition
    { operationDefinitionOperationType = operationType
    , operationDefinitionName = name
    , operationDefinitionVariableDefinitions = variableDefinitions
    , operationDefinitionDirectives = directives
    , operationDefinitionSelectionSet = selectionSet
    }


getOperationType :: Parser OperationType
getOperationType = M.choice
  [ fmap (const OperationTypeQuery) (getSymbol "query")
  , fmap (const OperationTypeMutation) (getSymbol "mutation")
  , fmap (const OperationTypeSubscription) (getSymbol "subscription")
  ]


getVariableDefinitions :: Parser VariableDefinitions
getVariableDefinitions = getInParentheses (do
  value <- M.many getVariableDefinition
  pure VariableDefinitions
    { variableDefinitionsValue = value
    })


getInParentheses :: Parser a -> Parser a
getInParentheses = M.between (getSymbol "(") (getSymbol ")")


getVariableDefinition :: Parser VariableDefinition
getVariableDefinition = do
  variable <- getVariable
  _ <- getColon
  type_ <- getType
  defaultValue <- M.optional getDefaultValue
  pure VariableDefinition
    { variableDefinitionVariable = variable
    , variableDefinitionType = type_
    , variableDefinitionDefaultValue = defaultValue
    }


getType :: Parser Type
getType = M.choice
  [ fmap TypeNonNull (M.try getNonNullType)
  , fmap TypeNamed getNamedType
  , fmap TypeList getListType
  ]


getNamedType :: Parser NamedType
getNamedType = do
  value <- getName
  pure NamedType
    { namedTypeValue = value
    }


getListType :: Parser ListType
getListType = getInBrackets (do
  value <- getType
  pure ListType
    { listTypeValue = value
    })


getInBrackets :: Parser a -> Parser a
getInBrackets = M.between (getSymbol "[") (getSymbol "]")


getNonNullType :: Parser NonNullType
getNonNullType = getLexeme (M.choice
  [ getNonNullNamedType
  , getNonNullListType
  ])


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
  value <- getValue
  pure DefaultValue
    { defaultValueValue = value
    }


getDirectives :: Parser Directives
getDirectives = do
  list <- M.some getDirective
  case NonEmpty.nonEmpty list of
    Nothing -> fail "impossible"
    Just value -> pure Directives
      { directivesValue = value
      }


getDirective :: Parser Directive
getDirective = do
  _ <- getSymbol "@"
  name <- getName
  arguments <- M.optional getArguments
  pure Directive
    { directiveName = name
    , directiveArguments = arguments
    }


getShortOperationDefinition :: Parser OperationDefinition
getShortOperationDefinition = do
  selectionSet <- getSelectionSet
  pure OperationDefinition
    { operationDefinitionOperationType = OperationTypeQuery
    , operationDefinitionName = Nothing
    , operationDefinitionVariableDefinitions = Nothing
    , operationDefinitionDirectives = Nothing
    , operationDefinitionSelectionSet = selectionSet
    }


getSelectionSet :: Parser SelectionSet
getSelectionSet = getInBraces (do
  value <- M.many getSelection
  pure SelectionSet
    { selectionSetValue = value
    })


getInBraces :: Parser a -> Parser a
getInBraces = M.between (getSymbol "{") (getSymbol "}")


getSelection :: Parser Selection
getSelection = M.choice
  [ fmap SelectionField getField
  , fmap SelectionFragmentSpread (M.try getFragmentSpread)
  , fmap SelectionInlineFragment getInlineFragment
  ]


getField :: Parser Field
getField = do
  alias <- M.optional (M.try getAlias)
  name <- getName
  arguments <- M.optional (M.try getArguments)
  directives <- M.optional (M.try getDirectives)
  selectionSet <- M.optional (M.try getSelectionSet)
  pure Field
    { fieldAlias = alias
    , fieldName = name
    , fieldArguments = arguments
    , fieldDirectives = directives
    , fieldSelectionSet = selectionSet
    }


getAlias :: Parser Alias
getAlias = do
  value <- getName
  _ <- getColon
  pure Alias
    { aliasValue = value
    }


getColon :: Parser String
getColon = getSymbol ":"


getName :: Parser Name
getName = getLexeme (do
  let
    underscore = ['_']
    uppers = ['A' .. 'Z']
    lowers = ['a' .. 'z']
    digits = ['0' .. '9']
  first <- M.oneOf (concat [underscore, uppers, lowers])
  rest <- M.many (M.oneOf (concat [underscore, digits, uppers, lowers]))
  pure Name
    { nameValue = Text.pack (first : rest)
    })


getArguments :: Parser Arguments
getArguments = getInParentheses (do
  value <- M.many getArgument
  pure Arguments
    { argumentsValue = value
    })


getArgument :: Parser Argument
getArgument = do
  name <- getName
  _ <- getColon
  value <- getValue
  pure Argument
    { argumentName = name
    , argumentValue = value
    }


getValue :: Parser Value
getValue = M.choice
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
  value <- getName
  pure Variable
    { variableValue = value
    }


getInt :: Parser Integer
getInt = getLexeme (do
  integerPart <- getIntegerPart
  case Read.readMaybe integerPart of
    Nothing -> fail "impossible"
    Just int -> pure int)


getIntegerPart :: Parser String
getIntegerPart = M.choice
  [ M.try getZero
  , getNonZero
  ]


getZero :: Parser String
getZero = do
  maybeNegativeSign <- M.optional getNegativeSign
  zero <- M.char '0'
  case maybeNegativeSign of
    Nothing -> pure ([zero])
    Just negativeSign -> pure (negativeSign : [zero])


getNegativeSign :: Parser Char
getNegativeSign = M.char '-'


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
getFloat = getLexeme (M.choice
  [ M.try getFractionalExponentFloat
  , M.try getFractionalFloat
  , getExponentFloat
  ])


getFractionalFloat :: Parser Scientific
getFractionalFloat = do
  integerPart <- getIntegerPart
  fractionalPart <- getFractionalPart
  case Read.readMaybe (integerPart ++ fractionalPart) of
    Nothing -> fail "impossible"
    Just float -> pure float


getFractionalPart :: Parser String
getFractionalPart = do
  decimalPoint <- getDecimalPoint
  digits <- M.some M.digitChar
  pure (decimalPoint : digits)


getDecimalPoint :: Parser Char
getDecimalPoint = M.char '.'


getExponentFloat :: Parser Scientific
getExponentFloat = do
  integerPart <- getIntegerPart
  exponentPart <- getExponentPart
  case Read.readMaybe (integerPart ++ exponentPart) of
    Nothing -> fail "impossible"
    Just float -> pure float


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


getFractionalExponentFloat :: Parser Scientific
getFractionalExponentFloat = do
  integerPart <- getIntegerPart
  fractionalPart <- getFractionalPart
  exponentPart <- getExponentPart
  case Read.readMaybe (integerPart ++ fractionalPart ++ exponentPart) of
    Nothing -> fail "impossible"
    Just float -> pure float


getString :: Parser Text
getString = getLexeme (do
  _ <- getQuote
  characters <- M.many getCharacter
  _ <- getQuote
  pure (Text.pack characters))


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
getStringCharacter = M.oneOf (concat
  [ ['\x0009']
  , ['\x0020' .. '\x0021']
  , ['\x0023' .. '\x005b']
  , ['\x005d' .. '\xffff']
  ])


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


getUnicodeCharacter :: Parser Char
getUnicodeCharacter = do
  digits <- getUnicodeEscape
  case Read.readMaybe ("\'\\x" ++ digits ++ "\'") of
    Nothing -> fail "impossible"
    Just character -> pure character


getUnicodeEscape :: Parser String
getUnicodeEscape = do
  _ <- getBackslash
  _ <- M.char 'u'
  M.count 4 M.hexDigitChar


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
  [ fmap (const True) getTrue
  , fmap (const False) getFalse
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
getList = getInBrackets (M.many getValue)


getObject :: Parser [ObjectField]
getObject = getInBraces (M.many getObjectField)


getObjectField :: Parser ObjectField
getObjectField = do
  name <- getName
  _ <- getColon
  value <- getValue
  pure ObjectField
    { objectFieldName = name
    , objectFieldValue = value
    }


getFragmentSpread :: Parser FragmentSpread
getFragmentSpread = do
  _ <- getEllipsis
  name <- getFragmentName
  directives <- M.optional getDirectives
  pure FragmentSpread
    { fragmentSpreadName = name
    , fragmentSpreadDirectives = directives
    }


getEllipsis :: Parser String
getEllipsis = getSymbol "..."


getFragmentName :: Parser FragmentName
getFragmentName = do
  value <- getName
  case Text.unpack (nameValue value) of
    "on" -> fail "invalid fragment name"
    _ -> pure FragmentName
      { fragmentNameValue = value
      }


getInlineFragment :: Parser InlineFragment
getInlineFragment = do
  _ <- getEllipsis
  typeCondition <- M.optional getTypeCondition
  directives <- M.optional getDirectives
  selectionSet <- getSelectionSet
  pure InlineFragment
    { inlineFragmentTypeCondition = typeCondition
    , inlineFragmentDirectives = directives
    , inlineFragmentSelectionSet = selectionSet
    }


getTypeCondition :: Parser TypeCondition
getTypeCondition = do
  _ <- getSymbol "on"
  value <- getNamedType
  pure TypeCondition
    { typeConditionValue = value
    }


getSymbol :: String -> Parser String
getSymbol = Lexer.symbol getSpace


getLexeme :: Parser a -> Parser a
getLexeme = Lexer.lexeme getSpace


getSpace :: Parser ()
getSpace = Lexer.space
  (do
    _ <- M.oneOf ['\xfeff', '\x0009', '\x0020', '\x000a', '\x000d', ',']
    pure ())
  (Lexer.skipLineComment "#")
  (fail "no block comments")


getFragmentDefinition :: Parser FragmentDefinition
getFragmentDefinition = do
  _ <- getSymbol "fragment"
  name <- getFragmentName
  typeCondition <- getTypeCondition
  directives <- M.optional getDirectives
  selectionSet <- getSelectionSet
  pure FragmentDefinition
    { fragmentName = name
    , fragmentTypeCondition = typeCondition
    , fragmentDirectives = directives
    , fragmentSelectionSet = selectionSet
    }


getTypeSystemDefinition :: Parser TypeSystemDefinition
getTypeSystemDefinition = M.choice
  [ fmap TypeSystemDefinitionSchema getSchemaDefinition
  , fmap TypeSystemDefinitionType getTypeDefinition
  , fmap TypeSystemDefinitionTypeExtension getTypeExtensionDefinition
  , fmap TypeSystemDefinitionDirective getDirectiveDefinition
  ]


getSchemaDefinition :: Parser SchemaDefinition
getSchemaDefinition = do
  _ <- getSymbol "schema"
  directives <- M.optional getDirectives
  operationTypeDefinitions <- getOperationTypeDefintions
  pure SchemaDefinition
    { schemaDefinitionDirectives = directives
    , schemaDefinitionOperationTypes = operationTypeDefinitions
    }


getOperationTypeDefintions :: Parser OperationTypeDefinitions
getOperationTypeDefintions = getInBraces (do
  value <- M.many getOperationTypeDefintion
  pure OperationTypeDefinitions
    { operationTypeDefinitionsValue = value
    })


getOperationTypeDefintion :: Parser OperationTypeDefinition
getOperationTypeDefintion = do
  operation <- getOperationType
  _ <- getColon
  type_ <- getNamedType
  pure OperationTypeDefinition
    { operationTypeDefinitionOperation = operation
    , operationTypeDefinitionType = type_
    }


getTypeDefinition :: Parser TypeDefinition
getTypeDefinition = M.choice
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
  name <- getName
  directives <- M.optional getDirectives
  pure ScalarTypeDefinition
    { scalarTypeDefinitionName = name
    , scalarTypeDefinitionDirectives = directives
    }


getObjectTypeDefinition :: Parser ObjectTypeDefinition
getObjectTypeDefinition = do
  _ <- getSymbol "type"
  name <- getName
  interfaces <- M.optional getInterfaces
  directives <- M.optional getDirectives
  fields <- getFieldDefinitions
  pure ObjectTypeDefinition
    { objectTypeDefinitionName = name
    , objectTypeDefinitionInterfaces = interfaces
    , objectTypeDefinitionDirectives = directives
    , objectTypeDefinitionFields = fields
    }


getInterfaces :: Parser Interfaces
getInterfaces = do
  _ <- getSymbol "implements"
  list <- M.some getNamedType
  case NonEmpty.nonEmpty list of
    Nothing -> fail "impossible"
    Just value -> pure Interfaces
      { interfacesValue = value
      }


getFieldDefinitions :: Parser FieldDefinitions
getFieldDefinitions = getInBraces (do
  value <- M.many getFieldDefinition
  pure FieldDefinitions
    { fieldDefinitionsValue = value
    })


getFieldDefinition :: Parser FieldDefinition
getFieldDefinition = do
  name <- getName
  arguments <- M.optional getInputValueDefinitions
  _ <- getColon
  type_ <- getType
  directives <- M.optional getDirectives
  pure FieldDefinition
    { fieldDefinitionName = name
    , fieldDefinitionArguments = arguments
    , fieldDefinitionType = type_
    , fieldDefinitionDirectives = directives
    }


getInputValueDefinitions :: Parser InputValueDefinitions
getInputValueDefinitions = getInParentheses (do
  value <- M.many getInputValueDefinition
  pure InputValueDefinitions
    { inputValueDefinitionsValue = value
    })


getInputValueDefinition :: Parser InputValueDefinition
getInputValueDefinition = do
  name <- getName
  _ <- getColon
  type_ <- getType
  defaultValue <- M.optional getDefaultValue
  directives <- M.optional getDirectives
  pure InputValueDefinition
    { inputValueDefinitionName = name
    , inputValueDefinitionType = type_
    , inputValueDefinitionDefaultValue = defaultValue
    , inputValueDefinitionDirectives = directives
    }


getInterfaceTypeDefinition :: Parser InterfaceTypeDefinition
getInterfaceTypeDefinition = do
  _ <- getSymbol "interface"
  name <- getName
  directives <- M.optional getDirectives
  fieldDefinitions <- getFieldDefinitions
  pure InterfaceTypeDefinition
    { interfaceTypeDefinitionName = name
    , interfaceTypeDefinitionDirectives = directives
    , interfaceTypeDefinitionFields = fieldDefinitions
    }


getUnionTypeDefinition :: Parser UnionTypeDefinition
getUnionTypeDefinition = do
  _ <- getSymbol "union"
  name <- getName
  directives <- M.optional getDirectives
  _ <- getSymbol "="
  types <- getUnionTypes
  pure UnionTypeDefinition
    { unionTypeDefinitionName = name
    , unionTypeDefinitionDirectives = directives
    , unionTypeDefinitionTypes = types
    }


getUnionTypes :: Parser UnionTypes
getUnionTypes = do
  list <- M.sepBy1 getNamedType (getSymbol "|")
  case NonEmpty.nonEmpty list of
    Nothing -> fail "impossible"
    Just value -> pure UnionTypes
      { unionTypesValue = value
      }


getEnumTypeDefinition :: Parser EnumTypeDefinition
getEnumTypeDefinition = do
  _ <- getSymbol "enum"
  name <- getName
  directives <- M.optional getDirectives
  values <- getEnumValues
  pure EnumTypeDefinition
    { enumTypeDefinitionName = name
    , enumTypeDefinitionDirectives = directives
    , enumTypeDefinitionValues = values
    }


getEnumValues :: Parser EnumValues
getEnumValues = getInBraces (do
  value <- M.many getEnumValueDefinition
  pure EnumValues
    { enumValuesValue = value
    })


getEnumValueDefinition :: Parser EnumValueDefinition
getEnumValueDefinition = do
  name <- getName
  directives <- M.optional getDirectives
  pure EnumValueDefinition
    { enumValueDefinitionName = name
    , enumValueDefinitionDirectives = directives
    }


getInputObjectTypeDefinition :: Parser InputObjectTypeDefinition
getInputObjectTypeDefinition = do
  _ <- getSymbol "input"
  name <- getName
  directives <- M.optional getDirectives
  fields <- getInputFieldDefinitions
  pure InputObjectTypeDefinition
    { inputObjectTypeDefinitionName = name
    , inputObjectTypeDefinitionDirectives = directives
    , inputObjectTypeDefinitionFields = fields
    }


getInputFieldDefinitions :: Parser InputFieldDefinitions
getInputFieldDefinitions = getInBraces (do
  value <- M.many getInputValueDefinition
  pure InputFieldDefinitions
    { inputFieldDefinitionsValue = value
    })


getTypeExtensionDefinition :: Parser TypeExtensionDefinition
getTypeExtensionDefinition = do
  _ <- getSymbol "extend"
  value <- getObjectTypeDefinition
  pure TypeExtensionDefinition
    { typeExtensionDefinitionValue = value
    }


getDirectiveDefinition :: Parser DirectiveDefinition
getDirectiveDefinition = do
  _ <- getSymbol "directive"
  _ <- getSymbol "@"
  name <- getName
  arguments <- M.optional getInputValueDefinitions
  _ <- getSymbol "on"
  locations <- getDirectiveLocations
  pure DirectiveDefinition
    { directiveDefinitionName = name
    , directiveDefinitionArguments = arguments
    , directiveDefinitionLocations = locations
    }


getDirectiveLocations :: Parser DirectiveLocations
getDirectiveLocations = do
  list <- M.sepBy1 getName (getSymbol "|")
  case NonEmpty.nonEmpty list of
    Nothing -> fail "impossible"
    Just value -> pure DirectiveLocations
      { directiveLocationsValue = value
      }
