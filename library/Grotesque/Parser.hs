module Grotesque.Parser where

import Data.Scientific (Scientific)
import Data.Text (Text)
import Grotesque.Language
import Text.Megaparsec
import Text.Megaparsec.Text (Parser)
import Text.Read (readMaybe)

import qualified Data.Text as Text
import qualified Text.Megaparsec.Lexer as Lexer


getDocument :: Parser Document
getDocument = do
  _ <- getSpace
  value <- many getDefinition
  pure Document
    { documentValue = value
    }


getDefinition :: Parser Definition
getDefinition = choice
  [ fmap DefinitionOperation getOperationDefinition
  , fmap DefinitionFragment getFragmentDefinition
  , fmap DefinitionTypeSystem getTypeSystemDefinition
  ]


getOperationDefinition :: Parser OperationDefinition
getOperationDefinition = choice
  [ getLongOperationDefinition
  , getShortOperationDefinition
  ]


getLongOperationDefinition :: Parser OperationDefinition
getLongOperationDefinition = do
  operationType <- getOperationType
  name <- optional (try getName)
  variableDefinitions <- optional (try getVariableDefinitions)
  directives <- optional (try getDirectives)
  selectionSet <- getSelectionSet
  pure OperationDefinition
    { operationDefinitionOperationType = operationType
    , operationDefinitionName = name
    , operationDefinitionVariableDefinitions = variableDefinitions
    , operationDefinitionDirectives = directives
    , operationDefinitionSelectionSet = selectionSet
    }


getOperationType :: Parser OperationType
getOperationType = choice
  [ fmap (const OperationTypeQuery) (getSymbol "query")
  , fmap (const OperationTypeMutation) (getSymbol "mutation")
  , fmap (const OperationTypeSubscription) (getSymbol "subscription")
  ]


getVariableDefinitions :: Parser VariableDefinitions
getVariableDefinitions = getInParentheses (do
  value <- many getVariableDefinition
  pure VariableDefinitions
    { variableDefinitionsValue = value
    })


getInParentheses :: Parser a -> Parser a
getInParentheses = between (getSymbol "(") (getSymbol ")")


getVariableDefinition :: Parser VariableDefinition
getVariableDefinition = do
  variable <- getVariable
  _ <- getColon
  type_ <- getType
  defaultValue <- optional getDefaultValue
  pure VariableDefinition
    { variableDefinitionVariable = variable
    , variableDefinitionType = type_
    , variableDefinitionDefaultValue = defaultValue
    }


getType :: Parser Type
getType = choice
  [ fmap TypeNonNull (try getNonNullType)
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
getInBrackets = between (getSymbol "[") (getSymbol "]")


getNonNullType :: Parser NonNullType
getNonNullType = getLexeme (choice
  [ getNonNullNamedType
  , getNonNullListType
  ])


getNonNullNamedType :: Parser NonNullType
getNonNullNamedType = do
  value <- getNamedType
  _ <- getExclamationPoint
  pure (NonNullTypeNamed value)


getExclamationPoint :: Parser Char
getExclamationPoint = char '!'


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
  value <- some getDirective
  pure Directives
    { directivesValue = value
    }


getDirective :: Parser Directive
getDirective = do
  _ <- getSymbol "@"
  name <- getName
  arguments <- optional getArguments
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
  value <- many getSelection
  pure SelectionSet
    { selectionSetValue = value
    })


getInBraces :: Parser a -> Parser a
getInBraces = between (getSymbol "{") (getSymbol "}")


getSelection :: Parser Selection
getSelection = choice
  [ fmap SelectionField getField
  , fmap SelectionFragmentSpread (try getFragmentSpread)
  , fmap SelectionInlineFragment getInlineFragment
  ]


getField :: Parser Field
getField = do
  alias <- optional (try getAlias)
  name <- getName
  arguments <- optional (try getArguments)
  directives <- optional (try getDirectives)
  selectionSet <- optional (try getSelectionSet)
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
  first <- oneOf (concat [underscore, uppers, lowers])
  rest <- many (oneOf (concat [underscore, digits, uppers, lowers]))
  pure Name
    { nameValue = Text.pack (first : rest)
    })


getArguments :: Parser Arguments
getArguments = getInParentheses (do
  value <- many getArgument
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
getValue = choice
  [ fmap ValueVariable getVariable
  , fmap ValueFloat (try getFloat)
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
  case readMaybe integerPart of
    Nothing -> fail "impossible"
    Just int -> pure int)


getIntegerPart :: Parser String
getIntegerPart = choice
  [ try getZero
  , getNonZero
  ]


getZero :: Parser String
getZero = do
  maybeNegativeSign <- optional getNegativeSign
  zero <- char '0'
  case maybeNegativeSign of
    Nothing -> pure ([zero])
    Just negativeSign -> pure (negativeSign : [zero])


getNegativeSign :: Parser Char
getNegativeSign = char '-'


getNonZero :: Parser String
getNonZero = do
  maybeNegativeSign <- optional getNegativeSign
  first <- getNonZeroDigit
  rest <- many digitChar
  case maybeNegativeSign of
    Nothing -> pure (first : rest)
    Just negativeSign -> pure (negativeSign : first : rest)


getNonZeroDigit :: Parser Char
getNonZeroDigit = oneOf ['1' .. '9']


getFloat :: Parser Scientific
getFloat = getLexeme (choice
  [ try getFractionalExponentFloat
  , try getFractionalFloat
  , getExponentFloat
  ])


getFractionalFloat :: Parser Scientific
getFractionalFloat = do
  integerPart <- getIntegerPart
  fractionalPart <- getFractionalPart
  case readMaybe (integerPart ++ fractionalPart) of
    Nothing -> fail "impossible"
    Just float -> pure float


getFractionalPart :: Parser String
getFractionalPart = do
  decimalPoint <- getDecimalPoint
  digits <- some digitChar
  pure (decimalPoint : digits)


getDecimalPoint :: Parser Char
getDecimalPoint = char '.'


getExponentFloat :: Parser Scientific
getExponentFloat = do
  integerPart <- getIntegerPart
  exponentPart <- getExponentPart
  case readMaybe (integerPart ++ exponentPart) of
    Nothing -> fail "impossible"
    Just float -> pure float


getExponentPart :: Parser String
getExponentPart = do
  exponentIndicator <- getExponentIndicator
  maybeSign <- optional getSign
  digits <- some digitChar
  case maybeSign of
    Nothing -> pure (exponentIndicator : digits)
    Just sign -> pure (exponentIndicator : sign : digits)


getExponentIndicator :: Parser Char
getExponentIndicator = char' 'e'


getSign :: Parser Char
getSign = oneOf ['+', '-']


getFractionalExponentFloat :: Parser Scientific
getFractionalExponentFloat = do
  integerPart <- getIntegerPart
  fractionalPart <- getFractionalPart
  exponentPart <- getExponentPart
  case readMaybe (integerPart ++ fractionalPart ++ exponentPart) of
    Nothing -> fail "impossible"
    Just float -> pure float


getString :: Parser Text
getString = getLexeme (do
  _ <- getQuote
  characters <- many getCharacter
  _ <- getQuote
  pure (Text.pack characters))


getQuote :: Parser Char
getQuote = char '"'


getCharacter :: Parser Char
getCharacter = choice
  [ getStringCharacter
  , try getUnicodeCharacter
  , getEscapedCharacter
  ]


getStringCharacter :: Parser Char
getStringCharacter = oneOf (concat
  [ ['\x0009']
  , ['\x0020' .. '\x0021']
  , ['\x0023' .. '\x005b']
  , ['\x005d' .. '\xffff']
  ])


getUnicodeCharacter :: Parser Char
getUnicodeCharacter = do
  _ <- getBackslash
  _ <- char 'u'
  digits <- count 4 hexDigitChar
  case readMaybe ("\'\\x" ++ digits ++ "\'") of
    Nothing -> fail "impossible"
    Just character -> pure character


getEscapedCharacter :: Parser Char
getEscapedCharacter = do
  _ <- getBackslash
  escape <- oneOf ['"', '\\', '/', 'b', 'f', 'n', 'r', 't']
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
getBackslash = char '\\'


getBoolean :: Parser Bool
getBoolean = choice
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
getList = getInBrackets (many getValue)


getObject :: Parser [ObjectField]
getObject = getInBraces (many getObjectField)


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
  directives <- optional getDirectives
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
  typeCondition <- optional getTypeCondition
  directives <- optional getDirectives
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
    _ <- oneOf ['\xfeff', '\x0009', '\x0020', '\x000a', '\x000d', ',']
    pure ())
  (Lexer.skipLineComment "#")
  (fail "no block comments")


getFragmentDefinition :: Parser FragmentDefinition
getFragmentDefinition = do
  _ <- getSymbol "fragment"
  name <- getFragmentName
  typeCondition <- getTypeCondition
  directives <- optional getDirectives
  selectionSet <- getSelectionSet
  pure FragmentDefinition
    { fragmentName = name
    , fragmentTypeCondition = typeCondition
    , fragmentDirectives = directives
    , fragmentSelectionSet = selectionSet
    }


getTypeSystemDefinition :: Parser TypeSystemDefinition
getTypeSystemDefinition = fail "" -- TODO
