module Grotesque.PrettyPrinter where

import Data.Text.Lazy.Builder.Scientific (scientificBuilder)
import Data.Text.Prettyprint.Doc
import Grotesque.Language
import Text.Printf (printf)

import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as Builder

prettyDocument :: Document -> Doc ()
prettyDocument document = vsep (map prettyDefinition (documentValue document))

prettyDefinition :: Definition -> Doc ()
prettyDefinition definition = case definition of
  DefinitionOperation operationDefinition -> prettyOperationDefinition operationDefinition
  DefinitionFragment fragmentDefinition -> prettyFragmentDefinition fragmentDefinition

prettyOperationDefinition :: OperationDefinition -> Doc ()
prettyOperationDefinition operationDefinition = sep
  [ prettyOperationType (operationDefinitionOperationType operationDefinition)
  , pretty (fmap prettyName (operationDefinitionName operationDefinition))
  , pretty (fmap prettyVariableDefinitions (operationDefinitionVariableDefinitions operationDefinition))
  , pretty (fmap prettyDirectives (operationDefinitionDirectives operationDefinition))
  , prettySelectionSet (operationDefinitionSelectionSet operationDefinition)
  ]

prettyName :: Name -> Doc ()
prettyName name = pretty (nameValue name)

prettyVariableDefinitions :: VariableDefinitions -> Doc ()
prettyVariableDefinitions variableDefinitions = parens (sep (map prettyVariableDefinition (variableDefinitionsValue variableDefinitions)))

prettyVariableDefinition :: VariableDefinition -> Doc ()
prettyVariableDefinition variableDefinition = sep
  [ hcat [prettyVariable (variableDefinitionVariable variableDefinition), pretty ":"]
  , prettyType (variableDefinitionType variableDefinition)
  , pretty (fmap prettyDefaultValue (variableDefinitionDefaultValue variableDefinition))
  ]

prettyType :: Type -> Doc ()
prettyType type_ = case type_ of
  TypeNamed namedType -> prettyNamedType namedType
  TypeList listType -> prettyListType listType
  TypeNonNull nonNullType -> prettyNonNullType nonNullType

prettyNamedType :: NamedType -> Doc ()
prettyNamedType namedType = prettyName (namedTypeValue namedType)

prettyListType :: ListType -> Doc ()
prettyListType listType = brackets (prettyType (listTypeValue listType))

prettyNonNullType :: NonNullType -> Doc ()
prettyNonNullType nonNullType = case nonNullType of
  NonNullTypeNamed namedType -> hcat [prettyNamedType namedType, pretty "!"]
  NonNullTypeList listType -> hcat [prettyListType listType, pretty "!"]

prettyDefaultValue :: DefaultValue -> Doc ()
prettyDefaultValue defaultValue = sep
  [ pretty "="
  , prettyValue (defaultValueValue defaultValue)
  ]

prettyDirectives :: Directives -> Doc ()
prettyDirectives directives = sep (map prettyDirective (directivesValue directives))

prettyDirective :: Directive -> Doc ()
prettyDirective directive = sep
  [ hcat [pretty "@", prettyName (directiveName directive)]
  , pretty (fmap prettyArguments (directiveArguments directive))
  ]

prettyOperationType :: OperationType -> Doc ()
prettyOperationType operationType = case operationType of
  OperationTypeQuery -> pretty "query"
  OperationTypeMutation -> pretty "mutation"
  OperationTypeSubscription -> pretty "subscription"

prettySelectionSet :: SelectionSet -> Doc ()
prettySelectionSet selectionSet = braces (sep (map prettySelection (selectionSetValue selectionSet)))

prettySelection :: Selection -> Doc ()
prettySelection selection = case selection of
  SelectionField field -> prettyField field
  SelectionFragmentSpread fragmentSpread -> prettyFragmentSpread fragmentSpread
  SelectionInlineFragment inlineFragment -> prettyInlineFragment inlineFragment

prettyField :: Field -> Doc ()
prettyField field = sep
  [ pretty (fmap prettyAlias (fieldAlias field))
  , prettyName (fieldName field)
  , pretty (fmap prettyArguments (fieldArguments field))
  , pretty (fmap prettyDirectives (fieldDirectives field))
  , pretty (fmap prettySelectionSet (fieldSelectionSet field))
  ]

prettyAlias :: Alias -> Doc ()
prettyAlias alias = hcat [prettyName (aliasValue alias), pretty ":"]

prettyArguments :: Arguments -> Doc ()
prettyArguments arguments = parens (sep (map prettyArgument (argumentsValue arguments)))

prettyArgument :: Argument -> Doc ()
prettyArgument argument = sep
  [ hcat [prettyName (argumentName argument), pretty ":"]
  , prettyValue (argumentValue argument)
  ]

prettyValue :: Value -> Doc ()
prettyValue value = case value of
  ValueVariable x -> prettyVariable x
  ValueInt x -> pretty x
  ValueFloat x -> pretty (Builder.toLazyText (scientificBuilder x))
  ValueString x -> dquotes (pretty (Builder.toLazyText (Text.foldl'
    (\b c -> case c of
      '"' -> b <> Builder.fromString "\\\""
      '\\' -> b <> Builder.fromString "\\\\"
      _ -> if c < ' ' || c > '~'
        then b <> Builder.fromString ('\\' : 'u' : printf "%04x" (fromEnum c))
        else b <> Builder.fromString [c])
    mempty
    x)))
  ValueBoolean x -> case x of
    False -> pretty "false"
    True -> pretty "true"
  ValueNull -> pretty "null"
  ValueEnum x -> prettyName x
  ValueList x -> brackets (sep (map prettyValue x))
  ValueObject x -> braces (sep (map prettyObjectField x))

prettyVariable :: Variable -> Doc ()
prettyVariable variable = hcat [pretty "$", prettyName (variableValue variable)]

prettyObjectField :: ObjectField -> Doc ()
prettyObjectField objectField =
  sep
    [ hcat [prettyName (objectFieldName objectField), pretty ":"]
    , prettyValue (objectFieldValue objectField)
    ]

prettyFragmentSpread :: FragmentSpread -> Doc ()
prettyFragmentSpread fragmentSpread = sep
  [ prettyFragmentName (fragmentSpreadName fragmentSpread)
  , pretty (fmap prettyDirectives (fragmentSpreadDirectives fragmentSpread))
  ]

prettyFragmentName :: FragmentName -> Doc ()
prettyFragmentName x = sep [pretty "...", prettyName (fragmentNameValue x)]

prettyInlineFragment :: InlineFragment -> Doc ()
prettyInlineFragment x = sep
  [ pretty "..."
  , pretty (fmap prettyTypeCondition (inlineFragmentTypeCondition x))
  , pretty (fmap prettyDirectives (inlineFragmentDirectives x))
  , prettySelectionSet (inlineFragmentSelectionSet x)
  ]

prettyTypeCondition :: TypeCondition -> Doc ()
prettyTypeCondition x = sep
  [ pretty "on"
  , prettyNamedType (typeConditionValue x)
  ]

prettyFragmentDefinition :: FragmentDefinition -> Doc ()
prettyFragmentDefinition x = sep
  [ prettyFragmentName (fragmentName x)
  , prettyTypeCondition (fragmentTypeCondition x)
  , pretty (fmap prettyDirectives (fragmentDirectives x))
  , prettySelectionSet (fragmentSelectionSet x)
  ]
