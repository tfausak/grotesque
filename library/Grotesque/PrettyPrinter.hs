module Grotesque.PrettyPrinter where

import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text.Lazy.Builder.Scientific (scientificBuilder)
import Data.Text.Prettyprint.Doc hiding (prettyList)
import Grotesque.Language
import Text.Printf (printf)

import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as Builder


prettyDocument :: Document -> Doc ()
prettyDocument x = vsep (map prettyDefinition (documentValue x))


prettyDefinition :: Definition -> Doc ()
prettyDefinition x = case x of
  DefinitionOperation y -> prettyOperationDefinition y
  DefinitionFragment y -> prettyFragmentDefinition y


prettyOperationDefinition :: OperationDefinition -> Doc ()
prettyOperationDefinition x = sep
  [ prettyOperationType (operationDefinitionOperationType x)
  , pretty (fmap prettyName (operationDefinitionName x))
  , pretty (fmap prettyVariableDefinitions (operationDefinitionVariableDefinitions x))
  , pretty (fmap prettyDirectives (operationDefinitionDirectives x))
  , prettySelectionSet (operationDefinitionSelectionSet x)
  ]


prettyName :: Name -> Doc ()
prettyName x = pretty (nameValue x)


prettyVariableDefinitions :: VariableDefinitions -> Doc ()
prettyVariableDefinitions x =
  parens (sep (map prettyVariableDefinition (variableDefinitionsValue x)))


prettyVariableDefinition :: VariableDefinition -> Doc ()
prettyVariableDefinition x = sep
  [ hcat [prettyVariable (variableDefinitionVariable x), pretty ":"]
  , prettyType (variableDefinitionType x)
  , pretty (fmap prettyDefaultValue (variableDefinitionDefaultValue x))
  ]


prettyType :: Type -> Doc ()
prettyType x = case x of
  TypeNamed y -> prettyNamedType y
  TypeList y -> prettyListType y
  TypeNonNull y -> prettyNonNullType y


prettyNamedType :: NamedType -> Doc ()
prettyNamedType x = prettyName (namedTypeValue x)


prettyListType :: ListType -> Doc ()
prettyListType x = brackets (prettyType (listTypeValue x))


prettyNonNullType :: NonNullType -> Doc ()
prettyNonNullType x = case x of
  NonNullTypeNamed y -> hcat [prettyNamedType y, pretty "!"]
  NonNullTypeList y -> hcat [prettyListType y, pretty "!"]


prettyDefaultValue :: DefaultValue -> Doc ()
prettyDefaultValue x = sep [pretty "=", prettyValue (defaultValueValue x)]


prettyDirectives :: Directives -> Doc ()
prettyDirectives x = sep (map prettyDirective (directivesValue x))


prettyDirective :: Directive -> Doc ()
prettyDirective x = sep
  [ hcat [pretty "@", prettyName (directiveName x)]
  , pretty (fmap prettyArguments (directiveArguments x))
  ]


prettyOperationType :: OperationType -> Doc ()
prettyOperationType operationType = case operationType of
  OperationTypeQuery -> pretty "query"
  OperationTypeMutation -> pretty "mutation"
  OperationTypeSubscription -> pretty "subscription"


prettySelectionSet :: SelectionSet -> Doc ()
prettySelectionSet x = braces (sep (map prettySelection (selectionSetValue x)))


prettySelection :: Selection -> Doc ()
prettySelection x = case x of
  SelectionField y -> prettyField y
  SelectionFragmentSpread y -> prettyFragmentSpread y
  SelectionInlineFragment y -> prettyInlineFragment y


prettyField :: Field -> Doc ()
prettyField x = sep
  [ pretty (fmap prettyAlias (fieldAlias x))
  , prettyName (fieldName x)
  , pretty (fmap prettyArguments (fieldArguments x))
  , pretty (fmap prettyDirectives (fieldDirectives x))
  , pretty (fmap prettySelectionSet (fieldSelectionSet x))
  ]


prettyAlias :: Alias -> Doc ()
prettyAlias x = hcat [prettyName (aliasValue x), pretty ":"]


prettyArguments :: Arguments -> Doc ()
prettyArguments x = parens (sep (map prettyArgument (argumentsValue x)))


prettyArgument :: Argument -> Doc ()
prettyArgument x = sep
  [ hcat [prettyName (argumentName x), pretty ":"]
  , prettyValue (argumentValue x)
  ]


prettyValue :: Value -> Doc ()
prettyValue x = case x of
  ValueVariable y -> prettyVariable y
  ValueInt y -> prettyInt y
  ValueFloat y -> prettyFloat y
  ValueString y -> prettyString y
  ValueBoolean y -> prettyBoolean y
  ValueNull -> prettyNull
  ValueEnum y -> prettyEnum y
  ValueList y -> prettyList y
  ValueObject y -> prettyObject y


prettyVariable :: Variable -> Doc ()
prettyVariable x = hcat [pretty "$", prettyName (variableValue x)]


prettyInt :: Integer -> Doc ()
prettyInt = pretty


prettyFloat :: Scientific -> Doc ()
prettyFloat x = pretty (Builder.toLazyText (scientificBuilder x))


prettyString :: Text -> Doc ()
prettyString x = let
  charBuilder c = case c of
    '"' -> Builder.fromString "\\\""
    '\\' -> Builder.fromString "\\\\"
    _ -> if c < ' ' || c > '~'
      then Builder.fromString ('\\' : 'u' : printf "%04x" (fromEnum c))
      else Builder.fromString [c]
  in dquotes (pretty (Builder.toLazyText
    (Text.foldl' (\b c -> b <> charBuilder c) mempty x)))


prettyBoolean :: Bool -> Doc ()
prettyBoolean x = case x of
  False -> pretty "false"
  True -> pretty "true"


prettyNull :: Doc ()
prettyNull = pretty "null"


prettyEnum :: Name -> Doc ()
prettyEnum x = prettyName x


prettyList :: [Value] -> Doc ()
prettyList x = brackets (sep (map prettyValue x))


prettyObject :: [ObjectField] -> Doc ()
prettyObject x = braces (sep (map prettyObjectField x))


prettyObjectField :: ObjectField -> Doc ()
prettyObjectField x =
  sep
    [ hcat [prettyName (objectFieldName x), pretty ":"]
    , prettyValue (objectFieldValue x)
    ]


prettyFragmentSpread :: FragmentSpread -> Doc ()
prettyFragmentSpread x = sep
  [ prettyFragmentName (fragmentSpreadName x)
  , pretty (fmap prettyDirectives (fragmentSpreadDirectives x))
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
