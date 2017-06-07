module Grotesque.PrettyPrinter where

import Data.Maybe (catMaybes)
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
  DefinitionTypeSystem y -> prettyTypeSystemDefinition y


prettyOperationDefinition :: OperationDefinition -> Doc ()
prettyOperationDefinition x = sep (catMaybes
  [ Just (prettyOperationType (operationDefinitionOperationType x))
  , fmap prettyName (operationDefinitionName x)
  , fmap prettyVariableDefinitions (operationDefinitionVariableDefinitions x)
  , fmap prettyDirectives (operationDefinitionDirectives x)
  , Just (prettySelectionSet (operationDefinitionSelectionSet x))
  ])


prettyName :: Name -> Doc ()
prettyName x = pretty (nameValue x)


prettyVariableDefinitions :: VariableDefinitions -> Doc ()
prettyVariableDefinitions x =
  parens (sep (map prettyVariableDefinition (variableDefinitionsValue x)))


prettyVariableDefinition :: VariableDefinition -> Doc ()
prettyVariableDefinition x = sep (catMaybes
  [ Just (hcat [prettyVariable (variableDefinitionVariable x), pretty ":"])
  , Just (prettyType (variableDefinitionType x))
  , fmap prettyDefaultValue (variableDefinitionDefaultValue x)
  ])


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
prettyDirective x = sep (catMaybes
  [ Just (hcat [pretty "@", prettyName (directiveName x)])
  , fmap prettyArguments (directiveArguments x)
  ])


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
prettyField x = sep (catMaybes
  [ fmap prettyAlias (fieldAlias x)
  , Just (prettyName (fieldName x))
  , fmap prettyArguments (fieldArguments x)
  , fmap prettyDirectives (fieldDirectives x)
  , fmap prettySelectionSet (fieldSelectionSet x)
  ])


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
prettyFragmentSpread x = sep (catMaybes
  [ Just (prettyFragmentName (fragmentSpreadName x))
  , fmap prettyDirectives (fragmentSpreadDirectives x)
  ])


prettyFragmentName :: FragmentName -> Doc ()
prettyFragmentName x = sep [pretty "...", prettyName (fragmentNameValue x)]


prettyInlineFragment :: InlineFragment -> Doc ()
prettyInlineFragment x = sep (catMaybes
  [ Just (pretty "...")
  , fmap prettyTypeCondition (inlineFragmentTypeCondition x)
  , fmap prettyDirectives (inlineFragmentDirectives x)
  , Just (prettySelectionSet (inlineFragmentSelectionSet x))
  ])


prettyTypeCondition :: TypeCondition -> Doc ()
prettyTypeCondition x = sep
  [ pretty "on"
  , prettyNamedType (typeConditionValue x)
  ]


prettyFragmentDefinition :: FragmentDefinition -> Doc ()
prettyFragmentDefinition x = sep (catMaybes
  [ Just (prettyFragmentName (fragmentName x))
  , Just (prettyTypeCondition (fragmentTypeCondition x))
  , fmap prettyDirectives (fragmentDirectives x)
  , Just (prettySelectionSet (fragmentSelectionSet x))
  ])


prettyTypeSystemDefinition :: TypeSystemDefinition -> Doc ()
prettyTypeSystemDefinition x = case x of
  TypeSystemDefinitionSchema y -> prettySchemaDefinition y
  TypeSystemDefinitionType y -> prettyTypeDefinition y
  TypeSystemDefinitionTypeExtension y -> prettyTypeExtensionDefinition y
  TypeSystemDefinitionDirective y -> prettyDirectiveDefinition y


prettySchemaDefinition :: SchemaDefinition -> Doc ()
prettySchemaDefinition x = sep (catMaybes
  [ Just (pretty "schema")
  , fmap prettyDirectives (schemaDefinitionDirectives x)
  , Just (prettyOperationTypeDefinitions (schemaDefinitionOperationTypes x))
  ])


prettyOperationTypeDefinitions :: OperationTypeDefinitions -> Doc ()
prettyOperationTypeDefinitions x =
  braces (sep (map prettyOperationTypeDefinition (operationTypeDefinitionsValue x)))


prettyOperationTypeDefinition :: OperationTypeDefinition -> Doc ()
prettyOperationTypeDefinition x = sep
  [ hcat [prettyOperationType (operationTypeDefinitionOperation x), pretty ":"]
  , prettyNamedType (operationTypeDefinitionType x)
  ]


prettyTypeDefinition :: TypeDefinition -> Doc ()
prettyTypeDefinition x = case x of
  TypeDefinitionScalar y -> prettyScalarTypeDefinition y
  TypeDefinitionObject y -> prettyObjectTypeDefinition y
  TypeDefinitionInterface y -> prettyInterfaceTypeDefinition y
  TypeDefinitionUnion y -> prettyUnionTypeDefinition y
  TypeDefinitionEnum y -> prettyEnumTypeDefinition y
  TypeDefinitionInputObject y -> prettyInputObjectTypeDefinition y


prettyScalarTypeDefinition :: ScalarTypeDefinition -> Doc ()
prettyScalarTypeDefinition x = sep (catMaybes
  [ Just (pretty "scalar")
  , Just (prettyName (scalarTypeDefinitionName x))
  , fmap prettyDirectives (scalarTypeDefinitionDirectives x)
  ])


prettyObjectTypeDefinition :: ObjectTypeDefinition -> Doc ()
prettyObjectTypeDefinition x = sep (catMaybes
  [ Just (pretty "type")
  , Just (prettyName (objectTypeDefinitionName x))
  , fmap prettyInterfaces (objectTypeDefinitionInterfaces x)
  , fmap prettyDirectives (objectTypeDefinitionDirectives x)
  , Just (prettyFieldDefinitions (objectTypeDefinitionFields x))
  ])


prettyInterfaces :: Interfaces -> Doc ()
prettyInterfaces x = case interfacesValue x of
  [] -> mempty
  y -> sep (pretty "implements" : map prettyNamedType y)


prettyFieldDefinitions :: FieldDefinitions -> Doc ()
prettyFieldDefinitions x =
  braces (sep (map prettyFieldDefinition (fieldDefinitionsValue x)))


prettyFieldDefinition :: FieldDefinition -> Doc ()
prettyFieldDefinition x = sep (catMaybes
  [ Just (hcat (catMaybes
    [ Just (prettyName (fieldDefinitionName x))
    , fmap prettyInputValueDefinitions (fieldDefinitionArguments x)
    , Just (pretty ":")
    ]))
  , Just (prettyType (fieldDefinitionType x))
  , fmap prettyDirectives (fieldDefinitionDirectives x)
  ])


prettyInputValueDefinitions :: InputValueDefinitions -> Doc ()
prettyInputValueDefinitions x =
  parens (sep (map prettyInputValueDefinition (inputValueDefinitionsValue x)))


prettyInputValueDefinition :: InputValueDefinition -> Doc ()
prettyInputValueDefinition x = sep (catMaybes
  [ Just (hcat
    [ prettyName (inputValueDefinitionName x)
    , pretty ":"
    ])
  , Just (prettyType (inputValueDefinitionType x))
  , fmap prettyDefaultValue (inputValueDefinitionDefaultValue x)
  , fmap prettyDirectives (inputValueDefinitionDirectives x)
  ])


prettyInterfaceTypeDefinition :: InterfaceTypeDefinition -> Doc ()
prettyInterfaceTypeDefinition _ = mempty -- TODO


prettyUnionTypeDefinition :: UnionTypeDefinition -> Doc ()
prettyUnionTypeDefinition _ = mempty -- TODO


prettyEnumTypeDefinition :: EnumTypeDefinition -> Doc ()
prettyEnumTypeDefinition _ = mempty -- TODO


prettyInputObjectTypeDefinition :: InputObjectTypeDefinition -> Doc ()
prettyInputObjectTypeDefinition _ = mempty -- TODO


prettyTypeExtensionDefinition :: TypeExtensionDefinition -> Doc ()
prettyTypeExtensionDefinition _ = mempty -- TODO


prettyDirectiveDefinition :: DirectiveDefinition -> Doc ()
prettyDirectiveDefinition _ = mempty -- TODO
