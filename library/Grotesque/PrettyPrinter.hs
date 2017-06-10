module Grotesque.PrettyPrinter where

import Grotesque.Language

import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc)

import qualified Data.Bits as Bits
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Builder.Scientific as Scientific
import qualified Data.Text.Prettyprint.Doc as P
import qualified Text.Printf as Printf


prettyDocument :: Document -> Doc ()
prettyDocument x = P.vsep (map prettyDefinition (documentValue x))


prettyDefinition :: Definition -> Doc ()
prettyDefinition x = case x of
  DefinitionOperation y -> prettyOperationDefinition y
  DefinitionFragment y -> prettyFragmentDefinition y
  DefinitionTypeSystem y -> prettyTypeSystemDefinition y


prettyOperationDefinition :: OperationDefinition -> Doc ()
prettyOperationDefinition x = P.sep (Maybe.catMaybes
  [ Just (prettyOperationType (operationDefinitionOperationType x))
  , fmap prettyName (operationDefinitionName x)
  , fmap prettyVariableDefinitions (operationDefinitionVariableDefinitions x)
  , fmap prettyDirectives (operationDefinitionDirectives x)
  , Just (prettySelectionSet (operationDefinitionSelectionSet x))
  ])


prettyName :: Name -> Doc ()
prettyName x = P.pretty (nameValue x)


prettyVariableDefinitions :: VariableDefinitions -> Doc ()
prettyVariableDefinitions x =
  P.parens (P.sep (map prettyVariableDefinition (variableDefinitionsValue x)))


prettyVariableDefinition :: VariableDefinition -> Doc ()
prettyVariableDefinition x = P.sep (Maybe.catMaybes
  [ Just (P.hcat [prettyVariable (variableDefinitionVariable x), P.pretty ":"])
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
prettyListType x = P.brackets (prettyType (listTypeValue x))


prettyNonNullType :: NonNullType -> Doc ()
prettyNonNullType x = case x of
  NonNullTypeNamed y -> P.hcat [prettyNamedType y, P.pretty "!"]
  NonNullTypeList y -> P.hcat [prettyListType y, P.pretty "!"]


prettyDefaultValue :: DefaultValue -> Doc ()
prettyDefaultValue x = P.sep [P.pretty "=", prettyValue (defaultValueValue x)]


prettyDirectives :: Directives -> Doc ()
prettyDirectives x = P.sep (map prettyDirective (NonEmpty.toList (directivesValue x)))


prettyDirective :: Directive -> Doc ()
prettyDirective x = P.sep (Maybe.catMaybes
  [ Just (P.hcat [P.pretty "@", prettyName (directiveName x)])
  , fmap prettyArguments (directiveArguments x)
  ])


prettyOperationType :: OperationType -> Doc ()
prettyOperationType operationType = case operationType of
  OperationTypeQuery -> P.pretty "query"
  OperationTypeMutation -> P.pretty "mutation"
  OperationTypeSubscription -> P.pretty "subscription"


prettySelectionSet :: SelectionSet -> Doc ()
prettySelectionSet x = P.braces (P.sep (map prettySelection (selectionSetValue x)))


prettySelection :: Selection -> Doc ()
prettySelection x = case x of
  SelectionField y -> prettyField y
  SelectionFragmentSpread y -> prettyFragmentSpread y
  SelectionInlineFragment y -> prettyInlineFragment y


prettyField :: Field -> Doc ()
prettyField x = P.sep (Maybe.catMaybes
  [ fmap prettyAlias (fieldAlias x)
  , Just (prettyName (fieldName x))
  , fmap prettyArguments (fieldArguments x)
  , fmap prettyDirectives (fieldDirectives x)
  , fmap prettySelectionSet (fieldSelectionSet x)
  ])


prettyAlias :: Alias -> Doc ()
prettyAlias x = P.hcat [prettyName (aliasValue x), P.pretty ":"]


prettyArguments :: Arguments -> Doc ()
prettyArguments x = P.parens (P.sep (map prettyArgument (argumentsValue x)))


prettyArgument :: Argument -> Doc ()
prettyArgument x = P.sep
  [ P.hcat [prettyName (argumentName x), P.pretty ":"]
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
prettyVariable x = P.hcat [P.pretty "$", prettyName (variableValue x)]


prettyInt :: Integer -> Doc ()
prettyInt = P.pretty


prettyFloat :: Scientific -> Doc ()
prettyFloat x = P.pretty (Builder.toLazyText (Scientific.scientificBuilder x))


prettyString :: Text -> Doc ()
prettyString x = let
  isAsciiPrintable c = c >= ' ' && c <= '~'
  isBmp c = c <= '\xffff'
  surrogatePair c = let
    n = fromEnum c - 0x010000
    h = 0x00d800 Bits..|. (Bits.shiftR n 10)
    l = 0x00dc00 Bits..|. (n Bits..&. 0x0003ff)
    in (h, l)
  charBuilder c = case c of
    '"' -> Builder.fromString "\\\""
    '\\' -> Builder.fromString "\\\\"
    _ -> if isAsciiPrintable c
      then Builder.fromString [c]
      else if isBmp c
        then Builder.fromString (Printf.printf "\\u%04x" (fromEnum c))
        else let
          (h, l) = surrogatePair c
          in Builder.fromString (Printf.printf "\\u%04x\\u%04x" h l)
  in P.dquotes (P.pretty (Builder.toLazyText
    (Text.foldl' (\b c -> mappend b (charBuilder c)) mempty x)))


prettyBoolean :: Bool -> Doc ()
prettyBoolean x = case x of
  False -> P.pretty "false"
  True -> P.pretty "true"


prettyNull :: Doc ()
prettyNull = P.pretty "null"


prettyEnum :: Name -> Doc ()
prettyEnum x = prettyName x


prettyList :: [Value] -> Doc ()
prettyList x = P.brackets (P.sep (map prettyValue x))


prettyObject :: [ObjectField] -> Doc ()
prettyObject x = P.braces (P.sep (map prettyObjectField x))


prettyObjectField :: ObjectField -> Doc ()
prettyObjectField x =
  P.sep
    [ P.hcat [prettyName (objectFieldName x), P.pretty ":"]
    , prettyValue (objectFieldValue x)
    ]


prettyFragmentSpread :: FragmentSpread -> Doc ()
prettyFragmentSpread x = P.sep (Maybe.catMaybes
  [ Just (P.pretty "...")
  , Just (prettyFragmentName (fragmentSpreadName x))
  , fmap prettyDirectives (fragmentSpreadDirectives x)
  ])


prettyFragmentName :: FragmentName -> Doc ()
prettyFragmentName x = prettyName (fragmentNameValue x)


prettyInlineFragment :: InlineFragment -> Doc ()
prettyInlineFragment x = P.sep (Maybe.catMaybes
  [ Just (P.pretty "...")
  , fmap prettyTypeCondition (inlineFragmentTypeCondition x)
  , fmap prettyDirectives (inlineFragmentDirectives x)
  , Just (prettySelectionSet (inlineFragmentSelectionSet x))
  ])


prettyTypeCondition :: TypeCondition -> Doc ()
prettyTypeCondition x = P.sep
  [ P.pretty "on"
  , prettyNamedType (typeConditionValue x)
  ]


prettyFragmentDefinition :: FragmentDefinition -> Doc ()
prettyFragmentDefinition x = P.sep (Maybe.catMaybes
  [ Just (P.pretty "fragment")
  , Just (prettyFragmentName (fragmentName x))
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
prettySchemaDefinition x = P.sep (Maybe.catMaybes
  [ Just (P.pretty "schema")
  , fmap prettyDirectives (schemaDefinitionDirectives x)
  , Just (prettyOperationTypeDefinitions (schemaDefinitionOperationTypes x))
  ])


prettyOperationTypeDefinitions :: OperationTypeDefinitions -> Doc ()
prettyOperationTypeDefinitions x =
  P.braces (P.sep (map prettyOperationTypeDefinition (operationTypeDefinitionsValue x)))


prettyOperationTypeDefinition :: OperationTypeDefinition -> Doc ()
prettyOperationTypeDefinition x = P.sep
  [ P.hcat [prettyOperationType (operationTypeDefinitionOperation x), P.pretty ":"]
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
prettyScalarTypeDefinition x = P.sep (Maybe.catMaybes
  [ Just (P.pretty "scalar")
  , Just (prettyName (scalarTypeDefinitionName x))
  , fmap prettyDirectives (scalarTypeDefinitionDirectives x)
  ])


prettyObjectTypeDefinition :: ObjectTypeDefinition -> Doc ()
prettyObjectTypeDefinition x = P.sep (Maybe.catMaybes
  [ Just (P.pretty "type")
  , Just (prettyName (objectTypeDefinitionName x))
  , fmap prettyInterfaces (objectTypeDefinitionInterfaces x)
  , fmap prettyDirectives (objectTypeDefinitionDirectives x)
  , Just (prettyFieldDefinitions (objectTypeDefinitionFields x))
  ])


prettyInterfaces :: Interfaces -> Doc ()
prettyInterfaces x =
  P.sep (P.pretty "implements" : map prettyNamedType (NonEmpty.toList (interfacesValue x)))


prettyFieldDefinitions :: FieldDefinitions -> Doc ()
prettyFieldDefinitions x =
  P.braces (P.sep (map prettyFieldDefinition (fieldDefinitionsValue x)))


prettyFieldDefinition :: FieldDefinition -> Doc ()
prettyFieldDefinition x = P.sep (Maybe.catMaybes
  [ Just (P.hcat (Maybe.catMaybes
    [ Just (prettyName (fieldDefinitionName x))
    , fmap prettyInputValueDefinitions (fieldDefinitionArguments x)
    , Just (P.pretty ":")
    ]))
  , Just (prettyType (fieldDefinitionType x))
  , fmap prettyDirectives (fieldDefinitionDirectives x)
  ])


prettyInputValueDefinitions :: InputValueDefinitions -> Doc ()
prettyInputValueDefinitions x =
  P.parens (P.sep (map prettyInputValueDefinition (inputValueDefinitionsValue x)))


prettyInputValueDefinition :: InputValueDefinition -> Doc ()
prettyInputValueDefinition x = P.sep (Maybe.catMaybes
  [ Just (P.hcat
    [ prettyName (inputValueDefinitionName x)
    , P.pretty ":"
    ])
  , Just (prettyType (inputValueDefinitionType x))
  , fmap prettyDefaultValue (inputValueDefinitionDefaultValue x)
  , fmap prettyDirectives (inputValueDefinitionDirectives x)
  ])


prettyInterfaceTypeDefinition :: InterfaceTypeDefinition -> Doc ()
prettyInterfaceTypeDefinition x = P.sep (Maybe.catMaybes
  [ Just (P.pretty "interface")
  , Just (prettyName (interfaceTypeDefinitionName x))
  , fmap prettyDirectives (interfaceTypeDefinitionDirectives x)
  , Just (prettyFieldDefinitions (interfaceTypeDefinitionFields x))
  ])


prettyUnionTypeDefinition :: UnionTypeDefinition -> Doc ()
prettyUnionTypeDefinition x = P.sep (Maybe.catMaybes
  [ Just (P.pretty "union")
  , Just (prettyName (unionTypeDefinitionName x))
  , fmap prettyDirectives (unionTypeDefinitionDirectives x)
  , Just (P.pretty "=")
  , Just (prettyUnionTypes (unionTypeDefinitionTypes x))
  ])


prettyUnionTypes :: UnionTypes -> Doc ()
prettyUnionTypes x = P.encloseSep mempty mempty (mconcat [P.space, P.pretty "|", P.space])
  (map prettyNamedType (NonEmpty.toList (unionTypesValue x)))


prettyEnumTypeDefinition :: EnumTypeDefinition -> Doc ()
prettyEnumTypeDefinition x = P.sep (Maybe.catMaybes
  [ Just (P.pretty "enum")
  , Just (prettyName (enumTypeDefinitionName x))
  , fmap prettyDirectives (enumTypeDefinitionDirectives x)
  , Just (prettyEnumValues (enumTypeDefinitionValues x))
  ])


prettyEnumValues :: EnumValues -> Doc ()
prettyEnumValues x =
  P.braces (P.sep (map prettyEnumValueDefinition (enumValuesValue x)))


prettyEnumValueDefinition :: EnumValueDefinition -> Doc ()
prettyEnumValueDefinition x = P.sep (Maybe.catMaybes
  [ Just (prettyName (enumValueDefinitionName x))
  , fmap prettyDirectives (enumValueDefinitionDirectives x)
  ])


prettyInputObjectTypeDefinition :: InputObjectTypeDefinition -> Doc ()
prettyInputObjectTypeDefinition x = P.sep (Maybe.catMaybes
  [ Just (P.pretty "input")
  , Just (prettyName (inputObjectTypeDefinitionName x))
  , fmap prettyDirectives (inputObjectTypeDefinitionDirectives x)
  , Just (prettyInputFieldDefinitions (inputObjectTypeDefinitionFields x))
  ])


prettyInputFieldDefinitions :: InputFieldDefinitions -> Doc ()
prettyInputFieldDefinitions x =
  P.braces (P.sep (map prettyInputValueDefinition (inputFieldDefinitionsValue x)))


prettyTypeExtensionDefinition :: TypeExtensionDefinition -> Doc ()
prettyTypeExtensionDefinition x = P.sep
  [ P.pretty "extend"
  , prettyObjectTypeDefinition (typeExtensionDefinitionValue x)
  ]


prettyDirectiveDefinition :: DirectiveDefinition -> Doc ()
prettyDirectiveDefinition x = P.sep (Maybe.catMaybes
  [ Just (P.pretty "directive")
  , Just (P.hcat [P.pretty "@", prettyName (directiveDefinitionName x)])
  , fmap prettyInputValueDefinitions (directiveDefinitionArguments x)
  , Just (P.pretty "on")
  , Just (prettyDirectiveLocations (directiveDefinitionLocations x))
  ])


prettyDirectiveLocations :: DirectiveLocations -> Doc ()
prettyDirectiveLocations x = P.encloseSep mempty mempty (mconcat [P.space, P.pretty "|", P.space])
  (map prettyName (NonEmpty.toList (directiveLocationsValue x)))
