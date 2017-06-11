{-# LANGUAGE RecordWildCards #-}

module Grotesque.PrettyPrinter where

import Grotesque.Language

import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Prettyprint.Doc (Doc)

import qualified Data.Bits as Bits
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Builder.Scientific as Scientific
import qualified Data.Text.Prettyprint.Doc as P
import qualified Data.Text.Prettyprint.Doc.Render.Text as P
import qualified Text.Printf as Printf

prettyPrintDocument :: Document -> Text
prettyPrintDocument x =
  P.renderStrict
    (P.layoutPretty (P.LayoutOptions P.Unbounded) (prettyDocument x))

prettyDocument :: Document -> Doc a
prettyDocument Document {..} = P.vsep (map prettyDefinition documentValue)

prettyDefinition :: Definition -> Doc a
prettyDefinition x =
  case x of
    DefinitionOperation y -> prettyOperationDefinition y
    DefinitionFragment y -> prettyFragmentDefinition y
    DefinitionTypeSystem y -> prettyTypeSystemDefinition y

prettyOperationDefinition :: OperationDefinition -> Doc a
prettyOperationDefinition OperationDefinition {..} =
  sepMaybes
    [ Just (prettyOperationType operationDefinitionOperationType)
    , fmap prettyName operationDefinitionName
    , fmap prettyVariableDefinitions operationDefinitionVariableDefinitions
    , fmap prettyDirectives operationDefinitionDirectives
    , Just (prettySelectionSet operationDefinitionSelectionSet)
    ]

sepMaybes :: [Maybe (Doc a)] -> Doc a
sepMaybes x = P.sep (Maybe.catMaybes x)

prettyName :: Name -> Doc a
prettyName Name {..} = P.pretty nameValue

prettyVariableDefinitions :: VariableDefinitions -> Doc a
prettyVariableDefinitions VariableDefinitions {..} =
  P.parens (P.sep (map prettyVariableDefinition variableDefinitionsValue))

prettyVariableDefinition :: VariableDefinition -> Doc a
prettyVariableDefinition VariableDefinition {..} =
  sepMaybes
    [ Just (P.hcat [prettyVariable variableDefinitionVariable, P.pretty ":"])
    , Just (prettyType variableDefinitionType)
    , fmap prettyDefaultValue variableDefinitionDefaultValue
    ]

prettyType :: Type -> Doc a
prettyType x =
  case x of
    TypeNamed y -> prettyNamedType y
    TypeList y -> prettyListType y
    TypeNonNull y -> prettyNonNullType y

prettyNamedType :: NamedType -> Doc a
prettyNamedType NamedType {..} = prettyName namedTypeValue

prettyListType :: ListType -> Doc a
prettyListType ListType {..} = P.brackets (prettyType listTypeValue)

prettyNonNullType :: NonNullType -> Doc a
prettyNonNullType x =
  case x of
    NonNullTypeNamed y -> P.hcat [prettyNamedType y, P.pretty "!"]
    NonNullTypeList y -> P.hcat [prettyListType y, P.pretty "!"]

prettyDefaultValue :: DefaultValue -> Doc a
prettyDefaultValue DefaultValue {..} =
  P.sep [P.pretty "=", prettyValue defaultValueValue]

prettyDirectives :: Directives -> Doc a
prettyDirectives Directives {..} =
  P.sep (map prettyDirective (NonEmpty.toList directivesValue))

prettyDirective :: Directive -> Doc a
prettyDirective Directive {..} =
  sepMaybes
    [ Just (P.hcat [P.pretty "@", prettyName directiveName])
    , fmap prettyArguments directiveArguments
    ]

prettyOperationType :: OperationType -> Doc a
prettyOperationType operationType =
  case operationType of
    OperationTypeQuery -> P.pretty "query"
    OperationTypeMutation -> P.pretty "mutation"
    OperationTypeSubscription -> P.pretty "subscription"

prettySelectionSet :: SelectionSet -> Doc a
prettySelectionSet SelectionSet {..} =
  P.braces (P.sep (map prettySelection selectionSetValue))

prettySelection :: Selection -> Doc a
prettySelection x =
  case x of
    SelectionField y -> prettyField y
    SelectionFragmentSpread y -> prettyFragmentSpread y
    SelectionInlineFragment y -> prettyInlineFragment y

prettyField :: Field -> Doc a
prettyField Field {..} =
  sepMaybes
    [ fmap prettyAlias fieldAlias
    , Just (prettyName fieldName)
    , fmap prettyArguments fieldArguments
    , fmap prettyDirectives fieldDirectives
    , fmap prettySelectionSet fieldSelectionSet
    ]

prettyAlias :: Alias -> Doc a
prettyAlias Alias {..} = P.hcat [prettyName aliasValue, P.pretty ":"]

prettyArguments :: Arguments -> Doc a
prettyArguments Arguments {..} =
  P.parens (P.sep (map prettyArgument argumentsValue))

prettyArgument :: Argument -> Doc a
prettyArgument Argument {..} =
  P.sep
    [P.hcat [prettyName argumentName, P.pretty ":"], prettyValue argumentValue]

prettyValue :: Value -> Doc a
prettyValue x =
  case x of
    ValueVariable y -> prettyVariable y
    ValueInt y -> prettyInt y
    ValueFloat y -> prettyFloat y
    ValueString y -> prettyString y
    ValueBoolean y -> prettyBoolean y
    ValueNull -> prettyNull
    ValueEnum y -> prettyEnum y
    ValueList y -> prettyList y
    ValueObject y -> prettyObject y

prettyVariable :: Variable -> Doc a
prettyVariable Variable {..} = P.hcat [P.pretty "$", prettyName variableValue]

prettyInt :: Integer -> Doc a
prettyInt = P.pretty

prettyFloat :: Scientific -> Doc a
prettyFloat x = P.pretty (Builder.toLazyText (Scientific.scientificBuilder x))

prettyString :: Text -> Doc a
prettyString x =
  P.dquotes
    (P.pretty
       (Builder.toLazyText
          (Text.foldl' (\b c -> mappend b (charBuilder c)) mempty x)))

charBuilder :: Char -> Builder
charBuilder c =
  case c of
    '"' -> Builder.fromString "\\\""
    '\\' -> Builder.fromString "\\\\"
    _
      | asciiPrintable c -> Builder.fromString [c]
      | bmp c -> Builder.fromString (Printf.printf "\\u%04x" (fromEnum c))
      | otherwise ->
        let (h, l) = surrogatePair c
        in Builder.fromString (Printf.printf "\\u%04x\\u%04x" h l)

asciiPrintable :: Char -> Bool
asciiPrintable c = ' ' <= c && c <= '~'

bmp :: Char -> Bool
bmp c = c <= '\xffff'

surrogatePair :: Char -> (Int, Int)
surrogatePair c =
  let n = fromEnum c - 0x010000
      h = 0x00d800 Bits..|. Bits.shiftR n 10
      l = 0x00dc00 Bits..|. (n Bits..&. 0x0003ff)
  in (h, l)

prettyBoolean :: Bool -> Doc a
prettyBoolean x =
  if x
    then P.pretty "true"
    else P.pretty "false"

prettyNull :: Doc a
prettyNull = P.pretty "null"

prettyEnum :: Name -> Doc a
prettyEnum = prettyName

prettyList :: [Value] -> Doc a
prettyList x = P.brackets (P.sep (map prettyValue x))

prettyObject :: [ObjectField] -> Doc a
prettyObject x = P.braces (P.sep (map prettyObjectField x))

prettyObjectField :: ObjectField -> Doc a
prettyObjectField ObjectField {..} =
  P.sep
    [ P.hcat [prettyName objectFieldName, P.pretty ":"]
    , prettyValue objectFieldValue
    ]

prettyFragmentSpread :: FragmentSpread -> Doc a
prettyFragmentSpread FragmentSpread {..} =
  sepMaybes
    [ Just (P.pretty "...")
    , Just (prettyFragmentName fragmentSpreadName)
    , fmap prettyDirectives fragmentSpreadDirectives
    ]

prettyFragmentName :: FragmentName -> Doc a
prettyFragmentName FragmentName {..} = prettyName fragmentNameValue

prettyInlineFragment :: InlineFragment -> Doc a
prettyInlineFragment InlineFragment {..} =
  sepMaybes
    [ Just (P.pretty "...")
    , fmap prettyTypeCondition inlineFragmentTypeCondition
    , fmap prettyDirectives inlineFragmentDirectives
    , Just (prettySelectionSet inlineFragmentSelectionSet)
    ]

prettyTypeCondition :: TypeCondition -> Doc a
prettyTypeCondition TypeCondition {..} =
  P.sep [P.pretty "on", prettyNamedType typeConditionValue]

prettyFragmentDefinition :: FragmentDefinition -> Doc a
prettyFragmentDefinition FragmentDefinition {..} =
  sepMaybes
    [ Just (P.pretty "fragment")
    , Just (prettyFragmentName fragmentName)
    , Just (prettyTypeCondition fragmentTypeCondition)
    , fmap prettyDirectives fragmentDirectives
    , Just (prettySelectionSet fragmentSelectionSet)
    ]

prettyTypeSystemDefinition :: TypeSystemDefinition -> Doc a
prettyTypeSystemDefinition x =
  case x of
    TypeSystemDefinitionSchema y -> prettySchemaDefinition y
    TypeSystemDefinitionType y -> prettyTypeDefinition y
    TypeSystemDefinitionTypeExtension y -> prettyTypeExtensionDefinition y
    TypeSystemDefinitionDirective y -> prettyDirectiveDefinition y

prettySchemaDefinition :: SchemaDefinition -> Doc a
prettySchemaDefinition SchemaDefinition {..} =
  sepMaybes
    [ Just (P.pretty "schema")
    , fmap prettyDirectives schemaDefinitionDirectives
    , Just (prettyOperationTypeDefinitions schemaDefinitionOperationTypes)
    ]

prettyOperationTypeDefinitions :: OperationTypeDefinitions -> Doc a
prettyOperationTypeDefinitions OperationTypeDefinitions {..} =
  P.braces
    (P.sep (map prettyOperationTypeDefinition operationTypeDefinitionsValue))

prettyOperationTypeDefinition :: OperationTypeDefinition -> Doc a
prettyOperationTypeDefinition OperationTypeDefinition {..} =
  P.sep
    [ P.hcat
        [prettyOperationType operationTypeDefinitionOperation, P.pretty ":"]
    , prettyNamedType operationTypeDefinitionType
    ]

prettyTypeDefinition :: TypeDefinition -> Doc a
prettyTypeDefinition x =
  case x of
    TypeDefinitionScalar y -> prettyScalarTypeDefinition y
    TypeDefinitionObject y -> prettyObjectTypeDefinition y
    TypeDefinitionInterface y -> prettyInterfaceTypeDefinition y
    TypeDefinitionUnion y -> prettyUnionTypeDefinition y
    TypeDefinitionEnum y -> prettyEnumTypeDefinition y
    TypeDefinitionInputObject y -> prettyInputObjectTypeDefinition y

prettyScalarTypeDefinition :: ScalarTypeDefinition -> Doc a
prettyScalarTypeDefinition ScalarTypeDefinition {..} =
  sepMaybes
    [ Just (P.pretty "scalar")
    , Just (prettyName scalarTypeDefinitionName)
    , fmap prettyDirectives scalarTypeDefinitionDirectives
    ]

prettyObjectTypeDefinition :: ObjectTypeDefinition -> Doc a
prettyObjectTypeDefinition ObjectTypeDefinition {..} =
  sepMaybes
    [ Just (P.pretty "type")
    , Just (prettyName objectTypeDefinitionName)
    , fmap prettyInterfaces objectTypeDefinitionInterfaces
    , fmap prettyDirectives objectTypeDefinitionDirectives
    , Just (prettyFieldDefinitions objectTypeDefinitionFields)
    ]

prettyInterfaces :: Interfaces -> Doc a
prettyInterfaces Interfaces {..} =
  P.sep
    (P.pretty "implements" :
     map prettyNamedType (NonEmpty.toList interfacesValue))

prettyFieldDefinitions :: FieldDefinitions -> Doc a
prettyFieldDefinitions FieldDefinitions {..} =
  P.braces (P.sep (map prettyFieldDefinition fieldDefinitionsValue))

prettyFieldDefinition :: FieldDefinition -> Doc a
prettyFieldDefinition FieldDefinition {..} =
  sepMaybes
    [ Just
        (P.hcat
           (Maybe.catMaybes
              [ Just (prettyName fieldDefinitionName)
              , fmap prettyInputValueDefinitions fieldDefinitionArguments
              , Just (P.pretty ":")
              ]))
    , Just (prettyType fieldDefinitionType)
    , fmap prettyDirectives fieldDefinitionDirectives
    ]

prettyInputValueDefinitions :: InputValueDefinitions -> Doc a
prettyInputValueDefinitions InputValueDefinitions {..} =
  P.parens (P.sep (map prettyInputValueDefinition inputValueDefinitionsValue))

prettyInputValueDefinition :: InputValueDefinition -> Doc a
prettyInputValueDefinition InputValueDefinition {..} =
  sepMaybes
    [ Just (P.hcat [prettyName inputValueDefinitionName, P.pretty ":"])
    , Just (prettyType inputValueDefinitionType)
    , fmap prettyDefaultValue inputValueDefinitionDefaultValue
    , fmap prettyDirectives inputValueDefinitionDirectives
    ]

prettyInterfaceTypeDefinition :: InterfaceTypeDefinition -> Doc a
prettyInterfaceTypeDefinition InterfaceTypeDefinition {..} =
  sepMaybes
    [ Just (P.pretty "interface")
    , Just (prettyName interfaceTypeDefinitionName)
    , fmap prettyDirectives interfaceTypeDefinitionDirectives
    , Just (prettyFieldDefinitions interfaceTypeDefinitionFields)
    ]

prettyUnionTypeDefinition :: UnionTypeDefinition -> Doc a
prettyUnionTypeDefinition UnionTypeDefinition {..} =
  sepMaybes
    [ Just (P.pretty "union")
    , Just (prettyName unionTypeDefinitionName)
    , fmap prettyDirectives unionTypeDefinitionDirectives
    , Just (P.pretty "=")
    , Just (prettyUnionTypes unionTypeDefinitionTypes)
    ]

prettyUnionTypes :: UnionTypes -> Doc a
prettyUnionTypes UnionTypes {..} =
  P.encloseSep
    mempty
    mempty
    (mconcat [P.space, P.pretty "|", P.space])
    (map prettyNamedType (NonEmpty.toList unionTypesValue))

prettyEnumTypeDefinition :: EnumTypeDefinition -> Doc a
prettyEnumTypeDefinition EnumTypeDefinition {..} =
  sepMaybes
    [ Just (P.pretty "enum")
    , Just (prettyName enumTypeDefinitionName)
    , fmap prettyDirectives enumTypeDefinitionDirectives
    , Just (prettyEnumValues enumTypeDefinitionValues)
    ]

prettyEnumValues :: EnumValues -> Doc a
prettyEnumValues EnumValues {..} =
  P.braces (P.sep (map prettyEnumValueDefinition enumValuesValue))

prettyEnumValueDefinition :: EnumValueDefinition -> Doc a
prettyEnumValueDefinition EnumValueDefinition {..} =
  sepMaybes
    [ Just (prettyName enumValueDefinitionName)
    , fmap prettyDirectives enumValueDefinitionDirectives
    ]

prettyInputObjectTypeDefinition :: InputObjectTypeDefinition -> Doc a
prettyInputObjectTypeDefinition InputObjectTypeDefinition {..} =
  sepMaybes
    [ Just (P.pretty "input")
    , Just (prettyName inputObjectTypeDefinitionName)
    , fmap prettyDirectives inputObjectTypeDefinitionDirectives
    , Just (prettyInputFieldDefinitions inputObjectTypeDefinitionFields)
    ]

prettyInputFieldDefinitions :: InputFieldDefinitions -> Doc a
prettyInputFieldDefinitions InputFieldDefinitions {..} =
  P.braces (P.sep (map prettyInputValueDefinition inputFieldDefinitionsValue))

prettyTypeExtensionDefinition :: TypeExtensionDefinition -> Doc a
prettyTypeExtensionDefinition TypeExtensionDefinition {..} =
  P.sep
    [P.pretty "extend", prettyObjectTypeDefinition typeExtensionDefinitionValue]

prettyDirectiveDefinition :: DirectiveDefinition -> Doc a
prettyDirectiveDefinition DirectiveDefinition {..} =
  sepMaybes
    [ Just (P.pretty "directive")
    , Just (P.hcat [P.pretty "@", prettyName directiveDefinitionName])
    , fmap prettyInputValueDefinitions directiveDefinitionArguments
    , Just (P.pretty "on")
    , Just (prettyDirectiveLocations directiveDefinitionLocations)
    ]

prettyDirectiveLocations :: DirectiveLocations -> Doc a
prettyDirectiveLocations DirectiveLocations {..} =
  P.encloseSep
    mempty
    mempty
    (mconcat [P.space, P.pretty "|", P.space])
    (map prettyName (NonEmpty.toList directiveLocationsValue))
