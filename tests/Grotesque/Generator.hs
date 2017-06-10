{-# LANGUAGE NumDecimals #-}

module Grotesque.Generator where

import Data.Scientific
import Grotesque
import Hedgehog

import qualified Data.Text as Text
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R


genDocument :: Gen IO Document
genDocument = Document <$> G.list (R.linear 0 2) genDefinition


genDefinition :: Gen IO Definition
genDefinition = G.choice
  [ DefinitionOperation <$> genOperationDefinition
  , DefinitionFragment <$> genFragmentDefinition
  , DefinitionTypeSystem <$> genTypeSystemDefinition
  ]


genOperationDefinition :: Gen IO OperationDefinition
genOperationDefinition = OperationDefinition
  <$> genOperationType
  <*> G.maybe genName
  <*> G.maybe genVariableDefinitions
  <*> G.maybe genDirectives
  <*> genSelectionSet


genOperationType :: Gen IO OperationType
genOperationType = G.enumBounded


genName :: Gen IO Name
genName = do
  let
    underscore = ['_']
    uppers = ['A' .. 'Z']
    lowers = ['a' .. 'z']
    digits = ['0' .. '9']
  first <- G.element $ concat [underscore, uppers, lowers]
  rest <- G.list (R.linear 0 7) . G.element $ concat [underscore, uppers, lowers, digits]
  pure . Name . Text.pack $ first : rest


genVariableDefinitions :: Gen IO VariableDefinitions
genVariableDefinitions = VariableDefinitions <$> G.list (R.linear 0 2) genVariableDefinition


genVariableDefinition :: Gen IO VariableDefinition
genVariableDefinition = VariableDefinition
  <$> genVariable
  <*> genType
  <*> G.maybe genDefaultValue


genVariable :: Gen IO Variable
genVariable = Variable <$> genName


genType :: Gen IO Type
genType = G.choice
  [ TypeNamed <$> genNamedType
  , TypeList <$> genListType
  , TypeNonNull <$> genNonNullType
  ]


genNamedType :: Gen IO NamedType
genNamedType = NamedType <$> genName


genListType :: Gen IO ListType
genListType = ListType <$> genType


genNonNullType :: Gen IO NonNullType
genNonNullType = G.choice
  [ NonNullTypeNamed <$> genNamedType
  , NonNullTypeList <$> genListType
  ]


genDefaultValue :: Gen IO DefaultValue
genDefaultValue = DefaultValue <$> genValue


genValue :: Gen IO Value
genValue = G.choice
  [ ValueVariable <$> genVariable
  , ValueInt <$> G.integral (R.linearFrom 0 (-1e9) 1e9)
  , ValueFloat . fromFloatDigits <$> G.double (R.linearFracFrom 0 (-1e9) 1e9)
  , ValueString <$> G.text (R.linear 0 8) G.unicode
  , ValueBoolean <$> G.bool
  , pure ValueNull
  , ValueEnum <$> genName
  , ValueList <$> G.list (R.linear 0 2) genValue
  , ValueObject <$> G.list (R.linear 0 2) genObjectField
  ]


genObjectField :: Gen IO ObjectField
genObjectField = ObjectField
  <$> genName
  <*> genValue


genDirectives :: Gen IO Directives
genDirectives = Directives <$> G.nonEmpty (R.linear 1 2) genDirective


genDirective :: Gen IO Directive
genDirective = Directive
  <$> genName
  <*> G.maybe genArguments


genSelectionSet :: Gen IO SelectionSet
genSelectionSet = SelectionSet <$> G.list (R.linear 0 2) genSelection


genSelection :: Gen IO Selection
genSelection = G.choice
  [ SelectionField <$> genField
  , SelectionFragmentSpread <$> genFragmentSpread
  , SelectionInlineFragment <$> genInlineFragment
  ]


genField :: Gen IO Field
genField = Field
  <$> G.maybe genAlias
  <*> genName
  <*> G.maybe genArguments
  <*> G.maybe genDirectives
  <*> G.maybe genSelectionSet


genAlias :: Gen IO Alias
genAlias = Alias <$> genName


genArguments :: Gen IO Arguments
genArguments = Arguments <$> G.list (R.linear 0 2) genArgument


genArgument :: Gen IO Argument
genArgument = Argument
  <$> genName
  <*> genValue


genFragmentSpread :: Gen IO FragmentSpread
genFragmentSpread = FragmentSpread
  <$> genFragmentName
  <*> G.maybe genDirectives


genFragmentName :: Gen IO FragmentName
genFragmentName = FragmentName <$> genName


genInlineFragment :: Gen IO InlineFragment
genInlineFragment = InlineFragment
  <$> G.maybe genTypeCondition
  <*> G.maybe genDirectives
  <*> genSelectionSet


genTypeCondition :: Gen IO TypeCondition
genTypeCondition = TypeCondition <$> genNamedType


genFragmentDefinition :: Gen IO FragmentDefinition
genFragmentDefinition = FragmentDefinition
  <$> genFragmentName
  <*> genTypeCondition
  <*> G.maybe genDirectives
  <*> genSelectionSet


genTypeSystemDefinition :: Gen IO TypeSystemDefinition
genTypeSystemDefinition = G.choice
  [ TypeSystemDefinitionSchema <$> genSchemaDefinition
  , TypeSystemDefinitionType <$> genTypeDefinition
  , TypeSystemDefinitionTypeExtension <$> genTypeExtensionDefinition
  , TypeSystemDefinitionDirective <$> genDirectiveDefinition
  ]


genSchemaDefinition :: Gen IO SchemaDefinition
genSchemaDefinition = SchemaDefinition
  <$> G.maybe genDirectives
  <*> genOperationTypeDefinitions


genOperationTypeDefinitions :: Gen IO OperationTypeDefinitions
genOperationTypeDefinitions = OperationTypeDefinitions <$> G.list (R.linear 0 2) genOperationTypeDefinition


genOperationTypeDefinition :: Gen IO OperationTypeDefinition
genOperationTypeDefinition = OperationTypeDefinition
  <$> genOperationType
  <*> genNamedType


genTypeDefinition :: Gen IO TypeDefinition
genTypeDefinition = G.choice
  [ TypeDefinitionScalar <$> genScalarTypeDefinition
  , TypeDefinitionObject <$> genObjectTypeDefinition
  , TypeDefinitionInterface <$> genInterfaceTypeDefinition
  , TypeDefinitionUnion <$> genUnionTypeDefinition
  , TypeDefinitionEnum <$> genEnumTypeDefinition
  , TypeDefinitionInputObject <$> genInputObjectTypeDefinition
  ]


genScalarTypeDefinition :: Gen IO ScalarTypeDefinition
genScalarTypeDefinition = ScalarTypeDefinition
  <$> genName
  <*> G.maybe genDirectives


genObjectTypeDefinition :: Gen IO ObjectTypeDefinition
genObjectTypeDefinition = ObjectTypeDefinition
  <$> genName
  <*> G.maybe genInterfaces
  <*> G.maybe genDirectives
  <*> genFieldDefinitions


genInterfaces :: Gen IO Interfaces
genInterfaces = Interfaces <$> G.nonEmpty (R.linear 1 2) genNamedType


genFieldDefinitions :: Gen IO FieldDefinitions
genFieldDefinitions = FieldDefinitions <$> G.list (R.linear 0 2) genFieldDefinition


genFieldDefinition :: Gen IO FieldDefinition
genFieldDefinition = FieldDefinition
  <$> genName
  <*> G.maybe genInputValueDefinitions
  <*> genType
  <*> G.maybe genDirectives


genInputValueDefinitions :: Gen IO InputValueDefinitions
genInputValueDefinitions = InputValueDefinitions <$> G.list (R.linear 0 2) genInputValueDefinition


genInputValueDefinition :: Gen IO InputValueDefinition
genInputValueDefinition = InputValueDefinition
  <$> genName
  <*> genType
  <*> G.maybe genDefaultValue
  <*> G.maybe genDirectives


genInterfaceTypeDefinition :: Gen IO InterfaceTypeDefinition
genInterfaceTypeDefinition = InterfaceTypeDefinition
  <$> genName
  <*> G.maybe genDirectives
  <*> genFieldDefinitions


genUnionTypeDefinition :: Gen IO UnionTypeDefinition
genUnionTypeDefinition = UnionTypeDefinition
  <$> genName
  <*> G.maybe genDirectives
  <*> genUnionTypes


genUnionTypes :: Gen IO UnionTypes
genUnionTypes = UnionTypes <$> G.nonEmpty (R.linear 1 2) genNamedType


genEnumTypeDefinition :: Gen IO EnumTypeDefinition
genEnumTypeDefinition = EnumTypeDefinition
  <$> genName
  <*> G.maybe genDirectives
  <*> genEnumValues


genEnumValues :: Gen IO EnumValues
genEnumValues = EnumValues <$> G.list (R.linear 0 2) genEnumValueDefinition


genEnumValueDefinition :: Gen IO EnumValueDefinition
genEnumValueDefinition = EnumValueDefinition
  <$> genName
  <*> G.maybe genDirectives


genInputObjectTypeDefinition :: Gen IO InputObjectTypeDefinition
genInputObjectTypeDefinition = InputObjectTypeDefinition
  <$> genName
  <*> G.maybe genDirectives
  <*> genInputFieldDefinitions


genInputFieldDefinitions :: Gen IO InputFieldDefinitions
genInputFieldDefinitions = InputFieldDefinitions <$> G.list (R.linear 0 2) genInputValueDefinition


genTypeExtensionDefinition :: Gen IO TypeExtensionDefinition
genTypeExtensionDefinition = TypeExtensionDefinition <$> genObjectTypeDefinition


genDirectiveDefinition :: Gen IO DirectiveDefinition
genDirectiveDefinition = G.discard -- TODO
