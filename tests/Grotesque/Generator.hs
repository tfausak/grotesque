{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Grotesque.Generator where

import Grotesque.Language

import Data.Scientific (Scientific)
import Data.Text (Text)
import Hedgehog (Gen)

import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import qualified Hedgehog.Gen as H
import qualified Hedgehog.Range as H

genDocument :: Monad m => Gen m Document
genDocument = do
  documentValue <- H.list (H.linear 0 2) genDefinition
  pure Document {..}

genDefinition :: Monad m => Gen m Definition
genDefinition =
  H.choice
    [ fmap DefinitionOperation genOperationDefinition
    , fmap DefinitionFragment genFragmentDefinition
    , fmap DefinitionTypeSystem genTypeSystemDefinition
    ]

genOperationDefinition :: Monad m => Gen m OperationDefinition
genOperationDefinition = do
  operationDefinitionOperationType <- genOperationType
  operationDefinitionName <- H.maybe genName
  operationDefinitionVariableDefinitions <- H.maybe genVariableDefinitions
  operationDefinitionDirectives <- H.maybe genDirectives
  operationDefinitionSelectionSet <- genSelectionSet
  pure OperationDefinition {..}

genOperationType :: Monad m => Gen m OperationType
genOperationType = H.enumBounded

genName :: Monad m => Gen m Name
genName = do
  first <- H.element nameFirstChars
  rest <- H.list (H.linear 0 7) (H.element nameRestChars)
  pure (Name (Text.pack (first : rest)))

nameFirstChars :: String
nameFirstChars = concat ["_", ['A' .. 'Z'], ['a' .. 'z']]

nameRestChars :: String
nameRestChars = nameFirstChars ++ ['0' .. '9']

genVariableDefinitions :: Monad m => Gen m VariableDefinitions
genVariableDefinitions = do
  variableDefinitionsValue <- H.list (H.linear 0 2) genVariableDefinition
  pure VariableDefinitions {..}

genVariableDefinition :: Monad m => Gen m VariableDefinition
genVariableDefinition = do
  variableDefinitionVariable <- genVariable
  variableDefinitionType <- genType
  variableDefinitionDefaultValue <- H.maybe genDefaultValue
  pure VariableDefinition {..}

genVariable :: Monad m => Gen m Variable
genVariable = do
  variableValue <- genName
  pure Variable {..}

genType :: Monad m => Gen m Type
genType =
  H.choice
    [ fmap TypeNamed genNamedType
    , fmap TypeList genListType
    , fmap TypeNonNull genNonNullType
    ]

genNamedType :: Monad m => Gen m NamedType
genNamedType = do
  namedTypeValue <- genName
  pure NamedType {..}

genListType :: Monad m => Gen m ListType
genListType = do
  listTypeValue <- genType
  pure ListType {..}

genNonNullType :: Monad m => Gen m NonNullType
genNonNullType =
  H.choice
    [fmap NonNullTypeNamed genNamedType, fmap NonNullTypeList genListType]

genDefaultValue :: Monad m => Gen m DefaultValue
genDefaultValue = do
  defaultValueValue <- genValue
  pure DefaultValue {..}

genValue :: Monad m => Gen m Value
genValue =
  H.choice
    [ fmap ValueVariable genVariable
    , fmap ValueInt genInt
    , fmap ValueFloat genFloat
    , fmap ValueString genString
    , fmap ValueBoolean genBoolean
    , pure ValueNull
    , fmap ValueEnum genEnum
    , fmap ValueList genList
    , fmap ValueObject genObject
    ]

genInt :: Monad m => Gen m Integer
genInt = H.integral (H.linearFrom 0 (-1000000000) 1000000000)

-- TODO
genFloat :: Monad m => Gen m Scientific
genFloat =
  Scientific.fromFloatDigits <$> H.double (H.linearFracFrom 0 (-1e9) 1e9)

genString :: Monad m => Gen m Text
genString = H.text (H.linear 0 8) H.unicode

genBoolean :: Monad m => Gen m Bool
genBoolean = H.bool

genEnum :: Monad m => Gen m Name
genEnum = genName

genList :: Monad m => Gen m [Value]
genList = H.list (H.linear 0 2) genValue

genObject :: Monad m => Gen m [ObjectField]
genObject = H.list (H.linear 0 2) genObjectField

genObjectField :: Monad m => Gen m ObjectField
genObjectField = do
  objectFieldName <- genName
  objectFieldValue <- genValue
  pure ObjectField {..}

genDirectives :: Monad m => Gen m Directives
genDirectives = do
  directivesValue <- H.nonEmpty (H.linear 1 2) genDirective
  pure Directives {..}

genDirective :: Monad m => Gen m Directive
genDirective = do
  directiveName <- genName
  directiveArguments <- H.maybe genArguments
  pure Directive {..}

genSelectionSet :: Monad m => Gen m SelectionSet
genSelectionSet = do
  selectionSetValue <- H.list (H.linear 0 2) genSelection
  pure SelectionSet {..}

genSelection :: Monad m => Gen m Selection
genSelection =
  H.choice
    [ fmap SelectionField genField
    , fmap SelectionFragmentSpread genFragmentSpread
    , fmap SelectionInlineFragment genInlineFragment
    ]

genField :: Monad m => Gen m Field
genField = do
  fieldAlias <- H.maybe genAlias
  fieldName <- genName
  fieldArguments <- H.maybe genArguments
  fieldDirectives <- H.maybe genDirectives
  fieldSelectionSet <- H.maybe genSelectionSet
  pure Field {..}

genAlias :: Monad m => Gen m Alias
genAlias = do
  aliasValue <- genName
  pure Alias {..}

genArguments :: Monad m => Gen m Arguments
genArguments = do
  argumentsValue <- H.list (H.linear 0 2) genArgument
  pure Arguments {..}

genArgument :: Monad m => Gen m Argument
genArgument = do
  argumentName <- genName
  argumentValue <- genValue
  pure Argument {..}

genFragmentSpread :: Monad m => Gen m FragmentSpread
genFragmentSpread = do
  fragmentSpreadName <- genFragmentName
  fragmentSpreadDirectives <- H.maybe genDirectives
  pure FragmentSpread {..}

genFragmentName :: Monad m => Gen m FragmentName
genFragmentName = do
  fragmentNameValue <- genName
  pure FragmentName {..}

genInlineFragment :: Monad m => Gen m InlineFragment
genInlineFragment = do
  inlineFragmentTypeCondition <- H.maybe genTypeCondition
  inlineFragmentDirectives <- H.maybe genDirectives
  inlineFragmentSelectionSet <- genSelectionSet
  pure InlineFragment {..}

genTypeCondition :: Monad m => Gen m TypeCondition
genTypeCondition = do
  typeConditionValue <- genNamedType
  pure TypeCondition {..}

genFragmentDefinition :: Monad m => Gen m FragmentDefinition
genFragmentDefinition = do
  fragmentName <- genFragmentName
  fragmentTypeCondition <- genTypeCondition
  fragmentDirectives <- H.maybe genDirectives
  fragmentSelectionSet <- genSelectionSet
  pure FragmentDefinition {..}

genTypeSystemDefinition :: Monad m => Gen m TypeSystemDefinition
genTypeSystemDefinition =
  H.choice
    [ fmap TypeSystemDefinitionSchema genSchemaDefinition
    , fmap TypeSystemDefinitionType genTypeDefinition
    , fmap TypeSystemDefinitionTypeExtension genTypeExtensionDefinition
    , fmap TypeSystemDefinitionDirective genDirectiveDefinition
    ]

genSchemaDefinition :: Monad m => Gen m SchemaDefinition
genSchemaDefinition = do
  schemaDefinitionDirectives <- H.maybe genDirectives
  schemaDefinitionOperationTypes <- genOperationTypeDefinitions
  pure SchemaDefinition {..}

genOperationTypeDefinitions :: Monad m => Gen m OperationTypeDefinitions
genOperationTypeDefinitions = do
  operationTypeDefinitionsValue <-
    H.list (H.linear 0 2) genOperationTypeDefinition
  pure OperationTypeDefinitions {..}

genOperationTypeDefinition :: Monad m => Gen m OperationTypeDefinition
genOperationTypeDefinition = do
  operationTypeDefinitionOperation <- genOperationType
  operationTypeDefinitionType <- genNamedType
  pure OperationTypeDefinition {..}

genTypeDefinition :: Monad m => Gen m TypeDefinition
genTypeDefinition =
  H.choice
    [ fmap TypeDefinitionScalar genScalarTypeDefinition
    , fmap TypeDefinitionObject genObjectTypeDefinition
    , fmap TypeDefinitionInterface genInterfaceTypeDefinition
    , fmap TypeDefinitionUnion genUnionTypeDefinition
    , fmap TypeDefinitionEnum genEnumTypeDefinition
    , fmap TypeDefinitionInputObject genInputObjectTypeDefinition
    ]

genScalarTypeDefinition :: Monad m => Gen m ScalarTypeDefinition
genScalarTypeDefinition = do
  scalarTypeDefinitionName <- genName
  scalarTypeDefinitionDirectives <- H.maybe genDirectives
  pure ScalarTypeDefinition {..}

genObjectTypeDefinition :: Monad m => Gen m ObjectTypeDefinition
genObjectTypeDefinition = do
  objectTypeDefinitionName <- genName
  objectTypeDefinitionInterfaces <- H.maybe genInterfaces
  objectTypeDefinitionDirectives <- H.maybe genDirectives
  objectTypeDefinitionFields <- genFieldDefinitions
  pure ObjectTypeDefinition {..}

genInterfaces :: Monad m => Gen m Interfaces
genInterfaces = do
  interfacesValue <- H.nonEmpty (H.linear 1 2) genNamedType
  pure Interfaces {..}

genFieldDefinitions :: Monad m => Gen m FieldDefinitions
genFieldDefinitions = do
  fieldDefinitionsValue <- H.list (H.linear 0 2) genFieldDefinition
  pure FieldDefinitions {..}

genFieldDefinition :: Monad m => Gen m FieldDefinition
genFieldDefinition = do
  fieldDefinitionName <- genName
  fieldDefinitionArguments <- H.maybe genInputValueDefinitions
  fieldDefinitionType <- genType
  fieldDefinitionDirectives <- H.maybe genDirectives
  pure FieldDefinition {..}

genInputValueDefinitions :: Monad m => Gen m InputValueDefinitions
genInputValueDefinitions = do
  inputValueDefinitionsValue <- H.list (H.linear 0 2) genInputValueDefinition
  pure InputValueDefinitions {..}

genInputValueDefinition :: Monad m => Gen m InputValueDefinition
genInputValueDefinition = do
  inputValueDefinitionName <- genName
  inputValueDefinitionType <- genType
  inputValueDefinitionDefaultValue <- H.maybe genDefaultValue
  inputValueDefinitionDirectives <- H.maybe genDirectives
  pure InputValueDefinition {..}

genInterfaceTypeDefinition :: Monad m => Gen m InterfaceTypeDefinition
genInterfaceTypeDefinition = do
  interfaceTypeDefinitionName <- genName
  interfaceTypeDefinitionDirectives <- H.maybe genDirectives
  interfaceTypeDefinitionFields <- genFieldDefinitions
  pure InterfaceTypeDefinition {..}

genUnionTypeDefinition :: Monad m => Gen m UnionTypeDefinition
genUnionTypeDefinition = do
  unionTypeDefinitionName <- genName
  unionTypeDefinitionDirectives <- H.maybe genDirectives
  unionTypeDefinitionTypes <- genUnionTypes
  pure UnionTypeDefinition {..}

genUnionTypes :: Monad m => Gen m UnionTypes
genUnionTypes = do
  unionTypesValue <- H.nonEmpty (H.linear 1 2) genNamedType
  pure UnionTypes {..}

genEnumTypeDefinition :: Monad m => Gen m EnumTypeDefinition
genEnumTypeDefinition = do
  enumTypeDefinitionName <- genName
  enumTypeDefinitionDirectives <- H.maybe genDirectives
  enumTypeDefinitionValues <- genEnumValues
  pure EnumTypeDefinition {..}

genEnumValues :: Monad m => Gen m EnumValues
genEnumValues = do
  enumValuesValue <- H.list (H.linear 0 2) genEnumValueDefinition
  pure EnumValues {..}

genEnumValueDefinition :: Monad m => Gen m EnumValueDefinition
genEnumValueDefinition = do
  enumValueDefinitionName <- genName
  enumValueDefinitionDirectives <- H.maybe genDirectives
  pure EnumValueDefinition {..}

genInputObjectTypeDefinition :: Monad m => Gen m InputObjectTypeDefinition
genInputObjectTypeDefinition = do
  inputObjectTypeDefinitionName <- genName
  inputObjectTypeDefinitionDirectives <- H.maybe genDirectives
  inputObjectTypeDefinitionFields <- genInputFieldDefinitions
  pure InputObjectTypeDefinition {..}

genInputFieldDefinitions :: Monad m => Gen m InputFieldDefinitions
genInputFieldDefinitions = do
  inputFieldDefinitionsValue <- H.list (H.linear 0 2) genInputValueDefinition
  pure InputFieldDefinitions {..}

genTypeExtensionDefinition :: Monad m => Gen m TypeExtensionDefinition
genTypeExtensionDefinition = do
  typeExtensionDefinitionValue <- genObjectTypeDefinition
  pure TypeExtensionDefinition {..}

genDirectiveDefinition :: Monad m => Gen m DirectiveDefinition
genDirectiveDefinition = do
  directiveDefinitionName <- genName
  directiveDefinitionArguments <- H.maybe genInputValueDefinitions
  directiveDefinitionLocations <- genDirectiveLocations
  pure DirectiveDefinition {..}

genDirectiveLocations :: Monad m => Gen m DirectiveLocations
genDirectiveLocations = do
  directiveLocationsValue <- H.nonEmpty (H.linear 1 2) genName
  pure DirectiveLocations {..}
