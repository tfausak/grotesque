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
genDocument = Document
  <$> H.list (H.linear 0 2) genDefinition


genDefinition :: Monad m => Gen m Definition
genDefinition = H.choice
  [ DefinitionOperation <$> genOperationDefinition
  , DefinitionFragment <$> genFragmentDefinition
  , DefinitionTypeSystem <$> genTypeSystemDefinition
  ]


genOperationDefinition :: Monad m => Gen m OperationDefinition
genOperationDefinition = OperationDefinition
  <$> genOperationType
  <*> H.maybe genName
  <*> H.maybe genVariableDefinitions
  <*> H.maybe genDirectives
  <*> genSelectionSet


genOperationType :: Monad m => Gen m OperationType
genOperationType = H.enumBounded


genName :: Monad m => Gen m Name
genName = (\h t -> Name (Text.pack (h : t)))
  <$> H.element nameFirstChars
  <*> H.list (H.linear 0 7) (H.element nameRestChars)


nameFirstChars :: [Char]
nameFirstChars = concat [['_'], ['A' .. 'Z'], ['a' .. 'z']]


nameRestChars :: [Char]
nameRestChars = concat [nameFirstChars, ['0' .. '9']]


genVariableDefinitions :: Monad m => Gen m VariableDefinitions
genVariableDefinitions = VariableDefinitions
  <$> H.list (H.linear 0 2) genVariableDefinition


genVariableDefinition :: Monad m => Gen m VariableDefinition
genVariableDefinition = VariableDefinition
  <$> genVariable
  <*> genType
  <*> H.maybe genDefaultValue


genVariable :: Monad m => Gen m Variable
genVariable = Variable
  <$> genName


genType :: Monad m => Gen m Type
genType = H.choice
  [ TypeNamed <$> genNamedType
  , TypeList <$> genListType
  , TypeNonNull <$> genNonNullType
  ]


genNamedType :: Monad m => Gen m NamedType
genNamedType = NamedType <$> genName


genListType :: Monad m => Gen m ListType
genListType = ListType <$> genType


genNonNullType :: Monad m => Gen m NonNullType
genNonNullType = H.choice
  [ NonNullTypeNamed <$> genNamedType
  , NonNullTypeList <$> genListType
  ]


genDefaultValue :: Monad m => Gen m DefaultValue
genDefaultValue = DefaultValue
  <$> genValue


genValue :: Monad m => Gen m Value
genValue = H.choice
  [ ValueVariable <$> genVariable
  , ValueInt <$> genInt
  , ValueFloat <$> genFloat
  , ValueString <$> genString
  , ValueBoolean <$> genBoolean
  , pure ValueNull
  , ValueEnum <$> genEnum
  , ValueList <$> genList
  , ValueObject <$> genObject
  ]


genInt :: Monad m => Gen m Integer
genInt = H.integral (H.linearFrom 0 (-1000000000) 1000000000)


genFloat :: Monad m => Gen m Scientific
genFloat = Scientific.fromFloatDigits
  <$> H.double (H.linearFracFrom 0 (-1e9) 1e9)


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
genObjectField = ObjectField
  <$> genName
  <*> genValue


genDirectives :: Monad m => Gen m Directives
genDirectives = Directives
  <$> H.nonEmpty (H.linear 1 2) genDirective


genDirective :: Monad m => Gen m Directive
genDirective = Directive
  <$> genName
  <*> H.maybe genArguments


genSelectionSet :: Monad m => Gen m SelectionSet
genSelectionSet = SelectionSet
  <$> H.list (H.linear 0 2) genSelection


genSelection :: Monad m => Gen m Selection
genSelection = H.choice
  [ SelectionField <$> genField
  , SelectionFragmentSpread <$> genFragmentSpread
  , SelectionInlineFragment <$> genInlineFragment
  ]


genField :: Monad m => Gen m Field
genField = Field
  <$> H.maybe genAlias
  <*> genName
  <*> H.maybe genArguments
  <*> H.maybe genDirectives
  <*> H.maybe genSelectionSet


genAlias :: Monad m => Gen m Alias
genAlias = Alias <$> genName


genArguments :: Monad m => Gen m Arguments
genArguments = Arguments
  <$> H.list (H.linear 0 2) genArgument


genArgument :: Monad m => Gen m Argument
genArgument = Argument
  <$> genName
  <*> genValue


genFragmentSpread :: Monad m => Gen m FragmentSpread
genFragmentSpread = FragmentSpread
  <$> genFragmentName
  <*> H.maybe genDirectives


genFragmentName :: Monad m => Gen m FragmentName
genFragmentName = FragmentName
  <$> genName


genInlineFragment :: Monad m => Gen m InlineFragment
genInlineFragment = InlineFragment
  <$> H.maybe genTypeCondition
  <*> H.maybe genDirectives
  <*> genSelectionSet


genTypeCondition :: Monad m => Gen m TypeCondition
genTypeCondition = TypeCondition
  <$> genNamedType


genFragmentDefinition :: Monad m => Gen m FragmentDefinition
genFragmentDefinition = FragmentDefinition
  <$> genFragmentName
  <*> genTypeCondition
  <*> H.maybe genDirectives
  <*> genSelectionSet


genTypeSystemDefinition :: Monad m => Gen m TypeSystemDefinition
genTypeSystemDefinition = H.choice
  [ TypeSystemDefinitionSchema <$> genSchemaDefinition
  , TypeSystemDefinitionType <$> genTypeDefinition
  , TypeSystemDefinitionTypeExtension <$> genTypeExtensionDefinition
  , TypeSystemDefinitionDirective <$> genDirectiveDefinition
  ]


genSchemaDefinition :: Monad m => Gen m SchemaDefinition
genSchemaDefinition = SchemaDefinition
  <$> H.maybe genDirectives
  <*> genOperationTypeDefinitions


genOperationTypeDefinitions :: Monad m => Gen m OperationTypeDefinitions
genOperationTypeDefinitions = OperationTypeDefinitions
  <$> H.list (H.linear 0 2) genOperationTypeDefinition


genOperationTypeDefinition :: Monad m => Gen m OperationTypeDefinition
genOperationTypeDefinition = OperationTypeDefinition
  <$> genOperationType
  <*> genNamedType


genTypeDefinition :: Monad m => Gen m TypeDefinition
genTypeDefinition = H.choice
  [ TypeDefinitionScalar <$> genScalarTypeDefinition
  , TypeDefinitionObject <$> genObjectTypeDefinition
  , TypeDefinitionInterface <$> genInterfaceTypeDefinition
  , TypeDefinitionUnion <$> genUnionTypeDefinition
  , TypeDefinitionEnum <$> genEnumTypeDefinition
  , TypeDefinitionInputObject <$> genInputObjectTypeDefinition
  ]


genScalarTypeDefinition :: Monad m => Gen m ScalarTypeDefinition
genScalarTypeDefinition = ScalarTypeDefinition
  <$> genName
  <*> H.maybe genDirectives


genObjectTypeDefinition :: Monad m => Gen m ObjectTypeDefinition
genObjectTypeDefinition = ObjectTypeDefinition
  <$> genName
  <*> H.maybe genInterfaces
  <*> H.maybe genDirectives
  <*> genFieldDefinitions


genInterfaces :: Monad m => Gen m Interfaces
genInterfaces = Interfaces
  <$> H.nonEmpty (H.linear 1 2) genNamedType


genFieldDefinitions :: Monad m => Gen m FieldDefinitions
genFieldDefinitions = FieldDefinitions
  <$> H.list (H.linear 0 2) genFieldDefinition


genFieldDefinition :: Monad m => Gen m FieldDefinition
genFieldDefinition = FieldDefinition
  <$> genName
  <*> H.maybe genInputValueDefinitions
  <*> genType
  <*> H.maybe genDirectives


genInputValueDefinitions :: Monad m => Gen m InputValueDefinitions
genInputValueDefinitions = InputValueDefinitions
  <$> H.list (H.linear 0 2) genInputValueDefinition


genInputValueDefinition :: Monad m => Gen m InputValueDefinition
genInputValueDefinition = InputValueDefinition
  <$> genName
  <*> genType
  <*> H.maybe genDefaultValue
  <*> H.maybe genDirectives


genInterfaceTypeDefinition :: Monad m => Gen m InterfaceTypeDefinition
genInterfaceTypeDefinition = InterfaceTypeDefinition
  <$> genName
  <*> H.maybe genDirectives
  <*> genFieldDefinitions


genUnionTypeDefinition :: Monad m => Gen m UnionTypeDefinition
genUnionTypeDefinition = UnionTypeDefinition
  <$> genName
  <*> H.maybe genDirectives
  <*> genUnionTypes


genUnionTypes :: Monad m => Gen m UnionTypes
genUnionTypes = UnionTypes
  <$> H.nonEmpty (H.linear 1 2) genNamedType


genEnumTypeDefinition :: Monad m => Gen m EnumTypeDefinition
genEnumTypeDefinition = EnumTypeDefinition
  <$> genName
  <*> H.maybe genDirectives
  <*> genEnumValues


genEnumValues :: Monad m => Gen m EnumValues
genEnumValues = EnumValues
  <$> H.list (H.linear 0 2) genEnumValueDefinition


genEnumValueDefinition :: Monad m => Gen m EnumValueDefinition
genEnumValueDefinition = EnumValueDefinition
  <$> genName
  <*> H.maybe genDirectives


genInputObjectTypeDefinition :: Monad m => Gen m InputObjectTypeDefinition
genInputObjectTypeDefinition = InputObjectTypeDefinition
  <$> genName
  <*> H.maybe genDirectives
  <*> genInputFieldDefinitions


genInputFieldDefinitions :: Monad m => Gen m InputFieldDefinitions
genInputFieldDefinitions = InputFieldDefinitions
  <$> H.list (H.linear 0 2) genInputValueDefinition


genTypeExtensionDefinition :: Monad m => Gen m TypeExtensionDefinition
genTypeExtensionDefinition = TypeExtensionDefinition
  <$> genObjectTypeDefinition


genDirectiveDefinition :: Monad m => Gen m DirectiveDefinition
genDirectiveDefinition = DirectiveDefinition
  <$> genName
  <*> H.maybe genInputValueDefinitions
  <*> genDirectiveLocations


genDirectiveLocations :: Monad m => Gen m DirectiveLocations
genDirectiveLocations = DirectiveLocations
  <$> H.nonEmpty (H.linear 1 2) genName
