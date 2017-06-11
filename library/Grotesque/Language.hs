{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Grotesque.Language where

import Control.DeepSeq (NFData)
import Data.List.NonEmpty (NonEmpty)
import Data.Scientific (Scientific)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | <https://facebook.github.io/graphql/#Document>
newtype Document = Document
  { documentValue :: [Definition]
  } deriving (Eq, Generic, NFData, Show)

-- | <https://facebook.github.io/graphql/#Definition>
data Definition
  = DefinitionOperation OperationDefinition
  | DefinitionFragment FragmentDefinition
  | DefinitionTypeSystem TypeSystemDefinition
  deriving (Eq, Generic, NFData, Show)

-- | <https://facebook.github.io/graphql/#OperationDefinition>
data OperationDefinition = OperationDefinition
  { operationDefinitionOperationType :: OperationType
  , operationDefinitionName :: Maybe Name
  , operationDefinitionVariableDefinitions :: Maybe VariableDefinitions
  , operationDefinitionDirectives :: Maybe Directives
  , operationDefinitionSelectionSet :: SelectionSet
  } deriving (Eq, Generic, NFData, Show)

-- | <https://facebook.github.io/graphql/#OperationType>
data OperationType
  = OperationTypeQuery
  | OperationTypeMutation
  | OperationTypeSubscription
  deriving (Bounded, Enum, Eq, Generic, NFData, Show)

-- | <https://facebook.github.io/graphql/#Name>
newtype Name = Name
  { nameValue :: Text
  } deriving (Eq, Generic, NFData, Show)

-- | <https://facebook.github.io/graphql/#VariableDefinitions>
newtype VariableDefinitions = VariableDefinitions
  { variableDefinitionsValue :: [VariableDefinition]
  } deriving (Eq, Generic, NFData, Show)

-- | <https://facebook.github.io/graphql/#VariableDefinition>
data VariableDefinition = VariableDefinition
  { variableDefinitionVariable :: Variable
  , variableDefinitionType :: Type
  , variableDefinitionDefaultValue :: Maybe DefaultValue
  } deriving (Eq, Generic, NFData, Show)

-- | <https://facebook.github.io/graphql/#Variable>
newtype Variable = Variable
  { variableValue :: Name
  } deriving (Eq, Generic, NFData, Show)

-- | <https://facebook.github.io/graphql/#Type>
data Type
  = TypeNamed NamedType
  | TypeList ListType
  | TypeNonNull NonNullType
  deriving (Eq, Generic, NFData, Show)

-- | <https://facebook.github.io/graphql/#NamedType>
newtype NamedType = NamedType
  { namedTypeValue :: Name
  } deriving (Eq, Generic, NFData, Show)

-- | <https://facebook.github.io/graphql/#ListType>
newtype ListType = ListType
  { listTypeValue :: Type
  } deriving (Eq, Generic, NFData, Show)

-- | <https://facebook.github.io/graphql/#NonNullType>
data NonNullType
  = NonNullTypeNamed NamedType
  | NonNullTypeList ListType
  deriving (Eq, Generic, NFData, Show)

-- | <https://facebook.github.io/graphql/#DefaultValue>
newtype DefaultValue = DefaultValue
  { defaultValueValue :: Value
  } deriving (Eq, Generic, NFData, Show)

-- | <https://facebook.github.io/graphql/#Value>
data Value
  = ValueVariable Variable
  | ValueInt Integer
  | ValueFloat Scientific
  | ValueString Text
  | ValueBoolean Bool
  | ValueNull
  | ValueEnum Name
  | ValueList [Value]
  | ValueObject [ObjectField]
  deriving (Eq, Generic, NFData, Show)

-- | <https://facebook.github.io/graphql/#ObjectField>
data ObjectField = ObjectField
  { objectFieldName :: Name
  , objectFieldValue :: Value
  } deriving (Eq, Generic, NFData, Show)

-- | <https://facebook.github.io/graphql/#Directives>
newtype Directives = Directives
  { directivesValue :: NonEmpty Directive
  } deriving (Eq, Generic, NFData, Show)

-- | <https://facebook.github.io/graphql/#Directive>
data Directive = Directive
  { directiveName :: Name
  , directiveArguments :: Maybe Arguments
  } deriving (Eq, Generic, NFData, Show)

-- | <https://facebook.github.io/graphql/#Arguments>
newtype Arguments = Arguments
  { argumentsValue :: [Argument]
  } deriving (Eq, Generic, NFData, Show)

-- | <https://facebook.github.io/graphql/#Argument>
data Argument = Argument
  { argumentName :: Name
  , argumentValue :: Value
  } deriving (Eq, Generic, NFData, Show)

-- | <https://facebook.github.io/graphql/#SelectionSet>
newtype SelectionSet = SelectionSet
  { selectionSetValue :: [Selection]
  } deriving (Eq, Generic, NFData, Show)

-- | <https://facebook.github.io/graphql/#Selection>
data Selection
  = SelectionField Field
  | SelectionFragmentSpread FragmentSpread
  | SelectionInlineFragment InlineFragment
  deriving (Eq, Generic, NFData, Show)

-- | <https://facebook.github.io/graphql/#Field>
data Field = Field
  { fieldAlias :: Maybe Alias
  , fieldName :: Name
  , fieldArguments :: Maybe Arguments
  , fieldDirectives :: Maybe Directives
  , fieldSelectionSet :: Maybe SelectionSet
  } deriving (Eq, Generic, NFData, Show)

-- | <https://facebook.github.io/graphql/#Alias>
newtype Alias = Alias
  { aliasValue :: Name
  } deriving (Eq, Generic, NFData, Show)

-- | <https://facebook.github.io/graphql/#FragmentSpread>
data FragmentSpread = FragmentSpread
  { fragmentSpreadName :: FragmentName
  , fragmentSpreadDirectives :: Maybe Directives
  } deriving (Eq, Generic, NFData, Show)

-- | <https://facebook.github.io/graphql/#FragmentName>
newtype FragmentName = FragmentName
  { fragmentNameValue :: Name
  } deriving (Eq, Generic, NFData, Show)

-- | <https://facebook.github.io/graphql/#InlineFragment>
data InlineFragment = InlineFragment
  { inlineFragmentTypeCondition :: Maybe TypeCondition
  , inlineFragmentDirectives :: Maybe Directives
  , inlineFragmentSelectionSet :: SelectionSet
  } deriving (Eq, Generic, NFData, Show)

-- | <https://facebook.github.io/graphql/#TypeCondition>
newtype TypeCondition = TypeCondition
  { typeConditionValue :: NamedType
  } deriving (Eq, Generic, NFData, Show)

-- | <https://facebook.github.io/graphql/#FragmentDefinition>
data FragmentDefinition = FragmentDefinition
  { fragmentName :: FragmentName
  , fragmentTypeCondition :: TypeCondition
  , fragmentDirectives :: Maybe Directives
  , fragmentSelectionSet :: SelectionSet
  } deriving (Eq, Generic, NFData, Show)

data TypeSystemDefinition
  = TypeSystemDefinitionSchema SchemaDefinition
  | TypeSystemDefinitionType TypeDefinition
  | TypeSystemDefinitionTypeExtension TypeExtensionDefinition
  | TypeSystemDefinitionDirective DirectiveDefinition
  deriving (Eq, Generic, NFData, Show)

data SchemaDefinition = SchemaDefinition
  { schemaDefinitionDirectives :: Maybe Directives
  , schemaDefinitionOperationTypes :: OperationTypeDefinitions
  } deriving (Eq, Generic, NFData, Show)

newtype OperationTypeDefinitions = OperationTypeDefinitions
  { operationTypeDefinitionsValue :: [OperationTypeDefinition]
  } deriving (Eq, Generic, NFData, Show)

data OperationTypeDefinition = OperationTypeDefinition
  { operationTypeDefinitionOperation :: OperationType
  , operationTypeDefinitionType :: NamedType
  } deriving (Eq, Generic, NFData, Show)

data TypeDefinition
  = TypeDefinitionScalar ScalarTypeDefinition
  | TypeDefinitionObject ObjectTypeDefinition
  | TypeDefinitionInterface InterfaceTypeDefinition
  | TypeDefinitionUnion UnionTypeDefinition
  | TypeDefinitionEnum EnumTypeDefinition
  | TypeDefinitionInputObject InputObjectTypeDefinition
  deriving (Eq, Generic, NFData, Show)

data ScalarTypeDefinition = ScalarTypeDefinition
  { scalarTypeDefinitionName :: Name
  , scalarTypeDefinitionDirectives :: Maybe Directives
  } deriving (Eq, Generic, NFData, Show)

data ObjectTypeDefinition = ObjectTypeDefinition
  { objectTypeDefinitionName :: Name
  , objectTypeDefinitionInterfaces :: Maybe Interfaces
  , objectTypeDefinitionDirectives :: Maybe Directives
  , objectTypeDefinitionFields :: FieldDefinitions
  } deriving (Eq, Generic, NFData, Show)

newtype Interfaces = Interfaces
  { interfacesValue :: NonEmpty NamedType
  } deriving (Eq, Generic, NFData, Show)

newtype FieldDefinitions = FieldDefinitions
  { fieldDefinitionsValue :: [FieldDefinition]
  } deriving (Eq, Generic, NFData, Show)

data FieldDefinition = FieldDefinition
  { fieldDefinitionName :: Name
  , fieldDefinitionArguments :: Maybe InputValueDefinitions
  , fieldDefinitionType :: Type
  , fieldDefinitionDirectives :: Maybe Directives
  } deriving (Eq, Generic, NFData, Show)

newtype InputValueDefinitions = InputValueDefinitions
  { inputValueDefinitionsValue :: [InputValueDefinition]
  } deriving (Eq, Generic, NFData, Show)

data InputValueDefinition = InputValueDefinition
  { inputValueDefinitionName :: Name
  , inputValueDefinitionType :: Type
  , inputValueDefinitionDefaultValue :: Maybe DefaultValue
  , inputValueDefinitionDirectives :: Maybe Directives
  } deriving (Eq, Generic, NFData, Show)

data InterfaceTypeDefinition = InterfaceTypeDefinition
  { interfaceTypeDefinitionName :: Name
  , interfaceTypeDefinitionDirectives :: Maybe Directives
  , interfaceTypeDefinitionFields :: FieldDefinitions
  } deriving (Eq, Generic, NFData, Show)

data UnionTypeDefinition = UnionTypeDefinition
  { unionTypeDefinitionName :: Name
  , unionTypeDefinitionDirectives :: Maybe Directives
  , unionTypeDefinitionTypes :: UnionTypes
  } deriving (Eq, Generic, NFData, Show)

newtype UnionTypes = UnionTypes
  { unionTypesValue :: NonEmpty NamedType
  } deriving (Eq, Generic, NFData, Show)

data EnumTypeDefinition = EnumTypeDefinition
  { enumTypeDefinitionName :: Name
  , enumTypeDefinitionDirectives :: Maybe Directives
  , enumTypeDefinitionValues :: EnumValues
  } deriving (Eq, Generic, NFData, Show)

newtype EnumValues = EnumValues
  { enumValuesValue :: [EnumValueDefinition]
  } deriving (Eq, Generic, NFData, Show)

data EnumValueDefinition = EnumValueDefinition
  { enumValueDefinitionName :: Name
  , enumValueDefinitionDirectives :: Maybe Directives
  } deriving (Eq, Generic, NFData, Show)

data InputObjectTypeDefinition = InputObjectTypeDefinition
  { inputObjectTypeDefinitionName :: Name
  , inputObjectTypeDefinitionDirectives :: Maybe Directives
  , inputObjectTypeDefinitionFields :: InputFieldDefinitions
  } deriving (Eq, Generic, NFData, Show)

newtype InputFieldDefinitions = InputFieldDefinitions
  { inputFieldDefinitionsValue :: [InputValueDefinition]
  } deriving (Eq, Generic, NFData, Show)

newtype TypeExtensionDefinition = TypeExtensionDefinition
  { typeExtensionDefinitionValue :: ObjectTypeDefinition
  } deriving (Eq, Generic, NFData, Show)

data DirectiveDefinition = DirectiveDefinition
  { directiveDefinitionName :: Name
  , directiveDefinitionArguments :: Maybe InputValueDefinitions
  , directiveDefinitionLocations :: DirectiveLocations
  } deriving (Eq, Generic, NFData, Show)

newtype DirectiveLocations = DirectiveLocations
  { directiveLocationsValue :: NonEmpty Name
  } deriving (Eq, Generic, NFData, Show)
