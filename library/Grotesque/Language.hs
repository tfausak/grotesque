module Grotesque.Language where

import Data.Scientific (Scientific)


-- | <https://facebook.github.io/graphql/#Document>
newtype Document = Document
  { documentValue :: [Definition]
  } deriving (Eq, Show)


-- | <https://facebook.github.io/graphql/#Definition>
data Definition
  = DefinitionOperation OperationDefinition
  | DefinitionFragment FragmentDefinition
  deriving (Eq, Show)


-- | <https://facebook.github.io/graphql/#OperationDefinition>
data OperationDefinition = OperationDefinition
  { operationDefinitionOperationType :: OperationType
  , operationDefinitionName :: Maybe Name
  , operationDefinitionVariableDefinitions :: Maybe VariableDefinitions
  , operationDefinitionDirectives :: Maybe Directives
  , operationDefinitionSelectionSet :: SelectionSet
  } deriving (Eq, Show)


-- | <https://facebook.github.io/graphql/#OperationType>
data OperationType
  = OperationTypeQuery
  | OperationTypeMutation
  | OperationTypeSubscription
  deriving (Eq, Show)


-- | <https://facebook.github.io/graphql/#Name>
newtype Name = Name
  { nameValue :: String
  } deriving (Eq, Show)


-- | <https://facebook.github.io/graphql/#VariableDefinitions>
newtype VariableDefinitions = VariableDefinitions
  { variableDefinitionsValue :: [VariableDefinition]
  } deriving (Eq, Show)


-- | <https://facebook.github.io/graphql/#VariableDefinition>
data VariableDefinition = VariableDefinition
  { variableDefinitionVariable :: Variable
  , variableDefinitionType :: Type
  , variableDefinitionDefaultValue :: Maybe DefaultValue
  } deriving (Eq, Show)


-- | <https://facebook.github.io/graphql/#Variable>
newtype Variable = Variable
  { variableValue :: Name
  } deriving (Eq, Show)


-- | <https://facebook.github.io/graphql/#Type>
data Type
  = TypeNamed NamedType
  | TypeList ListType
  | TypeNonNull NonNullType
  deriving (Eq, Show)


-- | <https://facebook.github.io/graphql/#NamedType>
newtype NamedType = NamedType
  { namedTypeValue :: Name
  } deriving (Eq, Show)


-- | <https://facebook.github.io/graphql/#ListType>
newtype ListType = ListType
  { listTypeValue :: Type
  } deriving (Eq, Show)


-- | <https://facebook.github.io/graphql/#NonNullType>
data NonNullType
  = NonNullTypeNamed NamedType
  | NonNullTypeList ListType
  deriving (Eq, Show)


-- | <https://facebook.github.io/graphql/#DefaultValue>
newtype DefaultValue = DefaultValue
  { defaultValueValue :: Value
  } deriving (Eq, Show)


-- | <https://facebook.github.io/graphql/#Value>
data Value
  = ValueVariable Variable
  | ValueInt Integer
  | ValueFloat Scientific
  | ValueString String
  | ValueBoolean Bool
  | ValueNull
  | ValueEnum Name
  | ValueList [Value]
  | ValueObject [ObjectField]
  deriving (Eq, Show)


-- | <https://facebook.github.io/graphql/#ObjectField>
data ObjectField = ObjectField
  { objectFieldName :: Name
  , objectFieldValue :: Value
  } deriving (Eq, Show)


-- | <https://facebook.github.io/graphql/#Directives>
newtype Directives = Directives
  { directivesValue :: [Directive]
  } deriving (Eq, Show)


-- | <https://facebook.github.io/graphql/#Directive>
data Directive = Directive
  { directiveName :: Name
  , directiveArguments :: Maybe Arguments
  } deriving (Eq, Show)


-- | <https://facebook.github.io/graphql/#Arguments>
newtype Arguments = Arguments
  { argumentsValue :: [Argument]
  } deriving (Eq, Show)


-- | <https://facebook.github.io/graphql/#Argument>
data Argument = Argument
  { argumentName :: Name
  , argumentValue :: Value
  } deriving (Eq, Show)


-- | <https://facebook.github.io/graphql/#SelectionSet>
newtype SelectionSet = SelectionSet
  { selectionSetValue :: [Selection]
  } deriving (Eq, Show)


-- | <https://facebook.github.io/graphql/#Selection>
data Selection
  = SelectionField Field
  | SelectionFragmentSpread FragmentSpread
  | SelectionInlineFragment InlineFragment
  deriving (Eq, Show)


-- | <https://facebook.github.io/graphql/#Field>
data Field = Field
  { fieldAlias :: Maybe Alias
  , fieldName :: Name
  , fieldArguments :: Maybe Arguments
  , fieldDirectives :: Maybe Directives
  , fieldSelectionSet :: Maybe SelectionSet
  } deriving (Eq, Show)


-- | <https://facebook.github.io/graphql/#Alias>
newtype Alias = Alias
  { aliasValue :: Name
  } deriving (Eq, Show)


-- | <https://facebook.github.io/graphql/#FragmentSpread>
data FragmentSpread = FragmentSpread
  { fragmentSpreadName :: FragmentName
  , fragmentSpreadDirectives :: Maybe Directives
  } deriving (Eq, Show)


-- | <https://facebook.github.io/graphql/#FragmentName>
newtype FragmentName = FragmentName
  { fragmentNameValue :: Name
  } deriving (Eq, Show)


-- | <https://facebook.github.io/graphql/#InlineFragment>
data InlineFragment = InlineFragment
  { inlineFragmentTypeCondition :: Maybe TypeCondition
  , inlineFragmentDirectives :: Maybe Directives
  , inlineFragmentSelectionSet :: SelectionSet
  } deriving (Eq, Show)


-- | <https://facebook.github.io/graphql/#TypeCondition>
newtype TypeCondition = TypeCondition
  { typeConditionValue :: NamedType
  } deriving (Eq, Show)


-- | <https://facebook.github.io/graphql/#FragmentDefinition>
data FragmentDefinition = FragmentDefinition
  { fragmentName :: FragmentName
  , fragmentTypeCondition :: TypeCondition
  , fragmentDirectives :: Maybe Directives
  , fragmentSelectionSet :: SelectionSet
  } deriving (Eq, Show)
