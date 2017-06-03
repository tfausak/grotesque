{-# LANGUAGE NoImplicitPrelude #-}

-- | <https://facebook.github.io/graphql/>
module Grotesque where

import Prelude
  ( Bool
  , Integer
  , Maybe
  , Rational
  , String
  )

-- * AST

-- | <https://facebook.github.io/graphql/#Name>
newtype Name = Name
  { nameValue :: String
  }

-- | <https://facebook.github.io/graphql/#Document>
newtype Document = Document
  { documentValue :: [Definition]
  }

-- | <https://facebook.github.io/graphql/#Definition>
data Definition
  = DefinitionOperation OperationDefinition
  | DefinitionFragment FragmentDefinition

-- | <https://facebook.github.io/graphql/#OperationDefinition>
data OperationDefinition = OperationDefinition
  { operationDefinitionOperationType :: OperationType
  , operationDefinitionName :: Maybe Name
  , operationDefinitionVariableDefinitions :: Maybe VariableDefinitions
  , operationDefinitionDirectives :: Maybe Directives
  , operationDefinitionSelectionSet :: SelectionSet
  }

-- | <https://facebook.github.io/graphql/#OperationType>
data OperationType
  = OperationTypeQuery
  | OperationTypeMutation
  | OperationTypeSubscription

-- | <https://facebook.github.io/graphql/#SelectionSet>
newtype SelectionSet = SelectionSet
  { selectionSetValue :: [Selection]
  }

-- | <https://facebook.github.io/graphql/#Selection>
data Selection
  = SelectionField Field
  | SelectionFragmentSpread FragmentSpread
  | SelectionInlineFragment InlineFragment

-- | <https://facebook.github.io/graphql/#Field>
data Field = Field
  { fieldAlias :: Maybe Alias
  , fieldName :: Name
  , fieldArguments :: Maybe Arguments
  , fieldDirectives :: Maybe Directives
  , fieldSelectionSet :: Maybe SelectionSet
  }

-- | <https://facebook.github.io/graphql/#Arguments>
newtype Arguments = Arguments
  { argumentsValue :: [Argument]
  }

-- | <https://facebook.github.io/graphql/#Argument>
data Argument = Argument
  { argumentName :: Name
  , argumentValue :: Value
  }

-- | <https://facebook.github.io/graphql/#Alias>
newtype Alias = Alias
  { aliasValue :: Name
  }

-- | <https://facebook.github.io/graphql/#FragmentSpread>
data FragmentSpread = FragmentSpread
  { fragmentSpreadName :: FragmentName
  , fragmentSpreadDirectives :: Maybe Directives
  }

-- | <https://facebook.github.io/graphql/#FragmentDefinition>
data FragmentDefinition = FragmentDefinition
  { fragmentName :: FragmentName
  , fragmentTypeCondition :: TypeCondition
  , fragmentDirectives :: Maybe Directives
  , fragmentSelectionSet :: SelectionSet
  }

-- | <https://facebook.github.io/graphql/#FragmentName>
newtype FragmentName = FragmentName
  { fragmentNameValue :: Name
  }

-- | <https://facebook.github.io/graphql/#TypeCondition>
newtype TypeCondition = TypeCondition
  { typeConditionValue :: NamedType
  }

-- | <https://facebook.github.io/graphql/#InlineFragment>
data InlineFragment = InlineFragment
  { inlineFragmentTypeCondition :: Maybe TypeCondition
  , inlineFragmentDirectives :: Maybe Directives
  , inlineFragmentSelectionSet :: SelectionSet
  }

-- | <https://facebook.github.io/graphql/#Value>
data Value
  = ValueVariable Variable
  | ValueInt Integer
  | ValueFloat Rational
  | ValueString String
  | ValueBoolean Bool
  | ValueNull
  | ValueEnum Name
  | ValueList [Value]
  | ValueObject [ObjectField]

-- | <https://facebook.github.io/graphql/#ObjectField>
data ObjectField = ObjectField
  { objectFieldName :: Name
  , objectFieldValue :: Value
  }

-- | <https://facebook.github.io/graphql/#Variable>
newtype Variable = Variable
  { variableValue :: Name
  }

-- | <https://facebook.github.io/graphql/#VariableDefinitions>
newtype VariableDefinitions = VariableDefinitions
  { variableDefinitionsValue :: [VariableDefinition]
  }

-- | <https://facebook.github.io/graphql/#VariableDefinition>
data VariableDefinition = VariableDefinition
  { variableDefinitionVariable :: Variable
  , variableDefinitionType :: Type
  , variableDefinitionDefaultValue :: Maybe DefaultValue
  }

-- | <https://facebook.github.io/graphql/#DefaultValue>
newtype DefaultValue = DefaultValue
  { defaultValueValue :: Value
  }

-- | <https://facebook.github.io/graphql/#Type>
data Type
  = TypeNamed NamedType
  | TypeList ListType
  | TypeNonNull NonNullType

-- | <https://facebook.github.io/graphql/#NamedType>
newtype NamedType = NamedType
  { namedTypeValue :: Name
  }

-- | <https://facebook.github.io/graphql/#ListType>
newtype ListType = ListType
  { listTypeValue :: [Type]
  }

-- | <https://facebook.github.io/graphql/#NonNullType>
data NonNullType
  = NonNullTypeNamed NamedType
  | NonNullTypeList ListType

-- | <https://facebook.github.io/graphql/#Directives>
newtype Directives = Directives
  { directivesValue :: [Directive]
  }

-- | <https://facebook.github.io/graphql/#Directive>
data Directive = Directive
  { directiveName :: Name
  , directiveArguments :: Maybe Arguments
  }
