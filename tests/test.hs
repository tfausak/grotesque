{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text
import Grotesque
import Hedgehog
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Heredoc
import Grotesque.Generator


main :: IO ()
main = do
  hspec . parallel $ do

    context "parser" $ do

      itParses "an empty document"
        " "
        Document { documentValue = [] }

      itParses "an empty query shorthand"
        " { } "
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [] } } ] }

      itParses "a field name"
        " { f } "
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [ SelectionField Field { fieldAlias = Nothing , fieldName = Name { nameValue = "f" } , fieldArguments = Nothing , fieldDirectives = Nothing , fieldSelectionSet = Nothing } ] } } ] }

      itParses "a field alias"
        " { a : f } "
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [ SelectionField Field { fieldAlias = Just Alias { aliasValue = Name { nameValue = "a" } } , fieldName = Name { nameValue = "f" } , fieldArguments = Nothing , fieldDirectives = Nothing , fieldSelectionSet = Nothing } ] } } ] }

      itParses "empty field arguments"
        " { f ( ) } "
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [ SelectionField Field { fieldAlias = Nothing , fieldName = Name { nameValue = "f" } , fieldArguments = Just Arguments { argumentsValue = [] } , fieldDirectives = Nothing , fieldSelectionSet = Nothing } ] } } ] }

      itParses "a null field argument"
        " { f ( a : null ) } "
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [ SelectionField Field { fieldAlias = Nothing , fieldName = Name { nameValue = "f" } , fieldArguments = Just Arguments { argumentsValue = [ Argument { argumentName = Name { nameValue = "a" } , argumentValue = ValueNull } ] } , fieldDirectives = Nothing , fieldSelectionSet = Nothing } ] } } ] }

      itParses "a list field argument"
        " { f ( a : [ ] ) } "
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [ SelectionField Field { fieldAlias = Nothing , fieldName = Name { nameValue = "f" } , fieldArguments = Just Arguments { argumentsValue = [ Argument { argumentName = Name { nameValue = "a" } , argumentValue = ValueList [] } ] } , fieldDirectives = Nothing , fieldSelectionSet = Nothing } ] } } ] }

      itParses "an enum field argument"
        " { f ( a : e ) } "
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [ SelectionField Field { fieldAlias = Nothing , fieldName = Name { nameValue = "f" } , fieldArguments = Just Arguments { argumentsValue = [ Argument { argumentName = Name { nameValue = "a" } , argumentValue = ValueEnum Name { nameValue = "e" } } ] } , fieldDirectives = Nothing , fieldSelectionSet = Nothing } ] } } ] }

      itParses "a boolean field argument"
        " { f ( a : false ) } "
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [ SelectionField Field { fieldAlias = Nothing , fieldName = Name { nameValue = "f" } , fieldArguments = Just Arguments { argumentsValue = [ Argument { argumentName = Name { nameValue = "a" } , argumentValue = ValueBoolean False } ] } , fieldDirectives = Nothing , fieldSelectionSet = Nothing } ] } } ] }

      itParses "a variable field argument"
        " { f ( a : $ v ) } "
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [ SelectionField Field { fieldAlias = Nothing , fieldName = Name { nameValue = "f" } , fieldArguments = Just Arguments { argumentsValue = [ Argument { argumentName = Name { nameValue = "a" } , argumentValue = ValueVariable Variable { variableValue = Name { nameValue = "v" } } } ] } , fieldDirectives = Nothing , fieldSelectionSet = Nothing } ] } } ] }

      itParses "an int field argument"
        " { f ( a : 0 ) } "
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [ SelectionField Field { fieldAlias = Nothing , fieldName = Name { nameValue = "f" } , fieldArguments = Just Arguments { argumentsValue = [ Argument { argumentName = Name { nameValue = "a" } , argumentValue = ValueInt 0 } ] } , fieldDirectives = Nothing , fieldSelectionSet = Nothing } ] } } ] }

      itParses "a float field argument"
        " { f ( a : 1.2 ) } "
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [ SelectionField Field { fieldAlias = Nothing , fieldName = Name { nameValue = "f" } , fieldArguments = Just Arguments { argumentsValue = [ Argument { argumentName = Name { nameValue = "a" } , argumentValue = ValueFloat 1.2 } ] } , fieldDirectives = Nothing , fieldSelectionSet = Nothing } ] } } ] }

      itParses "a string field argument"
        " { f ( a : \"\" ) } "
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [ SelectionField Field { fieldAlias = Nothing , fieldName = Name { nameValue = "f" } , fieldArguments = Just Arguments { argumentsValue = [ Argument { argumentName = Name { nameValue = "a" } , argumentValue = ValueString "" } ] } , fieldDirectives = Nothing , fieldSelectionSet = Nothing } ] } } ] }

      itParses "an object field argument"
        " { f ( a : { } ) } "
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [ SelectionField Field { fieldAlias = Nothing , fieldName = Name { nameValue = "f" } , fieldArguments = Just Arguments { argumentsValue = [ Argument { argumentName = Name { nameValue = "a" } , argumentValue = ValueObject [] } ] } , fieldDirectives = Nothing , fieldSelectionSet = Nothing } ] } } ] }

      itParses "an object field argument with a field"
        " { f ( a : { k : null } ) } "
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [ SelectionField Field { fieldAlias = Nothing , fieldName = Name { nameValue = "f" } , fieldArguments = Just Arguments { argumentsValue = [ Argument { argumentName = Name { nameValue = "a" } , argumentValue = ValueObject [ ObjectField { objectFieldName = Name { nameValue = "k" } , objectFieldValue = ValueNull } ] } ] } , fieldDirectives = Nothing , fieldSelectionSet = Nothing } ] } } ] }

      itParses "a query"
        " query { } "
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [] } } ] }

      itParses "a mutation"
        " mutation { } "
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeMutation , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [] } } ] }

      itParses "a subscription"
        " subscription { } "
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeSubscription , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [] } } ] }

      itParses "an empty variable definition"
        " query ( ) { } "
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Just VariableDefinitions { variableDefinitionsValue = [] } , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [] } } ] }

      itParses "a variable definition with a named type"
        " query ( $ v : t ) { } "
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Just VariableDefinitions { variableDefinitionsValue = [ VariableDefinition { variableDefinitionVariable = Variable { variableValue = Name { nameValue = "v" } } , variableDefinitionType = TypeNamed NamedType { namedTypeValue = Name { nameValue = "t" } } , variableDefinitionDefaultValue = Nothing } ] } , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [] } } ] }

      itParses "a variable definition with a list type"
        " query ( $ v : [ t ] ) { } "
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Just VariableDefinitions { variableDefinitionsValue = [ VariableDefinition { variableDefinitionVariable = Variable { variableValue = Name { nameValue = "v" } } , variableDefinitionType = TypeList ListType { listTypeValue = TypeNamed NamedType { namedTypeValue = Name { nameValue = "t" } } } , variableDefinitionDefaultValue = Nothing } ] } , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [] } } ] }

      itParses "a variable definition with a non-null named type"
        " query ( $ v : t ! ) { } "
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Just VariableDefinitions { variableDefinitionsValue = [ VariableDefinition { variableDefinitionVariable = Variable { variableValue = Name { nameValue = "v" } } , variableDefinitionType = TypeNonNull (NonNullTypeNamed NamedType { namedTypeValue = Name { nameValue = "t" } }) , variableDefinitionDefaultValue = Nothing } ] } , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [] } } ] }

      itParses "a variable definition with a non-null list type"
        " query ( $ v : [ t ] ! ) { } "
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Just VariableDefinitions { variableDefinitionsValue = [ VariableDefinition { variableDefinitionVariable = Variable { variableValue = Name { nameValue = "v" } } , variableDefinitionType = TypeNonNull (NonNullTypeList ListType { listTypeValue = TypeNamed NamedType { namedTypeValue = Name { nameValue = "t" } } }) , variableDefinitionDefaultValue = Nothing } ] } , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [] } } ] }

      itParses "a variable definition with a default value"
        " query ( $ v : t = null ) { } "
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Just VariableDefinitions { variableDefinitionsValue = [ VariableDefinition { variableDefinitionVariable = Variable { variableValue = Name { nameValue = "v" } } , variableDefinitionType = TypeNamed NamedType { namedTypeValue = Name { nameValue = "t" } } , variableDefinitionDefaultValue = Just DefaultValue { defaultValueValue = ValueNull } } ] } , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [] } } ] }

      itParses "a directive"
        " query @ d { } "
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Just Directives { directivesValue = [ Directive { directiveName = Name { nameValue = "d" } , directiveArguments = Nothing } ] } , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [] } } ] }

      itParses "a directive with empty arguments"
        " query @ d ( ) { } "
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Just Directives { directivesValue = [ Directive { directiveName = Name { nameValue = "d" } , directiveArguments = Just Arguments { argumentsValue = [] } } ] } , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [] } } ] }

      itParses "a directive with an argument"
        " query @ d ( a : null ) { } "
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Just Directives { directivesValue = [ Directive { directiveName = Name { nameValue = "d" } , directiveArguments = Just Arguments { argumentsValue = [ Argument { argumentName = Name { nameValue = "a" } , argumentValue = ValueNull } ] } } ] } , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [] } } ] }

      itParses "a fragment spread"
        " { ... s } "
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [ SelectionFragmentSpread FragmentSpread { fragmentSpreadName = FragmentName { fragmentNameValue = Name { nameValue = "s" } } , fragmentSpreadDirectives = Nothing } ] } } ] }

      itParses "a fragment spread with a directive"
        " { ... s @ d } "
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [ SelectionFragmentSpread FragmentSpread { fragmentSpreadName = FragmentName { fragmentNameValue = Name { nameValue = "s" } } , fragmentSpreadDirectives = Just Directives { directivesValue = [ Directive { directiveName = Name { nameValue = "d" } , directiveArguments = Nothing } ] } } ] } } ] }

      itParses "an inline fragment"
        " { ... { } } "
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [ SelectionInlineFragment InlineFragment { inlineFragmentTypeCondition = Nothing , inlineFragmentDirectives = Nothing , inlineFragmentSelectionSet = SelectionSet { selectionSetValue = [] } } ] } } ] }

      itParses "an inline fragment with a type condition"
        " { ... on t { } } "
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [ SelectionInlineFragment InlineFragment { inlineFragmentTypeCondition = Just TypeCondition { typeConditionValue = NamedType { namedTypeValue = Name { nameValue = "t" } } } , inlineFragmentDirectives = Nothing , inlineFragmentSelectionSet = SelectionSet { selectionSetValue = [] } } ] } } ] }

      itParses "an inline fragment with a directive"
        " { ... @ d { } } "
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [ SelectionInlineFragment InlineFragment { inlineFragmentTypeCondition = Nothing , inlineFragmentDirectives = Just Directives { directivesValue = [ Directive { directiveName = Name { nameValue = "d" } , directiveArguments = Nothing } ] } , inlineFragmentSelectionSet = SelectionSet { selectionSetValue = [] } } ] } } ] }

      itParses "a fragment definition"
        " fragment f on t { } "
        Document { documentValue = [ DefinitionFragment FragmentDefinition { fragmentName = FragmentName { fragmentNameValue = Name { nameValue = "f" } } , fragmentTypeCondition = TypeCondition { typeConditionValue = NamedType { namedTypeValue = Name { nameValue = "t" } } } , fragmentDirectives = Nothing , fragmentSelectionSet = SelectionSet { selectionSetValue = [] } } ] }

      itParses "a fragment definition with a directive"
        " fragment f on t @ d { } "
        Document { documentValue = [ DefinitionFragment FragmentDefinition { fragmentName = FragmentName { fragmentNameValue = Name { nameValue = "f" } } , fragmentTypeCondition = TypeCondition { typeConditionValue = NamedType { namedTypeValue = Name { nameValue = "t" } } } , fragmentDirectives = Just Directives { directivesValue = [ Directive { directiveName = Name { nameValue = "d" } , directiveArguments = Nothing } ] } , fragmentSelectionSet = SelectionSet { selectionSetValue = [] } } ] }

      itParses "the query kitchen sink"
        [here|
          # https://raw.githubusercontent.com/graphql/graphql-js/0de76ca/src/language/__tests__/kitchen-sink.graphql
          query queryName($foo: ComplexType, $site: Site = MOBILE) {
            whoever123is: node(id: [123, 456]) {
              id ,
              ... on User @defer {
                field2 {
                  id ,
                  alias: field1(first:10, after:$foo,) @include(if: $foo) {
                    id,
                    ...frag
                  }
                }
              }
              ... @skip(unless: $foo) {
                id
              }
              ... {
                id
              }
            }
          }

          mutation likeStory {
            like(story: 123) @defer {
              story {
                id
              }
            }
          }

          subscription StoryLikeSubscription($input: StoryLikeSubscribeInput) {
            storyLikeSubscribe(input: $input) {
              story {
                likers {
                  count
                }
                likeSentence {
                  text
                }
              }
            }
          }

          fragment frag on Friend {
            foo(size: $size, bar: $b, obj: {key: "value"})
          }

          {
            unnamed(truthy: true, falsey: false, nullish: null),
            query
          }
        |]
        Document {documentValue = [DefinitionOperation (OperationDefinition {operationDefinitionOperationType = OperationTypeQuery, operationDefinitionName = Just (Name {nameValue = "queryName"}), operationDefinitionVariableDefinitions = Just (VariableDefinitions {variableDefinitionsValue = [VariableDefinition {variableDefinitionVariable = Variable {variableValue = Name {nameValue = "foo"}}, variableDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "ComplexType"}}), variableDefinitionDefaultValue = Nothing},VariableDefinition {variableDefinitionVariable = Variable {variableValue = Name {nameValue = "site"}}, variableDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "Site"}}), variableDefinitionDefaultValue = Just (DefaultValue {defaultValueValue = ValueEnum (Name {nameValue = "MOBILE"})})}]}), operationDefinitionDirectives = Nothing, operationDefinitionSelectionSet = SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Just (Alias {aliasValue = Name {nameValue = "whoever123is"}}), fieldName = Name {nameValue = "node"}, fieldArguments = Just (Arguments {argumentsValue = [Argument {argumentName = Name {nameValue = "id"}, argumentValue = ValueList [ValueInt 123,ValueInt 456]}]}), fieldDirectives = Nothing, fieldSelectionSet = Just (SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "id"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Nothing}),SelectionInlineFragment (InlineFragment {inlineFragmentTypeCondition = Just (TypeCondition {typeConditionValue = NamedType {namedTypeValue = Name {nameValue = "User"}}}), inlineFragmentDirectives = Just (Directives {directivesValue = [Directive {directiveName = Name {nameValue = "defer"}, directiveArguments = Nothing}]}), inlineFragmentSelectionSet = SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "field2"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Just (SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "id"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Nothing}),SelectionField (Field {fieldAlias = Just (Alias {aliasValue = Name {nameValue = "alias"}}), fieldName = Name {nameValue = "field1"}, fieldArguments = Just (Arguments {argumentsValue = [Argument {argumentName = Name {nameValue = "first"}, argumentValue = ValueInt 10},Argument {argumentName = Name {nameValue = "after"}, argumentValue = ValueVariable (Variable {variableValue = Name {nameValue = "foo"}})}]}), fieldDirectives = Just (Directives {directivesValue = [Directive {directiveName = Name {nameValue = "include"}, directiveArguments = Just (Arguments {argumentsValue = [Argument {argumentName = Name {nameValue = "if"}, argumentValue = ValueVariable (Variable {variableValue = Name {nameValue = "foo"}})}]})}]}), fieldSelectionSet = Just (SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "id"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Nothing}),SelectionFragmentSpread (FragmentSpread {fragmentSpreadName = FragmentName {fragmentNameValue = Name {nameValue = "frag"}}, fragmentSpreadDirectives = Nothing})]})})]})})]}}),SelectionInlineFragment (InlineFragment {inlineFragmentTypeCondition = Nothing, inlineFragmentDirectives = Just (Directives {directivesValue = [Directive {directiveName = Name {nameValue = "skip"}, directiveArguments = Just (Arguments {argumentsValue = [Argument {argumentName = Name {nameValue = "unless"}, argumentValue = ValueVariable (Variable {variableValue = Name {nameValue = "foo"}})}]})}]}), inlineFragmentSelectionSet = SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "id"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Nothing})]}}),SelectionInlineFragment (InlineFragment {inlineFragmentTypeCondition = Nothing, inlineFragmentDirectives = Nothing, inlineFragmentSelectionSet = SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "id"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Nothing})]}})]})})]}}),DefinitionOperation (OperationDefinition {operationDefinitionOperationType = OperationTypeMutation, operationDefinitionName = Just (Name {nameValue = "likeStory"}), operationDefinitionVariableDefinitions = Nothing, operationDefinitionDirectives = Nothing, operationDefinitionSelectionSet = SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "like"}, fieldArguments = Just (Arguments {argumentsValue = [Argument {argumentName = Name {nameValue = "story"}, argumentValue = ValueInt 123}]}), fieldDirectives = Just (Directives {directivesValue = [Directive {directiveName = Name {nameValue = "defer"}, directiveArguments = Nothing}]}), fieldSelectionSet = Just (SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "story"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Just (SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "id"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Nothing})]})})]})})]}}),DefinitionOperation (OperationDefinition {operationDefinitionOperationType = OperationTypeSubscription, operationDefinitionName = Just (Name {nameValue = "StoryLikeSubscription"}), operationDefinitionVariableDefinitions = Just (VariableDefinitions {variableDefinitionsValue = [VariableDefinition {variableDefinitionVariable = Variable {variableValue = Name {nameValue = "input"}}, variableDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "StoryLikeSubscribeInput"}}), variableDefinitionDefaultValue = Nothing}]}), operationDefinitionDirectives = Nothing, operationDefinitionSelectionSet = SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "storyLikeSubscribe"}, fieldArguments = Just (Arguments {argumentsValue = [Argument {argumentName = Name {nameValue = "input"}, argumentValue = ValueVariable (Variable {variableValue = Name {nameValue = "input"}})}]}), fieldDirectives = Nothing, fieldSelectionSet = Just (SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "story"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Just (SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "likers"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Just (SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "count"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Nothing})]})}),SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "likeSentence"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Just (SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "text"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Nothing})]})})]})})]})})]}}),DefinitionFragment (FragmentDefinition {fragmentName = FragmentName {fragmentNameValue = Name {nameValue = "frag"}}, fragmentTypeCondition = TypeCondition {typeConditionValue = NamedType {namedTypeValue = Name {nameValue = "Friend"}}}, fragmentDirectives = Nothing, fragmentSelectionSet = SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "foo"}, fieldArguments = Just (Arguments {argumentsValue = [Argument {argumentName = Name {nameValue = "size"}, argumentValue = ValueVariable (Variable {variableValue = Name {nameValue = "size"}})},Argument {argumentName = Name {nameValue = "bar"}, argumentValue = ValueVariable (Variable {variableValue = Name {nameValue = "b"}})},Argument {argumentName = Name {nameValue = "obj"}, argumentValue = ValueObject [ObjectField {objectFieldName = Name {nameValue = "key"}, objectFieldValue = ValueString "value"}]}]}), fieldDirectives = Nothing, fieldSelectionSet = Nothing})]}}),DefinitionOperation (OperationDefinition {operationDefinitionOperationType = OperationTypeQuery, operationDefinitionName = Nothing, operationDefinitionVariableDefinitions = Nothing, operationDefinitionDirectives = Nothing, operationDefinitionSelectionSet = SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "unnamed"}, fieldArguments = Just (Arguments {argumentsValue = [Argument {argumentName = Name {nameValue = "truthy"}, argumentValue = ValueBoolean True},Argument {argumentName = Name {nameValue = "falsey"}, argumentValue = ValueBoolean False},Argument {argumentName = Name {nameValue = "nullish"}, argumentValue = ValueNull}]}), fieldDirectives = Nothing, fieldSelectionSet = Nothing}),SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "query"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Nothing})]}})]}

      itParses "a schema"
        " schema { } "
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionSchema (SchemaDefinition {schemaDefinitionDirectives = Nothing, schemaDefinitionOperationTypes = OperationTypeDefinitions {operationTypeDefinitionsValue = []}}))]}

      itParses "a schema with a directive"
        " schema @ d { } "
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionSchema (SchemaDefinition {schemaDefinitionDirectives = Just (Directives {directivesValue = [Directive {directiveName = Name {nameValue = "d"}, directiveArguments = Nothing}]}), schemaDefinitionOperationTypes = OperationTypeDefinitions {operationTypeDefinitionsValue = []}}))]}

      itParses "a schema with an operation type"
        " schema { query : t } "
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionSchema (SchemaDefinition {schemaDefinitionDirectives = Nothing, schemaDefinitionOperationTypes = OperationTypeDefinitions {operationTypeDefinitionsValue = [OperationTypeDefinition {operationTypeDefinitionOperation = OperationTypeQuery, operationTypeDefinitionType = NamedType {namedTypeValue = Name {nameValue = "t"}}}]}}))]}

      itParses "a scalar"
        " scalar t "
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionScalar (ScalarTypeDefinition {scalarTypeDefinitionName = Name {nameValue = "t"}, scalarTypeDefinitionDirectives = Nothing})))]}

      itParses "a scalar with a directive"
        " scalar t @ d "
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionScalar (ScalarTypeDefinition {scalarTypeDefinitionName = Name {nameValue = "t"}, scalarTypeDefinitionDirectives = Just (Directives {directivesValue = [Directive {directiveName = Name {nameValue = "d"}, directiveArguments = Nothing}]})})))]}

      itParses "an object type definition"
        " type t { } "
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionObject (ObjectTypeDefinition {objectTypeDefinitionName = Name {nameValue = "t"}, objectTypeDefinitionInterfaces = Nothing, objectTypeDefinitionDirectives = Nothing, objectTypeDefinitionFields = FieldDefinitions {fieldDefinitionsValue = []}})))]}

      itParses "an object type definition with an interface"
        " type t implements i { } "
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionObject (ObjectTypeDefinition {objectTypeDefinitionName = Name {nameValue = "t"}, objectTypeDefinitionInterfaces = Just (Interfaces {interfacesValue = [NamedType {namedTypeValue = Name {nameValue = "i"}}]}), objectTypeDefinitionDirectives = Nothing, objectTypeDefinitionFields = FieldDefinitions {fieldDefinitionsValue = []}})))]}

      itParses "an object type definition with a directive"
        " type t @ d { } "
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionObject (ObjectTypeDefinition {objectTypeDefinitionName = Name {nameValue = "t"}, objectTypeDefinitionInterfaces = Nothing, objectTypeDefinitionDirectives = Just (Directives {directivesValue = [Directive {directiveName = Name {nameValue = "d"}, directiveArguments = Nothing}]}), objectTypeDefinitionFields = FieldDefinitions {fieldDefinitionsValue = []}})))]}

      itParses "an object type definition with a field definition"
        " type t { f : s } "
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionObject (ObjectTypeDefinition {objectTypeDefinitionName = Name {nameValue = "t"}, objectTypeDefinitionInterfaces = Nothing, objectTypeDefinitionDirectives = Nothing, objectTypeDefinitionFields = FieldDefinitions {fieldDefinitionsValue = [FieldDefinition {fieldDefinitionName = Name {nameValue = "f"}, fieldDefinitionArguments = Nothing, fieldDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "s"}}), fieldDefinitionDirectives = Nothing}]}})))]}

      itParses "an object type definition with a field definition with empty arguments"
        " type t { f ( ) : s } "
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionObject (ObjectTypeDefinition {objectTypeDefinitionName = Name {nameValue = "t"}, objectTypeDefinitionInterfaces = Nothing, objectTypeDefinitionDirectives = Nothing, objectTypeDefinitionFields = FieldDefinitions {fieldDefinitionsValue = [FieldDefinition {fieldDefinitionName = Name {nameValue = "f"}, fieldDefinitionArguments = Just (InputValueDefinitions {inputValueDefinitionsValue = []}), fieldDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "s"}}), fieldDefinitionDirectives = Nothing}]}})))]}

      itParses "an object type definition with a field definition with an argument"
        " type t { f ( a : u ) : s } "
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionObject (ObjectTypeDefinition {objectTypeDefinitionName = Name {nameValue = "t"}, objectTypeDefinitionInterfaces = Nothing, objectTypeDefinitionDirectives = Nothing, objectTypeDefinitionFields = FieldDefinitions {fieldDefinitionsValue = [FieldDefinition {fieldDefinitionName = Name {nameValue = "f"}, fieldDefinitionArguments = Just (InputValueDefinitions {inputValueDefinitionsValue = [InputValueDefinition {inputValueDefinitionName = Name {nameValue = "a"}, inputValueDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "u"}}), inputValueDefinitionDefaultValue = Nothing, inputValueDefinitionDirectives = Nothing}]}), fieldDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "s"}}), fieldDefinitionDirectives = Nothing}]}})))]}

      itParses "an object type definition with a field definition with an argument with a default value"
        " type t { f ( a : u = d ) : s } "
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionObject (ObjectTypeDefinition {objectTypeDefinitionName = Name {nameValue = "t"}, objectTypeDefinitionInterfaces = Nothing, objectTypeDefinitionDirectives = Nothing, objectTypeDefinitionFields = FieldDefinitions {fieldDefinitionsValue = [FieldDefinition {fieldDefinitionName = Name {nameValue = "f"}, fieldDefinitionArguments = Just (InputValueDefinitions {inputValueDefinitionsValue = [InputValueDefinition {inputValueDefinitionName = Name {nameValue = "a"}, inputValueDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "u"}}), inputValueDefinitionDefaultValue = Just (DefaultValue {defaultValueValue = ValueEnum (Name {nameValue = "d"})}), inputValueDefinitionDirectives = Nothing}]}), fieldDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "s"}}), fieldDefinitionDirectives = Nothing}]}})))]}

      itParses "an object type definition with a field definition with an argument with a directive"
        " type t { f ( a : u @ d ) : s } "
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionObject (ObjectTypeDefinition {objectTypeDefinitionName = Name {nameValue = "t"}, objectTypeDefinitionInterfaces = Nothing, objectTypeDefinitionDirectives = Nothing, objectTypeDefinitionFields = FieldDefinitions {fieldDefinitionsValue = [FieldDefinition {fieldDefinitionName = Name {nameValue = "f"}, fieldDefinitionArguments = Just (InputValueDefinitions {inputValueDefinitionsValue = [InputValueDefinition {inputValueDefinitionName = Name {nameValue = "a"}, inputValueDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "u"}}), inputValueDefinitionDefaultValue = Nothing, inputValueDefinitionDirectives = Just (Directives {directivesValue = [Directive {directiveName = Name {nameValue = "d"}, directiveArguments = Nothing}]})}]}), fieldDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "s"}}), fieldDefinitionDirectives = Nothing}]}})))]}

      itParses "an object type definition with a field definition with a directive"
        " type t { f : s @ d } "
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionObject (ObjectTypeDefinition {objectTypeDefinitionName = Name {nameValue = "t"}, objectTypeDefinitionInterfaces = Nothing, objectTypeDefinitionDirectives = Nothing, objectTypeDefinitionFields = FieldDefinitions {fieldDefinitionsValue = [FieldDefinition {fieldDefinitionName = Name {nameValue = "f"}, fieldDefinitionArguments = Nothing, fieldDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "s"}}), fieldDefinitionDirectives = Just (Directives {directivesValue = [Directive {directiveName = Name {nameValue = "d"}, directiveArguments = Nothing}]})}]}})))]}

      itParses "an interface type definition"
        " interface i { } "
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionInterface (InterfaceTypeDefinition {interfaceTypeDefinitionName = Name {nameValue = "i"}, interfaceTypeDefinitionDirectives = Nothing, interfaceTypeDefinitionFields = FieldDefinitions {fieldDefinitionsValue = []}})))]}

      itParses "an interface type definition with a directive"
        " interface i @ d { } "
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionInterface (InterfaceTypeDefinition {interfaceTypeDefinitionName = Name {nameValue = "i"}, interfaceTypeDefinitionDirectives = Just (Directives {directivesValue = [Directive {directiveName = Name {nameValue = "d"}, directiveArguments = Nothing}]}), interfaceTypeDefinitionFields = FieldDefinitions {fieldDefinitionsValue = []}})))]}

      itParses "an interface type definition with a field definition"
        " interface i { f : t } "
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionInterface (InterfaceTypeDefinition {interfaceTypeDefinitionName = Name {nameValue = "i"}, interfaceTypeDefinitionDirectives = Nothing, interfaceTypeDefinitionFields = FieldDefinitions {fieldDefinitionsValue = [FieldDefinition {fieldDefinitionName = Name {nameValue = "f"}, fieldDefinitionArguments = Nothing, fieldDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "t"}}), fieldDefinitionDirectives = Nothing}]}})))]}

      itParses "a union type definition"
        " union u = t "
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionUnion (UnionTypeDefinition {unionTypeDefinitionName = Name {nameValue = "u"}, unionTypeDefinitionDirectives = Nothing, unionTypeDefinitionTypes = UnionTypes {unionTypesValue = [NamedType {namedTypeValue = Name {nameValue = "t"}}]}})))]}

      itParses "a union type definition with a directive"
        " union u @ d = t "
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionUnion (UnionTypeDefinition {unionTypeDefinitionName = Name {nameValue = "u"}, unionTypeDefinitionDirectives = Just (Directives {directivesValue = [Directive {directiveName = Name {nameValue = "d"}, directiveArguments = Nothing}]}), unionTypeDefinitionTypes = UnionTypes {unionTypesValue = [NamedType {namedTypeValue = Name {nameValue = "t"}}]}})))]}

      itParses "a union type definition with multiple types"
        " union u = t | s "
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionUnion (UnionTypeDefinition {unionTypeDefinitionName = Name {nameValue = "u"}, unionTypeDefinitionDirectives = Nothing, unionTypeDefinitionTypes = UnionTypes {unionTypesValue = [NamedType {namedTypeValue = Name {nameValue = "t"}}, NamedType {namedTypeValue = Name {nameValue = "s"}}]}})))]}

      itParses "an enum type"
        " enum e { } "
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionEnum (EnumTypeDefinition {enumTypeDefinitionName = Name {nameValue = "e"}, enumTypeDefinitionDirectives = Nothing, enumTypeDefinitionValues = EnumValues {enumValuesValue = []}})))]}

      itParses "an enum type with a directive"
        " enum e @ d { } "
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionEnum (EnumTypeDefinition {enumTypeDefinitionName = Name {nameValue = "e"}, enumTypeDefinitionDirectives = Just (Directives {directivesValue = [Directive {directiveName = Name {nameValue = "d"}, directiveArguments = Nothing}]}), enumTypeDefinitionValues = EnumValues {enumValuesValue = []}})))]}

      itParses "an enum type with some values"
        " enum e { t @ d s } "
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionEnum (EnumTypeDefinition {enumTypeDefinitionName = Name {nameValue = "e"}, enumTypeDefinitionDirectives = Nothing, enumTypeDefinitionValues = EnumValues {enumValuesValue = [EnumValueDefinition {enumValueDefinitionName = Name {nameValue = "t"}, enumValueDefinitionDirectives = Just (Directives {directivesValue = [Directive {directiveName = Name {nameValue = "d"}, directiveArguments = Nothing}]})},EnumValueDefinition {enumValueDefinitionName = Name {nameValue = "s"}, enumValueDefinitionDirectives = Nothing}]}})))]}

      itParses "an input type"
        " input i { } "
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionInputObject (InputObjectTypeDefinition {inputObjectTypeDefinitionName = Name {nameValue = "i"}, inputObjectTypeDefinitionDirectives = Nothing, inputObjectTypeDefinitionFields = InputFieldDefinitions {inputFieldDefinitionsValue = []}})))]}

      itParses "an input type with a directive"
        " input i @ d { } "
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionInputObject (InputObjectTypeDefinition {inputObjectTypeDefinitionName = Name {nameValue = "i"}, inputObjectTypeDefinitionDirectives = Just (Directives {directivesValue = [Directive {directiveName = Name {nameValue = "d"}, directiveArguments = Nothing}]}), inputObjectTypeDefinitionFields = InputFieldDefinitions {inputFieldDefinitionsValue = []}})))]}

      itParses "an input type with a field"
        " input i { f : t } "
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionInputObject (InputObjectTypeDefinition {inputObjectTypeDefinitionName = Name {nameValue = "i"}, inputObjectTypeDefinitionDirectives = Nothing, inputObjectTypeDefinitionFields = InputFieldDefinitions {inputFieldDefinitionsValue = [InputValueDefinition {inputValueDefinitionName = Name {nameValue = "f"}, inputValueDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "t"}}), inputValueDefinitionDefaultValue = Nothing, inputValueDefinitionDirectives = Nothing}]}})))]}

      itParses "a type extension"
        " extend type t { } "
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionTypeExtension (TypeExtensionDefinition {typeExtensionDefinitionValue = ObjectTypeDefinition {objectTypeDefinitionName = Name {nameValue = "t"}, objectTypeDefinitionInterfaces = Nothing, objectTypeDefinitionDirectives = Nothing, objectTypeDefinitionFields = FieldDefinitions {fieldDefinitionsValue = []}}}))]}

      itParses "a directive definition"
        " directive @ d on t "
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionDirective (DirectiveDefinition {directiveDefinitionName = Name {nameValue = "d"}, directiveDefinitionArguments = Nothing, directiveDefinitionLocations = DirectiveLocations {directiveLocationsValue = [Name {nameValue = "t"}]}}))]}

      itParses "the schema kitchen sink"
        [here|
          # https://raw.githubusercontent.com/graphql/graphql-js/c4f2656/src/language/__tests__/schema-kitchen-sink.graphql
          schema {
            query: QueryType
            mutation: MutationType
          }

          type Foo implements Bar {
            one: Type
            two(argument: InputType!): Type
            three(argument: InputType, other: String): Int
            four(argument: String = "string"): String
            five(argument: [String] = ["string", "string"]): String
            six(argument: InputType = {key: "value"}): Type
            seven(argument: Int = null): Type
          }

          type AnnotatedObject @onObject(arg: "value") {
            annotatedField(arg: Type = "default" @onArg): Type @onField
          }

          interface Bar {
            one: Type
            four(argument: String = "string"): String
          }

          interface AnnotatedInterface @onInterface {
            annotatedField(arg: Type @onArg): Type @onField
          }

          union Feed = Story | Article | Advert

          union AnnotatedUnion @onUnion = A | B

          scalar CustomScalar

          scalar AnnotatedScalar @onScalar

          enum Site {
            DESKTOP
            MOBILE
          }

          enum AnnotatedEnum @onEnum {
            ANNOTATED_VALUE @onEnumValue
            OTHER_VALUE
          }

          input InputType {
            key: String!
            answer: Int = 42
          }

          input AnnotatedInput @onInputObjectType {
            annotatedField: Type @onField
          }

          extend type Foo {
            seven(argument: [String]): Type
          }

          extend type Foo @onType {}

          type NoFields {}

          directive @skip(if: Boolean!) on FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT

          directive @include(if: Boolean!)
            on FIELD
             | FRAGMENT_SPREAD
             | INLINE_FRAGMENT
        |]
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionSchema (SchemaDefinition {schemaDefinitionDirectives = Nothing, schemaDefinitionOperationTypes = OperationTypeDefinitions {operationTypeDefinitionsValue = [OperationTypeDefinition {operationTypeDefinitionOperation = OperationTypeQuery, operationTypeDefinitionType = NamedType {namedTypeValue = Name {nameValue = "QueryType"}}},OperationTypeDefinition {operationTypeDefinitionOperation = OperationTypeMutation, operationTypeDefinitionType = NamedType {namedTypeValue = Name {nameValue = "MutationType"}}}]}})),DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionObject (ObjectTypeDefinition {objectTypeDefinitionName = Name {nameValue = "Foo"}, objectTypeDefinitionInterfaces = Just (Interfaces {interfacesValue = NamedType {namedTypeValue = Name {nameValue = "Bar"}} :| []}), objectTypeDefinitionDirectives = Nothing, objectTypeDefinitionFields = FieldDefinitions {fieldDefinitionsValue = [FieldDefinition {fieldDefinitionName = Name {nameValue = "one"}, fieldDefinitionArguments = Nothing, fieldDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "Type"}}), fieldDefinitionDirectives = Nothing},FieldDefinition {fieldDefinitionName = Name {nameValue = "two"}, fieldDefinitionArguments = Just (InputValueDefinitions {inputValueDefinitionsValue = [InputValueDefinition {inputValueDefinitionName = Name {nameValue = "argument"}, inputValueDefinitionType = TypeNonNull (NonNullTypeNamed (NamedType {namedTypeValue = Name {nameValue = "InputType"}})), inputValueDefinitionDefaultValue = Nothing, inputValueDefinitionDirectives = Nothing}]}), fieldDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "Type"}}), fieldDefinitionDirectives = Nothing},FieldDefinition {fieldDefinitionName = Name {nameValue = "three"}, fieldDefinitionArguments = Just (InputValueDefinitions {inputValueDefinitionsValue = [InputValueDefinition {inputValueDefinitionName = Name {nameValue = "argument"}, inputValueDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "InputType"}}), inputValueDefinitionDefaultValue = Nothing, inputValueDefinitionDirectives = Nothing},InputValueDefinition {inputValueDefinitionName = Name {nameValue = "other"}, inputValueDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "String"}}), inputValueDefinitionDefaultValue = Nothing, inputValueDefinitionDirectives = Nothing}]}), fieldDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "Int"}}), fieldDefinitionDirectives = Nothing},FieldDefinition {fieldDefinitionName = Name {nameValue = "four"}, fieldDefinitionArguments = Just (InputValueDefinitions {inputValueDefinitionsValue = [InputValueDefinition {inputValueDefinitionName = Name {nameValue = "argument"}, inputValueDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "String"}}), inputValueDefinitionDefaultValue = Just (DefaultValue {defaultValueValue = ValueString "string"}), inputValueDefinitionDirectives = Nothing}]}), fieldDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "String"}}), fieldDefinitionDirectives = Nothing},FieldDefinition {fieldDefinitionName = Name {nameValue = "five"}, fieldDefinitionArguments = Just (InputValueDefinitions {inputValueDefinitionsValue = [InputValueDefinition {inputValueDefinitionName = Name {nameValue = "argument"}, inputValueDefinitionType = TypeList (ListType {listTypeValue = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "String"}})}), inputValueDefinitionDefaultValue = Just (DefaultValue {defaultValueValue = ValueList [ValueString "string",ValueString "string"]}), inputValueDefinitionDirectives = Nothing}]}), fieldDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "String"}}), fieldDefinitionDirectives = Nothing},FieldDefinition {fieldDefinitionName = Name {nameValue = "six"}, fieldDefinitionArguments = Just (InputValueDefinitions {inputValueDefinitionsValue = [InputValueDefinition {inputValueDefinitionName = Name {nameValue = "argument"}, inputValueDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "InputType"}}), inputValueDefinitionDefaultValue = Just (DefaultValue {defaultValueValue = ValueObject [ObjectField {objectFieldName = Name {nameValue = "key"}, objectFieldValue = ValueString "value"}]}), inputValueDefinitionDirectives = Nothing}]}), fieldDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "Type"}}), fieldDefinitionDirectives = Nothing},FieldDefinition {fieldDefinitionName = Name {nameValue = "seven"}, fieldDefinitionArguments = Just (InputValueDefinitions {inputValueDefinitionsValue = [InputValueDefinition {inputValueDefinitionName = Name {nameValue = "argument"}, inputValueDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "Int"}}), inputValueDefinitionDefaultValue = Just (DefaultValue {defaultValueValue = ValueNull}), inputValueDefinitionDirectives = Nothing}]}), fieldDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "Type"}}), fieldDefinitionDirectives = Nothing}]}}))),DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionObject (ObjectTypeDefinition {objectTypeDefinitionName = Name {nameValue = "AnnotatedObject"}, objectTypeDefinitionInterfaces = Nothing, objectTypeDefinitionDirectives = Just (Directives {directivesValue = Directive {directiveName = Name {nameValue = "onObject"}, directiveArguments = Just (Arguments {argumentsValue = [Argument {argumentName = Name {nameValue = "arg"}, argumentValue = ValueString "value"}]})} :| []}), objectTypeDefinitionFields = FieldDefinitions {fieldDefinitionsValue = [FieldDefinition {fieldDefinitionName = Name {nameValue = "annotatedField"}, fieldDefinitionArguments = Just (InputValueDefinitions {inputValueDefinitionsValue = [InputValueDefinition {inputValueDefinitionName = Name {nameValue = "arg"}, inputValueDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "Type"}}), inputValueDefinitionDefaultValue = Just (DefaultValue {defaultValueValue = ValueString "default"}), inputValueDefinitionDirectives = Just (Directives {directivesValue = Directive {directiveName = Name {nameValue = "onArg"}, directiveArguments = Nothing} :| []})}]}), fieldDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "Type"}}), fieldDefinitionDirectives = Just (Directives {directivesValue = Directive {directiveName = Name {nameValue = "onField"}, directiveArguments = Nothing} :| []})}]}}))),DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionInterface (InterfaceTypeDefinition {interfaceTypeDefinitionName = Name {nameValue = "Bar"}, interfaceTypeDefinitionDirectives = Nothing, interfaceTypeDefinitionFields = FieldDefinitions {fieldDefinitionsValue = [FieldDefinition {fieldDefinitionName = Name {nameValue = "one"}, fieldDefinitionArguments = Nothing, fieldDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "Type"}}), fieldDefinitionDirectives = Nothing},FieldDefinition {fieldDefinitionName = Name {nameValue = "four"}, fieldDefinitionArguments = Just (InputValueDefinitions {inputValueDefinitionsValue = [InputValueDefinition {inputValueDefinitionName = Name {nameValue = "argument"}, inputValueDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "String"}}), inputValueDefinitionDefaultValue = Just (DefaultValue {defaultValueValue = ValueString "string"}), inputValueDefinitionDirectives = Nothing}]}), fieldDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "String"}}), fieldDefinitionDirectives = Nothing}]}}))),DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionInterface (InterfaceTypeDefinition {interfaceTypeDefinitionName = Name {nameValue = "AnnotatedInterface"}, interfaceTypeDefinitionDirectives = Just (Directives {directivesValue = Directive {directiveName = Name {nameValue = "onInterface"}, directiveArguments = Nothing} :| []}), interfaceTypeDefinitionFields = FieldDefinitions {fieldDefinitionsValue = [FieldDefinition {fieldDefinitionName = Name {nameValue = "annotatedField"}, fieldDefinitionArguments = Just (InputValueDefinitions {inputValueDefinitionsValue = [InputValueDefinition {inputValueDefinitionName = Name {nameValue = "arg"}, inputValueDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "Type"}}), inputValueDefinitionDefaultValue = Nothing, inputValueDefinitionDirectives = Just (Directives {directivesValue = Directive {directiveName = Name {nameValue = "onArg"}, directiveArguments = Nothing} :| []})}]}), fieldDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "Type"}}), fieldDefinitionDirectives = Just (Directives {directivesValue = Directive {directiveName = Name {nameValue = "onField"}, directiveArguments = Nothing} :| []})}]}}))),DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionUnion (UnionTypeDefinition {unionTypeDefinitionName = Name {nameValue = "Feed"}, unionTypeDefinitionDirectives = Nothing, unionTypeDefinitionTypes = UnionTypes {unionTypesValue = NamedType {namedTypeValue = Name {nameValue = "Story"}} :| [NamedType {namedTypeValue = Name {nameValue = "Article"}},NamedType {namedTypeValue = Name {nameValue = "Advert"}}]}}))),DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionUnion (UnionTypeDefinition {unionTypeDefinitionName = Name {nameValue = "AnnotatedUnion"}, unionTypeDefinitionDirectives = Just (Directives {directivesValue = Directive {directiveName = Name {nameValue = "onUnion"}, directiveArguments = Nothing} :| []}), unionTypeDefinitionTypes = UnionTypes {unionTypesValue = NamedType {namedTypeValue = Name {nameValue = "A"}} :| [NamedType {namedTypeValue = Name {nameValue = "B"}}]}}))),DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionScalar (ScalarTypeDefinition {scalarTypeDefinitionName = Name {nameValue = "CustomScalar"}, scalarTypeDefinitionDirectives = Nothing}))),DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionScalar (ScalarTypeDefinition {scalarTypeDefinitionName = Name {nameValue = "AnnotatedScalar"}, scalarTypeDefinitionDirectives = Just (Directives {directivesValue = Directive {directiveName = Name {nameValue = "onScalar"}, directiveArguments = Nothing} :| []})}))),DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionEnum (EnumTypeDefinition {enumTypeDefinitionName = Name {nameValue = "Site"}, enumTypeDefinitionDirectives = Nothing, enumTypeDefinitionValues = EnumValues {enumValuesValue = [EnumValueDefinition {enumValueDefinitionName = Name {nameValue = "DESKTOP"}, enumValueDefinitionDirectives = Nothing},EnumValueDefinition {enumValueDefinitionName = Name {nameValue = "MOBILE"}, enumValueDefinitionDirectives = Nothing}]}}))),DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionEnum (EnumTypeDefinition {enumTypeDefinitionName = Name {nameValue = "AnnotatedEnum"}, enumTypeDefinitionDirectives = Just (Directives {directivesValue = Directive {directiveName = Name {nameValue = "onEnum"}, directiveArguments = Nothing} :| []}), enumTypeDefinitionValues = EnumValues {enumValuesValue = [EnumValueDefinition {enumValueDefinitionName = Name {nameValue = "ANNOTATED_VALUE"}, enumValueDefinitionDirectives = Just (Directives {directivesValue = Directive {directiveName = Name {nameValue = "onEnumValue"}, directiveArguments = Nothing} :| []})},EnumValueDefinition {enumValueDefinitionName = Name {nameValue = "OTHER_VALUE"}, enumValueDefinitionDirectives = Nothing}]}}))),DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionInputObject (InputObjectTypeDefinition {inputObjectTypeDefinitionName = Name {nameValue = "InputType"}, inputObjectTypeDefinitionDirectives = Nothing, inputObjectTypeDefinitionFields = InputFieldDefinitions {inputFieldDefinitionsValue = [InputValueDefinition {inputValueDefinitionName = Name {nameValue = "key"}, inputValueDefinitionType = TypeNonNull (NonNullTypeNamed (NamedType {namedTypeValue = Name {nameValue = "String"}})), inputValueDefinitionDefaultValue = Nothing, inputValueDefinitionDirectives = Nothing},InputValueDefinition {inputValueDefinitionName = Name {nameValue = "answer"}, inputValueDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "Int"}}), inputValueDefinitionDefaultValue = Just (DefaultValue {defaultValueValue = ValueInt 42}), inputValueDefinitionDirectives = Nothing}]}}))),DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionInputObject (InputObjectTypeDefinition {inputObjectTypeDefinitionName = Name {nameValue = "AnnotatedInput"}, inputObjectTypeDefinitionDirectives = Just (Directives {directivesValue = Directive {directiveName = Name {nameValue = "onInputObjectType"}, directiveArguments = Nothing} :| []}), inputObjectTypeDefinitionFields = InputFieldDefinitions {inputFieldDefinitionsValue = [InputValueDefinition {inputValueDefinitionName = Name {nameValue = "annotatedField"}, inputValueDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "Type"}}), inputValueDefinitionDefaultValue = Nothing, inputValueDefinitionDirectives = Just (Directives {directivesValue = Directive {directiveName = Name {nameValue = "onField"}, directiveArguments = Nothing} :| []})}]}}))),DefinitionTypeSystem (TypeSystemDefinitionTypeExtension (TypeExtensionDefinition {typeExtensionDefinitionValue = ObjectTypeDefinition {objectTypeDefinitionName = Name {nameValue = "Foo"}, objectTypeDefinitionInterfaces = Nothing, objectTypeDefinitionDirectives = Nothing, objectTypeDefinitionFields = FieldDefinitions {fieldDefinitionsValue = [FieldDefinition {fieldDefinitionName = Name {nameValue = "seven"}, fieldDefinitionArguments = Just (InputValueDefinitions {inputValueDefinitionsValue = [InputValueDefinition {inputValueDefinitionName = Name {nameValue = "argument"}, inputValueDefinitionType = TypeList (ListType {listTypeValue = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "String"}})}), inputValueDefinitionDefaultValue = Nothing, inputValueDefinitionDirectives = Nothing}]}), fieldDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "Type"}}), fieldDefinitionDirectives = Nothing}]}}})),DefinitionTypeSystem (TypeSystemDefinitionTypeExtension (TypeExtensionDefinition {typeExtensionDefinitionValue = ObjectTypeDefinition {objectTypeDefinitionName = Name {nameValue = "Foo"}, objectTypeDefinitionInterfaces = Nothing, objectTypeDefinitionDirectives = Just (Directives {directivesValue = Directive {directiveName = Name {nameValue = "onType"}, directiveArguments = Nothing} :| []}), objectTypeDefinitionFields = FieldDefinitions {fieldDefinitionsValue = []}}})),DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionObject (ObjectTypeDefinition {objectTypeDefinitionName = Name {nameValue = "NoFields"}, objectTypeDefinitionInterfaces = Nothing, objectTypeDefinitionDirectives = Nothing, objectTypeDefinitionFields = FieldDefinitions {fieldDefinitionsValue = []}}))),DefinitionTypeSystem (TypeSystemDefinitionDirective (DirectiveDefinition {directiveDefinitionName = Name {nameValue = "skip"}, directiveDefinitionArguments = Just (InputValueDefinitions {inputValueDefinitionsValue = [InputValueDefinition {inputValueDefinitionName = Name {nameValue = "if"}, inputValueDefinitionType = TypeNonNull (NonNullTypeNamed (NamedType {namedTypeValue = Name {nameValue = "Boolean"}})), inputValueDefinitionDefaultValue = Nothing, inputValueDefinitionDirectives = Nothing}]}), directiveDefinitionLocations = DirectiveLocations {directiveLocationsValue = Name {nameValue = "FIELD"} :| [Name {nameValue = "FRAGMENT_SPREAD"},Name {nameValue = "INLINE_FRAGMENT"}]}})),DefinitionTypeSystem (TypeSystemDefinitionDirective (DirectiveDefinition {directiveDefinitionName = Name {nameValue = "include"}, directiveDefinitionArguments = Just (InputValueDefinitions {inputValueDefinitionsValue = [InputValueDefinition {inputValueDefinitionName = Name {nameValue = "if"}, inputValueDefinitionType = TypeNonNull (NonNullTypeNamed (NamedType {namedTypeValue = Name {nameValue = "Boolean"}})), inputValueDefinitionDefaultValue = Nothing, inputValueDefinitionDirectives = Nothing}]}), directiveDefinitionLocations = DirectiveLocations {directiveLocationsValue = Name {nameValue = "FIELD"} :| [Name {nameValue = "FRAGMENT_SPREAD"},Name {nameValue = "INLINE_FRAGMENT"}]}}))]}

    context "pretty printer" $ do

      itPrettyPrints "an empty document"
        Document { documentValue = [] }
        ""

      itPrettyPrints "a query"
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [] } } ] }
        "query {}"

      itPrettyPrints "a field"
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [ SelectionField Field { fieldAlias = Nothing , fieldName = Name { nameValue = "f" } , fieldArguments = Nothing , fieldDirectives = Nothing , fieldSelectionSet = Nothing } ] } } ] }
        "query {f}"

      itPrettyPrints "a field alias"
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [ SelectionField Field { fieldAlias = Just Alias { aliasValue = Name { nameValue = "a" } } , fieldName = Name { nameValue = "f" } , fieldArguments = Nothing , fieldDirectives = Nothing , fieldSelectionSet = Nothing } ] } } ] }
        "query {a: f}"

      itPrettyPrints "empty arguments"
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [ SelectionField Field { fieldAlias = Nothing , fieldName = Name { nameValue = "f" } , fieldArguments = Just Arguments { argumentsValue = [] } , fieldDirectives = Nothing , fieldSelectionSet = Nothing } ] } } ] }
        "query {f ()}"

      itPrettyPrints "a null"
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [ SelectionField Field { fieldAlias = Nothing , fieldName = Name { nameValue = "f" } , fieldArguments = Just Arguments { argumentsValue = [ Argument { argumentName = Name { nameValue = "a" } , argumentValue = ValueNull } ] } , fieldDirectives = Nothing , fieldSelectionSet = Nothing } ] } } ] }
        "query {f (a: null)}"

      itPrettyPrints "a list"
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [ SelectionField Field { fieldAlias = Nothing , fieldName = Name { nameValue = "f" } , fieldArguments = Just Arguments { argumentsValue = [ Argument { argumentName = Name { nameValue = "a" } , argumentValue = ValueList [] } ] } , fieldDirectives = Nothing , fieldSelectionSet = Nothing } ] } } ] }
        "query {f (a: [])}"

      itPrettyPrints "an enum"
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [ SelectionField Field { fieldAlias = Nothing , fieldName = Name { nameValue = "f" } , fieldArguments = Just Arguments { argumentsValue = [ Argument { argumentName = Name { nameValue = "a" } , argumentValue = ValueEnum Name { nameValue = "e" } } ] } , fieldDirectives = Nothing , fieldSelectionSet = Nothing } ] } } ] }
        "query {f (a: e)}"

      itPrettyPrints "a boolean"
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [ SelectionField Field { fieldAlias = Nothing , fieldName = Name { nameValue = "f" } , fieldArguments = Just Arguments { argumentsValue = [ Argument { argumentName = Name { nameValue = "a" } , argumentValue = ValueBoolean False } ] } , fieldDirectives = Nothing , fieldSelectionSet = Nothing } ] } } ] }
        "query {f (a: false)}"

      itPrettyPrints "a variable"
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [ SelectionField Field { fieldAlias = Nothing , fieldName = Name { nameValue = "f" } , fieldArguments = Just Arguments { argumentsValue = [ Argument { argumentName = Name { nameValue = "a" } , argumentValue = ValueVariable Variable { variableValue = Name { nameValue = "v" } } } ] } , fieldDirectives = Nothing , fieldSelectionSet = Nothing } ] } } ] }
        "query {f (a: $v)}"

      itPrettyPrints "an int"
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [ SelectionField Field { fieldAlias = Nothing , fieldName = Name { nameValue = "f" } , fieldArguments = Just Arguments { argumentsValue = [ Argument { argumentName = Name { nameValue = "a" } , argumentValue = ValueInt 0 } ] } , fieldDirectives = Nothing , fieldSelectionSet = Nothing } ] } } ] }
        "query {f (a: 0)}"

      itPrettyPrints "a float"
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [ SelectionField Field { fieldAlias = Nothing , fieldName = Name { nameValue = "f" } , fieldArguments = Just Arguments { argumentsValue = [ Argument { argumentName = Name { nameValue = "a" } , argumentValue = ValueFloat 1.2 } ] } , fieldDirectives = Nothing , fieldSelectionSet = Nothing } ] } } ] }
        "query {f (a: 1.2)}"

      itPrettyPrints "a string"
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [ SelectionField Field { fieldAlias = Nothing , fieldName = Name { nameValue = "f" } , fieldArguments = Just Arguments { argumentsValue = [ Argument { argumentName = Name { nameValue = "a" } , argumentValue = ValueString "" } ] } , fieldDirectives = Nothing , fieldSelectionSet = Nothing } ] } } ] }
        "query {f (a: \"\")}"

      itPrettyPrints "an object"
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [ SelectionField Field { fieldAlias = Nothing , fieldName = Name { nameValue = "f" } , fieldArguments = Just Arguments { argumentsValue = [ Argument { argumentName = Name { nameValue = "a" } , argumentValue = ValueObject [] } ] } , fieldDirectives = Nothing , fieldSelectionSet = Nothing } ] } } ] }
        "query {f (a: {})}"

      itPrettyPrints "an object with a field"
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [ SelectionField Field { fieldAlias = Nothing , fieldName = Name { nameValue = "f" } , fieldArguments = Just Arguments { argumentsValue = [ Argument { argumentName = Name { nameValue = "a" } , argumentValue = ValueObject [ ObjectField { objectFieldName = Name { nameValue = "k" } , objectFieldValue = ValueNull } ] } ] } , fieldDirectives = Nothing , fieldSelectionSet = Nothing } ] } } ] }
        "query {f (a: {k: null})}"

      itPrettyPrints "a mutation"
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeMutation , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [] } } ] }
        "mutation {}"

      itPrettyPrints "a subscription"
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeSubscription , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [] } } ] }
        "subscription {}"

      itPrettyPrints "an empty variable definition"
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Just VariableDefinitions { variableDefinitionsValue = [] } , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [] } } ] }
        "query () {}"

      itPrettyPrints "a named variable definition"
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Just VariableDefinitions { variableDefinitionsValue = [ VariableDefinition { variableDefinitionVariable = Variable { variableValue = Name { nameValue = "v" } } , variableDefinitionType = TypeNamed NamedType { namedTypeValue = Name { nameValue = "t" } } , variableDefinitionDefaultValue = Nothing } ] } , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [] } } ] }
        "query ($v: t) {}"

      itPrettyPrints "a list variable definition"
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Just VariableDefinitions { variableDefinitionsValue = [ VariableDefinition { variableDefinitionVariable = Variable { variableValue = Name { nameValue = "v" } } , variableDefinitionType = TypeList ListType { listTypeValue = TypeNamed NamedType { namedTypeValue = Name { nameValue = "t" } } } , variableDefinitionDefaultValue = Nothing } ] } , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [] } } ] }
        "query ($v: [t]) {}"

      itPrettyPrints "a non-null named variable definition"
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Just VariableDefinitions { variableDefinitionsValue = [ VariableDefinition { variableDefinitionVariable = Variable { variableValue = Name { nameValue = "v" } } , variableDefinitionType = TypeNonNull (NonNullTypeNamed NamedType { namedTypeValue = Name { nameValue = "t" } }) , variableDefinitionDefaultValue = Nothing } ] } , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [] } } ] }
        "query ($v: t!) {}"

      itPrettyPrints "a non-null list variable definition"
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Just VariableDefinitions { variableDefinitionsValue = [ VariableDefinition { variableDefinitionVariable = Variable { variableValue = Name { nameValue = "v" } } , variableDefinitionType = TypeNonNull (NonNullTypeList ListType { listTypeValue = TypeNamed NamedType { namedTypeValue = Name { nameValue = "t" } } }) , variableDefinitionDefaultValue = Nothing } ] } , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [] } } ] }
        "query ($v: [t]!) {}"

      itPrettyPrints "a variable definition with a default value"
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Just VariableDefinitions { variableDefinitionsValue = [ VariableDefinition { variableDefinitionVariable = Variable { variableValue = Name { nameValue = "v" } } , variableDefinitionType = TypeNamed NamedType { namedTypeValue = Name { nameValue = "t" } } , variableDefinitionDefaultValue = Just DefaultValue { defaultValueValue = ValueNull } } ] } , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [] } } ] }
        "query ($v: t = null) {}"

      itPrettyPrints "a directive"
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Just Directives { directivesValue = [ Directive { directiveName = Name { nameValue = "d" } , directiveArguments = Nothing } ] } , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [] } } ] }
        "query @d {}"

      itPrettyPrints "a directive with empty arguments"
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Just Directives { directivesValue = [ Directive { directiveName = Name { nameValue = "d" } , directiveArguments = Just Arguments { argumentsValue = [] } } ] } , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [] } } ] }
        "query @d () {}"

      itPrettyPrints "a directive with an argument"
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Just Directives { directivesValue = [ Directive { directiveName = Name { nameValue = "d" } , directiveArguments = Just Arguments { argumentsValue = [ Argument { argumentName = Name { nameValue = "a" } , argumentValue = ValueNull } ] } } ] } , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [] } } ] }
        "query @d (a: null) {}"

      itPrettyPrints "a fragment spread"
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [ SelectionFragmentSpread FragmentSpread { fragmentSpreadName = FragmentName { fragmentNameValue = Name { nameValue = "s" } } , fragmentSpreadDirectives = Nothing } ] } } ] }
        "query {... s}"

      itPrettyPrints "a fragment spread with a directive"
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [ SelectionFragmentSpread FragmentSpread { fragmentSpreadName = FragmentName { fragmentNameValue = Name { nameValue = "s" } } , fragmentSpreadDirectives = Just Directives { directivesValue = [ Directive { directiveName = Name { nameValue = "d" } , directiveArguments = Nothing } ] } } ] } } ] }
        "query {... s @d}"

      itPrettyPrints "an inline fragment"
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [ SelectionInlineFragment InlineFragment { inlineFragmentTypeCondition = Nothing , inlineFragmentDirectives = Nothing , inlineFragmentSelectionSet = SelectionSet { selectionSetValue = [] } } ] } } ] }
        "query {... {}}"

      itPrettyPrints "an inline fragment with a type condition"
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [ SelectionInlineFragment InlineFragment { inlineFragmentTypeCondition = Just TypeCondition { typeConditionValue = NamedType { namedTypeValue = Name { nameValue = "t" } } } , inlineFragmentDirectives = Nothing , inlineFragmentSelectionSet = SelectionSet { selectionSetValue = [] } } ] } } ] }
        "query {... on t {}}"

      itPrettyPrints "an inline fragment with a directive"
        Document { documentValue = [ DefinitionOperation OperationDefinition { operationDefinitionOperationType = OperationTypeQuery , operationDefinitionName = Nothing , operationDefinitionVariableDefinitions = Nothing , operationDefinitionDirectives = Nothing , operationDefinitionSelectionSet = SelectionSet { selectionSetValue = [ SelectionInlineFragment InlineFragment { inlineFragmentTypeCondition = Nothing , inlineFragmentDirectives = Just Directives { directivesValue = [ Directive { directiveName = Name { nameValue = "d" } , directiveArguments = Nothing } ] } , inlineFragmentSelectionSet = SelectionSet { selectionSetValue = [] } } ] } } ] }
        "query {... @d {}}"

      itPrettyPrints "a fragment definition"
        Document { documentValue = [ DefinitionFragment FragmentDefinition { fragmentName = FragmentName { fragmentNameValue = Name { nameValue = "f" } } , fragmentTypeCondition = TypeCondition { typeConditionValue = NamedType { namedTypeValue = Name { nameValue = "t" } } } , fragmentDirectives = Nothing , fragmentSelectionSet = SelectionSet { selectionSetValue = [] } } ] }
        "fragment f on t {}"

      itPrettyPrints "a fragment definition with a directive"
        Document { documentValue = [ DefinitionFragment FragmentDefinition { fragmentName = FragmentName { fragmentNameValue = Name { nameValue = "f" } } , fragmentTypeCondition = TypeCondition { typeConditionValue = NamedType { namedTypeValue = Name { nameValue = "t" } } } , fragmentDirectives = Just Directives { directivesValue = [ Directive { directiveName = Name { nameValue = "d" } , directiveArguments = Nothing } ] } , fragmentSelectionSet = SelectionSet { selectionSetValue = [] } } ] }
        "fragment f on t @d {}"

      itPrettyPrints "the query kitchen sink"
        Document {documentValue = [DefinitionOperation (OperationDefinition {operationDefinitionOperationType = OperationTypeQuery, operationDefinitionName = Just (Name {nameValue = "queryName"}), operationDefinitionVariableDefinitions = Just (VariableDefinitions {variableDefinitionsValue = [VariableDefinition {variableDefinitionVariable = Variable {variableValue = Name {nameValue = "foo"}}, variableDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "ComplexType"}}), variableDefinitionDefaultValue = Nothing},VariableDefinition {variableDefinitionVariable = Variable {variableValue = Name {nameValue = "site"}}, variableDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "Site"}}), variableDefinitionDefaultValue = Just (DefaultValue {defaultValueValue = ValueEnum (Name {nameValue = "MOBILE"})})}]}), operationDefinitionDirectives = Nothing, operationDefinitionSelectionSet = SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Just (Alias {aliasValue = Name {nameValue = "whoever123is"}}), fieldName = Name {nameValue = "node"}, fieldArguments = Just (Arguments {argumentsValue = [Argument {argumentName = Name {nameValue = "id"}, argumentValue = ValueList [ValueInt 123,ValueInt 456]}]}), fieldDirectives = Nothing, fieldSelectionSet = Just (SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "id"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Nothing}),SelectionInlineFragment (InlineFragment {inlineFragmentTypeCondition = Just (TypeCondition {typeConditionValue = NamedType {namedTypeValue = Name {nameValue = "User"}}}), inlineFragmentDirectives = Just (Directives {directivesValue = [Directive {directiveName = Name {nameValue = "defer"}, directiveArguments = Nothing}]}), inlineFragmentSelectionSet = SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "field2"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Just (SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "id"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Nothing}),SelectionField (Field {fieldAlias = Just (Alias {aliasValue = Name {nameValue = "alias"}}), fieldName = Name {nameValue = "field1"}, fieldArguments = Just (Arguments {argumentsValue = [Argument {argumentName = Name {nameValue = "first"}, argumentValue = ValueInt 10},Argument {argumentName = Name {nameValue = "after"}, argumentValue = ValueVariable (Variable {variableValue = Name {nameValue = "foo"}})}]}), fieldDirectives = Just (Directives {directivesValue = [Directive {directiveName = Name {nameValue = "include"}, directiveArguments = Just (Arguments {argumentsValue = [Argument {argumentName = Name {nameValue = "if"}, argumentValue = ValueVariable (Variable {variableValue = Name {nameValue = "foo"}})}]})}]}), fieldSelectionSet = Just (SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "id"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Nothing}),SelectionFragmentSpread (FragmentSpread {fragmentSpreadName = FragmentName {fragmentNameValue = Name {nameValue = "frag"}}, fragmentSpreadDirectives = Nothing})]})})]})})]}}),SelectionInlineFragment (InlineFragment {inlineFragmentTypeCondition = Nothing, inlineFragmentDirectives = Just (Directives {directivesValue = [Directive {directiveName = Name {nameValue = "skip"}, directiveArguments = Just (Arguments {argumentsValue = [Argument {argumentName = Name {nameValue = "unless"}, argumentValue = ValueVariable (Variable {variableValue = Name {nameValue = "foo"}})}]})}]}), inlineFragmentSelectionSet = SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "id"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Nothing})]}}),SelectionInlineFragment (InlineFragment {inlineFragmentTypeCondition = Nothing, inlineFragmentDirectives = Nothing, inlineFragmentSelectionSet = SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "id"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Nothing})]}})]})})]}}),DefinitionOperation (OperationDefinition {operationDefinitionOperationType = OperationTypeMutation, operationDefinitionName = Just (Name {nameValue = "likeStory"}), operationDefinitionVariableDefinitions = Nothing, operationDefinitionDirectives = Nothing, operationDefinitionSelectionSet = SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "like"}, fieldArguments = Just (Arguments {argumentsValue = [Argument {argumentName = Name {nameValue = "story"}, argumentValue = ValueInt 123}]}), fieldDirectives = Just (Directives {directivesValue = [Directive {directiveName = Name {nameValue = "defer"}, directiveArguments = Nothing}]}), fieldSelectionSet = Just (SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "story"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Just (SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "id"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Nothing})]})})]})})]}}),DefinitionOperation (OperationDefinition {operationDefinitionOperationType = OperationTypeSubscription, operationDefinitionName = Just (Name {nameValue = "StoryLikeSubscription"}), operationDefinitionVariableDefinitions = Just (VariableDefinitions {variableDefinitionsValue = [VariableDefinition {variableDefinitionVariable = Variable {variableValue = Name {nameValue = "input"}}, variableDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "StoryLikeSubscribeInput"}}), variableDefinitionDefaultValue = Nothing}]}), operationDefinitionDirectives = Nothing, operationDefinitionSelectionSet = SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "storyLikeSubscribe"}, fieldArguments = Just (Arguments {argumentsValue = [Argument {argumentName = Name {nameValue = "input"}, argumentValue = ValueVariable (Variable {variableValue = Name {nameValue = "input"}})}]}), fieldDirectives = Nothing, fieldSelectionSet = Just (SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "story"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Just (SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "likers"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Just (SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "count"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Nothing})]})}),SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "likeSentence"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Just (SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "text"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Nothing})]})})]})})]})})]}}),DefinitionFragment (FragmentDefinition {fragmentName = FragmentName {fragmentNameValue = Name {nameValue = "frag"}}, fragmentTypeCondition = TypeCondition {typeConditionValue = NamedType {namedTypeValue = Name {nameValue = "Friend"}}}, fragmentDirectives = Nothing, fragmentSelectionSet = SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "foo"}, fieldArguments = Just (Arguments {argumentsValue = [Argument {argumentName = Name {nameValue = "size"}, argumentValue = ValueVariable (Variable {variableValue = Name {nameValue = "size"}})},Argument {argumentName = Name {nameValue = "bar"}, argumentValue = ValueVariable (Variable {variableValue = Name {nameValue = "b"}})},Argument {argumentName = Name {nameValue = "obj"}, argumentValue = ValueObject [ObjectField {objectFieldName = Name {nameValue = "key"}, objectFieldValue = ValueString "value"}]}]}), fieldDirectives = Nothing, fieldSelectionSet = Nothing})]}}),DefinitionOperation (OperationDefinition {operationDefinitionOperationType = OperationTypeQuery, operationDefinitionName = Nothing, operationDefinitionVariableDefinitions = Nothing, operationDefinitionDirectives = Nothing, operationDefinitionSelectionSet = SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "unnamed"}, fieldArguments = Just (Arguments {argumentsValue = [Argument {argumentName = Name {nameValue = "truthy"}, argumentValue = ValueBoolean True},Argument {argumentName = Name {nameValue = "falsey"}, argumentValue = ValueBoolean False},Argument {argumentName = Name {nameValue = "nullish"}, argumentValue = ValueNull}]}), fieldDirectives = Nothing, fieldSelectionSet = Nothing}),SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "query"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Nothing})]}})]}
        [here|query queryName ($foo: ComplexType $site: Site = MOBILE) {whoever123is: node (id: [123 456]) {id ... on User @defer {field2 {id alias: field1 (first: 10 after: $foo) @include (if: $foo) {id ... frag}}} ... @skip (unless: $foo) {id} ... {id}}}
mutation likeStory {like (story: 123) @defer {story {id}}}
subscription StoryLikeSubscription ($input: StoryLikeSubscribeInput) {storyLikeSubscribe (input: $input) {story {likers {count} likeSentence {text}}}}
fragment frag on Friend {foo (size: $size bar: $b obj: {key: "value"})}
query {unnamed (truthy: true falsey: false nullish: null) query}|]

      itPrettyPrints "a schema"
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionSchema (SchemaDefinition {schemaDefinitionDirectives = Nothing, schemaDefinitionOperationTypes = OperationTypeDefinitions {operationTypeDefinitionsValue = []}}))]}
        "schema {}"

      itPrettyPrints "a schema with a directive"
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionSchema (SchemaDefinition {schemaDefinitionDirectives = Just (Directives {directivesValue = [Directive {directiveName = Name {nameValue = "d"}, directiveArguments = Nothing}]}), schemaDefinitionOperationTypes = OperationTypeDefinitions {operationTypeDefinitionsValue = []}}))]}
        "schema @d {}"

      itPrettyPrints "a schema with an operation type"
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionSchema (SchemaDefinition {schemaDefinitionDirectives = Nothing, schemaDefinitionOperationTypes = OperationTypeDefinitions {operationTypeDefinitionsValue = [OperationTypeDefinition {operationTypeDefinitionOperation = OperationTypeQuery, operationTypeDefinitionType = NamedType {namedTypeValue = Name {nameValue = "t"}}}]}}))]}
        "schema {query: t}"

      itPrettyPrints "a scalar"
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionScalar (ScalarTypeDefinition {scalarTypeDefinitionName = Name {nameValue = "t"}, scalarTypeDefinitionDirectives = Nothing})))]}
        "scalar t"

      itPrettyPrints "a scalar with a directive"
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionScalar (ScalarTypeDefinition {scalarTypeDefinitionName = Name {nameValue = "t"}, scalarTypeDefinitionDirectives = Just (Directives {directivesValue = [Directive {directiveName = Name {nameValue = "d"}, directiveArguments = Nothing}]})})))]}
        "scalar t @d"

      itPrettyPrints "an object type definition"
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionObject (ObjectTypeDefinition {objectTypeDefinitionName = Name {nameValue = "t"}, objectTypeDefinitionInterfaces = Nothing, objectTypeDefinitionDirectives = Nothing, objectTypeDefinitionFields = FieldDefinitions {fieldDefinitionsValue = []}})))]}
        "type t {}"

      itPrettyPrints "an object type definition with an interface"
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionObject (ObjectTypeDefinition {objectTypeDefinitionName = Name {nameValue = "t"}, objectTypeDefinitionInterfaces = Just (Interfaces {interfacesValue = [NamedType {namedTypeValue = Name {nameValue = "i"}}]}), objectTypeDefinitionDirectives = Nothing, objectTypeDefinitionFields = FieldDefinitions {fieldDefinitionsValue = []}})))]}
        "type t implements i {}"

      itPrettyPrints "an object type definition with a directive"
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionObject (ObjectTypeDefinition {objectTypeDefinitionName = Name {nameValue = "t"}, objectTypeDefinitionInterfaces = Nothing, objectTypeDefinitionDirectives = Just (Directives {directivesValue = [Directive {directiveName = Name {nameValue = "d"}, directiveArguments = Nothing}]}), objectTypeDefinitionFields = FieldDefinitions {fieldDefinitionsValue = []}})))]}
        "type t @d {}"

      itPrettyPrints "an object type definition with a field definition"
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionObject (ObjectTypeDefinition {objectTypeDefinitionName = Name {nameValue = "t"}, objectTypeDefinitionInterfaces = Nothing, objectTypeDefinitionDirectives = Nothing, objectTypeDefinitionFields = FieldDefinitions {fieldDefinitionsValue = [FieldDefinition {fieldDefinitionName = Name {nameValue = "f"}, fieldDefinitionArguments = Nothing, fieldDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "s"}}), fieldDefinitionDirectives = Nothing}]}})))]}
        "type t {f: s}"

      itPrettyPrints "an object type definition with a field definition with empty arguments"
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionObject (ObjectTypeDefinition {objectTypeDefinitionName = Name {nameValue = "t"}, objectTypeDefinitionInterfaces = Nothing, objectTypeDefinitionDirectives = Nothing, objectTypeDefinitionFields = FieldDefinitions {fieldDefinitionsValue = [FieldDefinition {fieldDefinitionName = Name {nameValue = "f"}, fieldDefinitionArguments = Just (InputValueDefinitions {inputValueDefinitionsValue = []}), fieldDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "s"}}), fieldDefinitionDirectives = Nothing}]}})))]}
        "type t {f(): s}"

      itPrettyPrints "an object type definition with a field definition with an argument"
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionObject (ObjectTypeDefinition {objectTypeDefinitionName = Name {nameValue = "t"}, objectTypeDefinitionInterfaces = Nothing, objectTypeDefinitionDirectives = Nothing, objectTypeDefinitionFields = FieldDefinitions {fieldDefinitionsValue = [FieldDefinition {fieldDefinitionName = Name {nameValue = "f"}, fieldDefinitionArguments = Just (InputValueDefinitions {inputValueDefinitionsValue = [InputValueDefinition {inputValueDefinitionName = Name {nameValue = "a"}, inputValueDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "u"}}), inputValueDefinitionDefaultValue = Nothing, inputValueDefinitionDirectives = Nothing}]}), fieldDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "s"}}), fieldDefinitionDirectives = Nothing}]}})))]}
        "type t {f(a: u): s}"

      itPrettyPrints "an object type definition with a field definition with an argument with a default value"
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionObject (ObjectTypeDefinition {objectTypeDefinitionName = Name {nameValue = "t"}, objectTypeDefinitionInterfaces = Nothing, objectTypeDefinitionDirectives = Nothing, objectTypeDefinitionFields = FieldDefinitions {fieldDefinitionsValue = [FieldDefinition {fieldDefinitionName = Name {nameValue = "f"}, fieldDefinitionArguments = Just (InputValueDefinitions {inputValueDefinitionsValue = [InputValueDefinition {inputValueDefinitionName = Name {nameValue = "a"}, inputValueDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "u"}}), inputValueDefinitionDefaultValue = Just (DefaultValue {defaultValueValue = ValueEnum (Name {nameValue = "d"})}), inputValueDefinitionDirectives = Nothing}]}), fieldDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "s"}}), fieldDefinitionDirectives = Nothing}]}})))]}
        "type t {f(a: u = d): s}"

      itPrettyPrints "an object type definition with a field definition with an argument with a directive"
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionObject (ObjectTypeDefinition {objectTypeDefinitionName = Name {nameValue = "t"}, objectTypeDefinitionInterfaces = Nothing, objectTypeDefinitionDirectives = Nothing, objectTypeDefinitionFields = FieldDefinitions {fieldDefinitionsValue = [FieldDefinition {fieldDefinitionName = Name {nameValue = "f"}, fieldDefinitionArguments = Just (InputValueDefinitions {inputValueDefinitionsValue = [InputValueDefinition {inputValueDefinitionName = Name {nameValue = "a"}, inputValueDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "u"}}), inputValueDefinitionDefaultValue = Nothing, inputValueDefinitionDirectives = Just (Directives {directivesValue = [Directive {directiveName = Name {nameValue = "d"}, directiveArguments = Nothing}]})}]}), fieldDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "s"}}), fieldDefinitionDirectives = Nothing}]}})))]}
        "type t {f(a: u @d): s}"

      itPrettyPrints "an object type definition with a field definition with a directive"
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionObject (ObjectTypeDefinition {objectTypeDefinitionName = Name {nameValue = "t"}, objectTypeDefinitionInterfaces = Nothing, objectTypeDefinitionDirectives = Nothing, objectTypeDefinitionFields = FieldDefinitions {fieldDefinitionsValue = [FieldDefinition {fieldDefinitionName = Name {nameValue = "f"}, fieldDefinitionArguments = Nothing, fieldDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "s"}}), fieldDefinitionDirectives = Just (Directives {directivesValue = [Directive {directiveName = Name {nameValue = "d"}, directiveArguments = Nothing}]})}]}})))]}
        "type t {f: s @d}"

      itPrettyPrints "an interface type definition"
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionInterface (InterfaceTypeDefinition {interfaceTypeDefinitionName = Name {nameValue = "i"}, interfaceTypeDefinitionDirectives = Nothing, interfaceTypeDefinitionFields = FieldDefinitions {fieldDefinitionsValue = []}})))]}
        "interface i {}"

      itPrettyPrints "an interface type definition with a directive"
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionInterface (InterfaceTypeDefinition {interfaceTypeDefinitionName = Name {nameValue = "i"}, interfaceTypeDefinitionDirectives = Just (Directives {directivesValue = [Directive {directiveName = Name {nameValue = "d"}, directiveArguments = Nothing}]}), interfaceTypeDefinitionFields = FieldDefinitions {fieldDefinitionsValue = []}})))]}
        "interface i @d {}"

      itPrettyPrints "an interface type definition with a field definition"
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionInterface (InterfaceTypeDefinition {interfaceTypeDefinitionName = Name {nameValue = "i"}, interfaceTypeDefinitionDirectives = Nothing, interfaceTypeDefinitionFields = FieldDefinitions {fieldDefinitionsValue = [FieldDefinition {fieldDefinitionName = Name {nameValue = "f"}, fieldDefinitionArguments = Nothing, fieldDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "t"}}), fieldDefinitionDirectives = Nothing}]}})))]}
        "interface i {f: t}"

      itPrettyPrints "a union type definition"
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionUnion (UnionTypeDefinition {unionTypeDefinitionName = Name {nameValue = "u"}, unionTypeDefinitionDirectives = Nothing, unionTypeDefinitionTypes = UnionTypes {unionTypesValue = [NamedType {namedTypeValue = Name {nameValue = "t"}}]}})))]}
        "union u = t"

      itPrettyPrints "a union type definition with a directive"
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionUnion (UnionTypeDefinition {unionTypeDefinitionName = Name {nameValue = "u"}, unionTypeDefinitionDirectives = Just (Directives {directivesValue = [Directive {directiveName = Name {nameValue = "d"}, directiveArguments = Nothing}]}), unionTypeDefinitionTypes = UnionTypes {unionTypesValue = [NamedType {namedTypeValue = Name {nameValue = "t"}}]}})))]}
        "union u @d = t"

      itPrettyPrints "a union type definition with multiple types"
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionUnion (UnionTypeDefinition {unionTypeDefinitionName = Name {nameValue = "u"}, unionTypeDefinitionDirectives = Nothing, unionTypeDefinitionTypes = UnionTypes {unionTypesValue = [NamedType {namedTypeValue = Name {nameValue = "t"}}, NamedType {namedTypeValue = Name {nameValue = "s"}}]}})))]}
        "union u = t | s"

      itPrettyPrints "an enum type"
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionEnum (EnumTypeDefinition {enumTypeDefinitionName = Name {nameValue = "e"}, enumTypeDefinitionDirectives = Nothing, enumTypeDefinitionValues = EnumValues {enumValuesValue = []}})))]}
        "enum e {}"

      itPrettyPrints "an enum type with a directive"
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionEnum (EnumTypeDefinition {enumTypeDefinitionName = Name {nameValue = "e"}, enumTypeDefinitionDirectives = Just (Directives {directivesValue = [Directive {directiveName = Name {nameValue = "d"}, directiveArguments = Nothing}]}), enumTypeDefinitionValues = EnumValues {enumValuesValue = []}})))]}
        "enum e @d {}"

      itPrettyPrints "an enum type with some values"
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionEnum (EnumTypeDefinition {enumTypeDefinitionName = Name {nameValue = "e"}, enumTypeDefinitionDirectives = Nothing, enumTypeDefinitionValues = EnumValues {enumValuesValue = [EnumValueDefinition {enumValueDefinitionName = Name {nameValue = "t"}, enumValueDefinitionDirectives = Just (Directives {directivesValue = [Directive {directiveName = Name {nameValue = "d"}, directiveArguments = Nothing}]})},EnumValueDefinition {enumValueDefinitionName = Name {nameValue = "s"}, enumValueDefinitionDirectives = Nothing}]}})))]}
        "enum e {t @d s}"

      itPrettyPrints "an input type"
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionInputObject (InputObjectTypeDefinition {inputObjectTypeDefinitionName = Name {nameValue = "i"}, inputObjectTypeDefinitionDirectives = Nothing, inputObjectTypeDefinitionFields = InputFieldDefinitions {inputFieldDefinitionsValue = []}})))]}
        "input i {}"

      itPrettyPrints "an input type with a directive"
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionInputObject (InputObjectTypeDefinition {inputObjectTypeDefinitionName = Name {nameValue = "i"}, inputObjectTypeDefinitionDirectives = Just (Directives {directivesValue = [Directive {directiveName = Name {nameValue = "d"}, directiveArguments = Nothing}]}), inputObjectTypeDefinitionFields = InputFieldDefinitions {inputFieldDefinitionsValue = []}})))]}
        "input i @d {}"

      itPrettyPrints "an input type with a field"
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionInputObject (InputObjectTypeDefinition {inputObjectTypeDefinitionName = Name {nameValue = "i"}, inputObjectTypeDefinitionDirectives = Nothing, inputObjectTypeDefinitionFields = InputFieldDefinitions {inputFieldDefinitionsValue = [InputValueDefinition {inputValueDefinitionName = Name {nameValue = "f"}, inputValueDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "t"}}), inputValueDefinitionDefaultValue = Nothing, inputValueDefinitionDirectives = Nothing}]}})))]}
        "input i {f: t}"

      itPrettyPrints "a type extension"
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionTypeExtension (TypeExtensionDefinition {typeExtensionDefinitionValue = ObjectTypeDefinition {objectTypeDefinitionName = Name {nameValue = "t"}, objectTypeDefinitionInterfaces = Nothing, objectTypeDefinitionDirectives = Nothing, objectTypeDefinitionFields = FieldDefinitions {fieldDefinitionsValue = []}}}))]}
        "extend type t {}"

      itPrettyPrints "a directive definition"
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionDirective (DirectiveDefinition {directiveDefinitionName = Name {nameValue = "d"}, directiveDefinitionArguments = Nothing, directiveDefinitionLocations = DirectiveLocations {directiveLocationsValue = [Name {nameValue = "t"}]}}))]}
        "directive @d on t"

      itPrettyPrints "the schema kitchen sink"
        Document {documentValue = [DefinitionTypeSystem (TypeSystemDefinitionSchema (SchemaDefinition {schemaDefinitionDirectives = Nothing, schemaDefinitionOperationTypes = OperationTypeDefinitions {operationTypeDefinitionsValue = [OperationTypeDefinition {operationTypeDefinitionOperation = OperationTypeQuery, operationTypeDefinitionType = NamedType {namedTypeValue = Name {nameValue = "QueryType"}}},OperationTypeDefinition {operationTypeDefinitionOperation = OperationTypeMutation, operationTypeDefinitionType = NamedType {namedTypeValue = Name {nameValue = "MutationType"}}}]}})),DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionObject (ObjectTypeDefinition {objectTypeDefinitionName = Name {nameValue = "Foo"}, objectTypeDefinitionInterfaces = Just (Interfaces {interfacesValue = NamedType {namedTypeValue = Name {nameValue = "Bar"}} :| []}), objectTypeDefinitionDirectives = Nothing, objectTypeDefinitionFields = FieldDefinitions {fieldDefinitionsValue = [FieldDefinition {fieldDefinitionName = Name {nameValue = "one"}, fieldDefinitionArguments = Nothing, fieldDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "Type"}}), fieldDefinitionDirectives = Nothing},FieldDefinition {fieldDefinitionName = Name {nameValue = "two"}, fieldDefinitionArguments = Just (InputValueDefinitions {inputValueDefinitionsValue = [InputValueDefinition {inputValueDefinitionName = Name {nameValue = "argument"}, inputValueDefinitionType = TypeNonNull (NonNullTypeNamed (NamedType {namedTypeValue = Name {nameValue = "InputType"}})), inputValueDefinitionDefaultValue = Nothing, inputValueDefinitionDirectives = Nothing}]}), fieldDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "Type"}}), fieldDefinitionDirectives = Nothing},FieldDefinition {fieldDefinitionName = Name {nameValue = "three"}, fieldDefinitionArguments = Just (InputValueDefinitions {inputValueDefinitionsValue = [InputValueDefinition {inputValueDefinitionName = Name {nameValue = "argument"}, inputValueDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "InputType"}}), inputValueDefinitionDefaultValue = Nothing, inputValueDefinitionDirectives = Nothing},InputValueDefinition {inputValueDefinitionName = Name {nameValue = "other"}, inputValueDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "String"}}), inputValueDefinitionDefaultValue = Nothing, inputValueDefinitionDirectives = Nothing}]}), fieldDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "Int"}}), fieldDefinitionDirectives = Nothing},FieldDefinition {fieldDefinitionName = Name {nameValue = "four"}, fieldDefinitionArguments = Just (InputValueDefinitions {inputValueDefinitionsValue = [InputValueDefinition {inputValueDefinitionName = Name {nameValue = "argument"}, inputValueDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "String"}}), inputValueDefinitionDefaultValue = Just (DefaultValue {defaultValueValue = ValueString "string"}), inputValueDefinitionDirectives = Nothing}]}), fieldDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "String"}}), fieldDefinitionDirectives = Nothing},FieldDefinition {fieldDefinitionName = Name {nameValue = "five"}, fieldDefinitionArguments = Just (InputValueDefinitions {inputValueDefinitionsValue = [InputValueDefinition {inputValueDefinitionName = Name {nameValue = "argument"}, inputValueDefinitionType = TypeList (ListType {listTypeValue = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "String"}})}), inputValueDefinitionDefaultValue = Just (DefaultValue {defaultValueValue = ValueList [ValueString "string",ValueString "string"]}), inputValueDefinitionDirectives = Nothing}]}), fieldDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "String"}}), fieldDefinitionDirectives = Nothing},FieldDefinition {fieldDefinitionName = Name {nameValue = "six"}, fieldDefinitionArguments = Just (InputValueDefinitions {inputValueDefinitionsValue = [InputValueDefinition {inputValueDefinitionName = Name {nameValue = "argument"}, inputValueDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "InputType"}}), inputValueDefinitionDefaultValue = Just (DefaultValue {defaultValueValue = ValueObject [ObjectField {objectFieldName = Name {nameValue = "key"}, objectFieldValue = ValueString "value"}]}), inputValueDefinitionDirectives = Nothing}]}), fieldDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "Type"}}), fieldDefinitionDirectives = Nothing},FieldDefinition {fieldDefinitionName = Name {nameValue = "seven"}, fieldDefinitionArguments = Just (InputValueDefinitions {inputValueDefinitionsValue = [InputValueDefinition {inputValueDefinitionName = Name {nameValue = "argument"}, inputValueDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "Int"}}), inputValueDefinitionDefaultValue = Just (DefaultValue {defaultValueValue = ValueNull}), inputValueDefinitionDirectives = Nothing}]}), fieldDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "Type"}}), fieldDefinitionDirectives = Nothing}]}}))),DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionObject (ObjectTypeDefinition {objectTypeDefinitionName = Name {nameValue = "AnnotatedObject"}, objectTypeDefinitionInterfaces = Nothing, objectTypeDefinitionDirectives = Just (Directives {directivesValue = Directive {directiveName = Name {nameValue = "onObject"}, directiveArguments = Just (Arguments {argumentsValue = [Argument {argumentName = Name {nameValue = "arg"}, argumentValue = ValueString "value"}]})} :| []}), objectTypeDefinitionFields = FieldDefinitions {fieldDefinitionsValue = [FieldDefinition {fieldDefinitionName = Name {nameValue = "annotatedField"}, fieldDefinitionArguments = Just (InputValueDefinitions {inputValueDefinitionsValue = [InputValueDefinition {inputValueDefinitionName = Name {nameValue = "arg"}, inputValueDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "Type"}}), inputValueDefinitionDefaultValue = Just (DefaultValue {defaultValueValue = ValueString "default"}), inputValueDefinitionDirectives = Just (Directives {directivesValue = Directive {directiveName = Name {nameValue = "onArg"}, directiveArguments = Nothing} :| []})}]}), fieldDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "Type"}}), fieldDefinitionDirectives = Just (Directives {directivesValue = Directive {directiveName = Name {nameValue = "onField"}, directiveArguments = Nothing} :| []})}]}}))),DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionInterface (InterfaceTypeDefinition {interfaceTypeDefinitionName = Name {nameValue = "Bar"}, interfaceTypeDefinitionDirectives = Nothing, interfaceTypeDefinitionFields = FieldDefinitions {fieldDefinitionsValue = [FieldDefinition {fieldDefinitionName = Name {nameValue = "one"}, fieldDefinitionArguments = Nothing, fieldDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "Type"}}), fieldDefinitionDirectives = Nothing},FieldDefinition {fieldDefinitionName = Name {nameValue = "four"}, fieldDefinitionArguments = Just (InputValueDefinitions {inputValueDefinitionsValue = [InputValueDefinition {inputValueDefinitionName = Name {nameValue = "argument"}, inputValueDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "String"}}), inputValueDefinitionDefaultValue = Just (DefaultValue {defaultValueValue = ValueString "string"}), inputValueDefinitionDirectives = Nothing}]}), fieldDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "String"}}), fieldDefinitionDirectives = Nothing}]}}))),DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionInterface (InterfaceTypeDefinition {interfaceTypeDefinitionName = Name {nameValue = "AnnotatedInterface"}, interfaceTypeDefinitionDirectives = Just (Directives {directivesValue = Directive {directiveName = Name {nameValue = "onInterface"}, directiveArguments = Nothing} :| []}), interfaceTypeDefinitionFields = FieldDefinitions {fieldDefinitionsValue = [FieldDefinition {fieldDefinitionName = Name {nameValue = "annotatedField"}, fieldDefinitionArguments = Just (InputValueDefinitions {inputValueDefinitionsValue = [InputValueDefinition {inputValueDefinitionName = Name {nameValue = "arg"}, inputValueDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "Type"}}), inputValueDefinitionDefaultValue = Nothing, inputValueDefinitionDirectives = Just (Directives {directivesValue = Directive {directiveName = Name {nameValue = "onArg"}, directiveArguments = Nothing} :| []})}]}), fieldDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "Type"}}), fieldDefinitionDirectives = Just (Directives {directivesValue = Directive {directiveName = Name {nameValue = "onField"}, directiveArguments = Nothing} :| []})}]}}))),DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionUnion (UnionTypeDefinition {unionTypeDefinitionName = Name {nameValue = "Feed"}, unionTypeDefinitionDirectives = Nothing, unionTypeDefinitionTypes = UnionTypes {unionTypesValue = NamedType {namedTypeValue = Name {nameValue = "Story"}} :| [NamedType {namedTypeValue = Name {nameValue = "Article"}},NamedType {namedTypeValue = Name {nameValue = "Advert"}}]}}))),DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionUnion (UnionTypeDefinition {unionTypeDefinitionName = Name {nameValue = "AnnotatedUnion"}, unionTypeDefinitionDirectives = Just (Directives {directivesValue = Directive {directiveName = Name {nameValue = "onUnion"}, directiveArguments = Nothing} :| []}), unionTypeDefinitionTypes = UnionTypes {unionTypesValue = NamedType {namedTypeValue = Name {nameValue = "A"}} :| [NamedType {namedTypeValue = Name {nameValue = "B"}}]}}))),DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionScalar (ScalarTypeDefinition {scalarTypeDefinitionName = Name {nameValue = "CustomScalar"}, scalarTypeDefinitionDirectives = Nothing}))),DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionScalar (ScalarTypeDefinition {scalarTypeDefinitionName = Name {nameValue = "AnnotatedScalar"}, scalarTypeDefinitionDirectives = Just (Directives {directivesValue = Directive {directiveName = Name {nameValue = "onScalar"}, directiveArguments = Nothing} :| []})}))),DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionEnum (EnumTypeDefinition {enumTypeDefinitionName = Name {nameValue = "Site"}, enumTypeDefinitionDirectives = Nothing, enumTypeDefinitionValues = EnumValues {enumValuesValue = [EnumValueDefinition {enumValueDefinitionName = Name {nameValue = "DESKTOP"}, enumValueDefinitionDirectives = Nothing},EnumValueDefinition {enumValueDefinitionName = Name {nameValue = "MOBILE"}, enumValueDefinitionDirectives = Nothing}]}}))),DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionEnum (EnumTypeDefinition {enumTypeDefinitionName = Name {nameValue = "AnnotatedEnum"}, enumTypeDefinitionDirectives = Just (Directives {directivesValue = Directive {directiveName = Name {nameValue = "onEnum"}, directiveArguments = Nothing} :| []}), enumTypeDefinitionValues = EnumValues {enumValuesValue = [EnumValueDefinition {enumValueDefinitionName = Name {nameValue = "ANNOTATED_VALUE"}, enumValueDefinitionDirectives = Just (Directives {directivesValue = Directive {directiveName = Name {nameValue = "onEnumValue"}, directiveArguments = Nothing} :| []})},EnumValueDefinition {enumValueDefinitionName = Name {nameValue = "OTHER_VALUE"}, enumValueDefinitionDirectives = Nothing}]}}))),DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionInputObject (InputObjectTypeDefinition {inputObjectTypeDefinitionName = Name {nameValue = "InputType"}, inputObjectTypeDefinitionDirectives = Nothing, inputObjectTypeDefinitionFields = InputFieldDefinitions {inputFieldDefinitionsValue = [InputValueDefinition {inputValueDefinitionName = Name {nameValue = "key"}, inputValueDefinitionType = TypeNonNull (NonNullTypeNamed (NamedType {namedTypeValue = Name {nameValue = "String"}})), inputValueDefinitionDefaultValue = Nothing, inputValueDefinitionDirectives = Nothing},InputValueDefinition {inputValueDefinitionName = Name {nameValue = "answer"}, inputValueDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "Int"}}), inputValueDefinitionDefaultValue = Just (DefaultValue {defaultValueValue = ValueInt 42}), inputValueDefinitionDirectives = Nothing}]}}))),DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionInputObject (InputObjectTypeDefinition {inputObjectTypeDefinitionName = Name {nameValue = "AnnotatedInput"}, inputObjectTypeDefinitionDirectives = Just (Directives {directivesValue = Directive {directiveName = Name {nameValue = "onInputObjectType"}, directiveArguments = Nothing} :| []}), inputObjectTypeDefinitionFields = InputFieldDefinitions {inputFieldDefinitionsValue = [InputValueDefinition {inputValueDefinitionName = Name {nameValue = "annotatedField"}, inputValueDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "Type"}}), inputValueDefinitionDefaultValue = Nothing, inputValueDefinitionDirectives = Just (Directives {directivesValue = Directive {directiveName = Name {nameValue = "onField"}, directiveArguments = Nothing} :| []})}]}}))),DefinitionTypeSystem (TypeSystemDefinitionTypeExtension (TypeExtensionDefinition {typeExtensionDefinitionValue = ObjectTypeDefinition {objectTypeDefinitionName = Name {nameValue = "Foo"}, objectTypeDefinitionInterfaces = Nothing, objectTypeDefinitionDirectives = Nothing, objectTypeDefinitionFields = FieldDefinitions {fieldDefinitionsValue = [FieldDefinition {fieldDefinitionName = Name {nameValue = "seven"}, fieldDefinitionArguments = Just (InputValueDefinitions {inputValueDefinitionsValue = [InputValueDefinition {inputValueDefinitionName = Name {nameValue = "argument"}, inputValueDefinitionType = TypeList (ListType {listTypeValue = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "String"}})}), inputValueDefinitionDefaultValue = Nothing, inputValueDefinitionDirectives = Nothing}]}), fieldDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "Type"}}), fieldDefinitionDirectives = Nothing}]}}})),DefinitionTypeSystem (TypeSystemDefinitionTypeExtension (TypeExtensionDefinition {typeExtensionDefinitionValue = ObjectTypeDefinition {objectTypeDefinitionName = Name {nameValue = "Foo"}, objectTypeDefinitionInterfaces = Nothing, objectTypeDefinitionDirectives = Just (Directives {directivesValue = Directive {directiveName = Name {nameValue = "onType"}, directiveArguments = Nothing} :| []}), objectTypeDefinitionFields = FieldDefinitions {fieldDefinitionsValue = []}}})),DefinitionTypeSystem (TypeSystemDefinitionType (TypeDefinitionObject (ObjectTypeDefinition {objectTypeDefinitionName = Name {nameValue = "NoFields"}, objectTypeDefinitionInterfaces = Nothing, objectTypeDefinitionDirectives = Nothing, objectTypeDefinitionFields = FieldDefinitions {fieldDefinitionsValue = []}}))),DefinitionTypeSystem (TypeSystemDefinitionDirective (DirectiveDefinition {directiveDefinitionName = Name {nameValue = "skip"}, directiveDefinitionArguments = Just (InputValueDefinitions {inputValueDefinitionsValue = [InputValueDefinition {inputValueDefinitionName = Name {nameValue = "if"}, inputValueDefinitionType = TypeNonNull (NonNullTypeNamed (NamedType {namedTypeValue = Name {nameValue = "Boolean"}})), inputValueDefinitionDefaultValue = Nothing, inputValueDefinitionDirectives = Nothing}]}), directiveDefinitionLocations = DirectiveLocations {directiveLocationsValue = Name {nameValue = "FIELD"} :| [Name {nameValue = "FRAGMENT_SPREAD"},Name {nameValue = "INLINE_FRAGMENT"}]}})),DefinitionTypeSystem (TypeSystemDefinitionDirective (DirectiveDefinition {directiveDefinitionName = Name {nameValue = "include"}, directiveDefinitionArguments = Just (InputValueDefinitions {inputValueDefinitionsValue = [InputValueDefinition {inputValueDefinitionName = Name {nameValue = "if"}, inputValueDefinitionType = TypeNonNull (NonNullTypeNamed (NamedType {namedTypeValue = Name {nameValue = "Boolean"}})), inputValueDefinitionDefaultValue = Nothing, inputValueDefinitionDirectives = Nothing}]}), directiveDefinitionLocations = DirectiveLocations {directiveLocationsValue = Name {nameValue = "FIELD"} :| [Name {nameValue = "FRAGMENT_SPREAD"},Name {nameValue = "INLINE_FRAGMENT"}]}}))]}
        [here|schema {query: QueryType mutation: MutationType}
type Foo implements Bar {one: Type two(argument: InputType!): Type three(argument: InputType other: String): Int four(argument: String = "string"): String five(argument: [String] = ["string" "string"]): String six(argument: InputType = {key: "value"}): Type seven(argument: Int = null): Type}
type AnnotatedObject @onObject (arg: "value") {annotatedField(arg: Type = "default" @onArg): Type @onField}
interface Bar {one: Type four(argument: String = "string"): String}
interface AnnotatedInterface @onInterface {annotatedField(arg: Type @onArg): Type @onField}
union Feed = Story | Article | Advert
union AnnotatedUnion @onUnion = A | B
scalar CustomScalar
scalar AnnotatedScalar @onScalar
enum Site {DESKTOP MOBILE}
enum AnnotatedEnum @onEnum {ANNOTATED_VALUE @onEnumValue OTHER_VALUE}
input InputType {key: String! answer: Int = 42}
input AnnotatedInput @onInputObjectType {annotatedField: Type @onField}
extend type Foo {seven(argument: [String]): Type}
extend type Foo @onType {}
type NoFields {}
directive @skip (if: Boolean!) on FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT
directive @include (if: Boolean!) on FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT|]

  _ <- checkParallel $$(discover)
  pure ()


itParses :: HasCallStack => String -> Text -> Document -> Spec
itParses description input document =
  it ("parses " ++ description)
    (shouldParse (parseDocument input) document)


itPrettyPrints :: HasCallStack => String -> Document -> Text -> Spec
itPrettyPrints description document output =
  it ("pretty prints " ++ description)
    (shouldBe (prettyPrintDocument document) output)


prop_round_trip :: Property
prop_round_trip = withTests 1000 . property $ do
  document <- forAll genDocument
  tripping document prettyPrintDocument parseDocument
