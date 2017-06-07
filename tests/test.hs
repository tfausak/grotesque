{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.Text
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Grotesque
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Heredoc


main :: IO ()
main = hspec . parallel $ do

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

    itParses "the kitchen sink"
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
      "... f on t {}"

    itPrettyPrints "a fragment definition with a directive"
      Document { documentValue = [ DefinitionFragment FragmentDefinition { fragmentName = FragmentName { fragmentNameValue = Name { nameValue = "f" } } , fragmentTypeCondition = TypeCondition { typeConditionValue = NamedType { namedTypeValue = Name { nameValue = "t" } } } , fragmentDirectives = Just Directives { directivesValue = [ Directive { directiveName = Name { nameValue = "d" } , directiveArguments = Nothing } ] } , fragmentSelectionSet = SelectionSet { selectionSetValue = [] } } ] }
      "... f on t @d {}"

    itPrettyPrints "the kitchen sink"
      Document {documentValue = [DefinitionOperation (OperationDefinition {operationDefinitionOperationType = OperationTypeQuery, operationDefinitionName = Just (Name {nameValue = "queryName"}), operationDefinitionVariableDefinitions = Just (VariableDefinitions {variableDefinitionsValue = [VariableDefinition {variableDefinitionVariable = Variable {variableValue = Name {nameValue = "foo"}}, variableDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "ComplexType"}}), variableDefinitionDefaultValue = Nothing},VariableDefinition {variableDefinitionVariable = Variable {variableValue = Name {nameValue = "site"}}, variableDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "Site"}}), variableDefinitionDefaultValue = Just (DefaultValue {defaultValueValue = ValueEnum (Name {nameValue = "MOBILE"})})}]}), operationDefinitionDirectives = Nothing, operationDefinitionSelectionSet = SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Just (Alias {aliasValue = Name {nameValue = "whoever123is"}}), fieldName = Name {nameValue = "node"}, fieldArguments = Just (Arguments {argumentsValue = [Argument {argumentName = Name {nameValue = "id"}, argumentValue = ValueList [ValueInt 123,ValueInt 456]}]}), fieldDirectives = Nothing, fieldSelectionSet = Just (SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "id"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Nothing}),SelectionInlineFragment (InlineFragment {inlineFragmentTypeCondition = Just (TypeCondition {typeConditionValue = NamedType {namedTypeValue = Name {nameValue = "User"}}}), inlineFragmentDirectives = Just (Directives {directivesValue = [Directive {directiveName = Name {nameValue = "defer"}, directiveArguments = Nothing}]}), inlineFragmentSelectionSet = SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "field2"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Just (SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "id"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Nothing}),SelectionField (Field {fieldAlias = Just (Alias {aliasValue = Name {nameValue = "alias"}}), fieldName = Name {nameValue = "field1"}, fieldArguments = Just (Arguments {argumentsValue = [Argument {argumentName = Name {nameValue = "first"}, argumentValue = ValueInt 10},Argument {argumentName = Name {nameValue = "after"}, argumentValue = ValueVariable (Variable {variableValue = Name {nameValue = "foo"}})}]}), fieldDirectives = Just (Directives {directivesValue = [Directive {directiveName = Name {nameValue = "include"}, directiveArguments = Just (Arguments {argumentsValue = [Argument {argumentName = Name {nameValue = "if"}, argumentValue = ValueVariable (Variable {variableValue = Name {nameValue = "foo"}})}]})}]}), fieldSelectionSet = Just (SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "id"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Nothing}),SelectionFragmentSpread (FragmentSpread {fragmentSpreadName = FragmentName {fragmentNameValue = Name {nameValue = "frag"}}, fragmentSpreadDirectives = Nothing})]})})]})})]}}),SelectionInlineFragment (InlineFragment {inlineFragmentTypeCondition = Nothing, inlineFragmentDirectives = Just (Directives {directivesValue = [Directive {directiveName = Name {nameValue = "skip"}, directiveArguments = Just (Arguments {argumentsValue = [Argument {argumentName = Name {nameValue = "unless"}, argumentValue = ValueVariable (Variable {variableValue = Name {nameValue = "foo"}})}]})}]}), inlineFragmentSelectionSet = SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "id"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Nothing})]}}),SelectionInlineFragment (InlineFragment {inlineFragmentTypeCondition = Nothing, inlineFragmentDirectives = Nothing, inlineFragmentSelectionSet = SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "id"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Nothing})]}})]})})]}}),DefinitionOperation (OperationDefinition {operationDefinitionOperationType = OperationTypeMutation, operationDefinitionName = Just (Name {nameValue = "likeStory"}), operationDefinitionVariableDefinitions = Nothing, operationDefinitionDirectives = Nothing, operationDefinitionSelectionSet = SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "like"}, fieldArguments = Just (Arguments {argumentsValue = [Argument {argumentName = Name {nameValue = "story"}, argumentValue = ValueInt 123}]}), fieldDirectives = Just (Directives {directivesValue = [Directive {directiveName = Name {nameValue = "defer"}, directiveArguments = Nothing}]}), fieldSelectionSet = Just (SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "story"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Just (SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "id"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Nothing})]})})]})})]}}),DefinitionOperation (OperationDefinition {operationDefinitionOperationType = OperationTypeSubscription, operationDefinitionName = Just (Name {nameValue = "StoryLikeSubscription"}), operationDefinitionVariableDefinitions = Just (VariableDefinitions {variableDefinitionsValue = [VariableDefinition {variableDefinitionVariable = Variable {variableValue = Name {nameValue = "input"}}, variableDefinitionType = TypeNamed (NamedType {namedTypeValue = Name {nameValue = "StoryLikeSubscribeInput"}}), variableDefinitionDefaultValue = Nothing}]}), operationDefinitionDirectives = Nothing, operationDefinitionSelectionSet = SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "storyLikeSubscribe"}, fieldArguments = Just (Arguments {argumentsValue = [Argument {argumentName = Name {nameValue = "input"}, argumentValue = ValueVariable (Variable {variableValue = Name {nameValue = "input"}})}]}), fieldDirectives = Nothing, fieldSelectionSet = Just (SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "story"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Just (SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "likers"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Just (SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "count"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Nothing})]})}),SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "likeSentence"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Just (SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "text"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Nothing})]})})]})})]})})]}}),DefinitionFragment (FragmentDefinition {fragmentName = FragmentName {fragmentNameValue = Name {nameValue = "frag"}}, fragmentTypeCondition = TypeCondition {typeConditionValue = NamedType {namedTypeValue = Name {nameValue = "Friend"}}}, fragmentDirectives = Nothing, fragmentSelectionSet = SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "foo"}, fieldArguments = Just (Arguments {argumentsValue = [Argument {argumentName = Name {nameValue = "size"}, argumentValue = ValueVariable (Variable {variableValue = Name {nameValue = "size"}})},Argument {argumentName = Name {nameValue = "bar"}, argumentValue = ValueVariable (Variable {variableValue = Name {nameValue = "b"}})},Argument {argumentName = Name {nameValue = "obj"}, argumentValue = ValueObject [ObjectField {objectFieldName = Name {nameValue = "key"}, objectFieldValue = ValueString "value"}]}]}), fieldDirectives = Nothing, fieldSelectionSet = Nothing})]}}),DefinitionOperation (OperationDefinition {operationDefinitionOperationType = OperationTypeQuery, operationDefinitionName = Nothing, operationDefinitionVariableDefinitions = Nothing, operationDefinitionDirectives = Nothing, operationDefinitionSelectionSet = SelectionSet {selectionSetValue = [SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "unnamed"}, fieldArguments = Just (Arguments {argumentsValue = [Argument {argumentName = Name {nameValue = "truthy"}, argumentValue = ValueBoolean True},Argument {argumentName = Name {nameValue = "falsey"}, argumentValue = ValueBoolean False},Argument {argumentName = Name {nameValue = "nullish"}, argumentValue = ValueNull}]}), fieldDirectives = Nothing, fieldSelectionSet = Nothing}),SelectionField (Field {fieldAlias = Nothing, fieldName = Name {nameValue = "query"}, fieldArguments = Nothing, fieldDirectives = Nothing, fieldSelectionSet = Nothing})]}})]}
      [here|query queryName ($foo: ComplexType $site: Site = MOBILE) {whoever123is: node (id: [123 456]) {id ... on User @defer {field2 {id alias: field1 (first: 10 after: $foo) @include (if: $foo) {id ... frag}}} ... @skip (unless: $foo) {id} ... {id}}}
mutation likeStory {like (story: 123) @defer {story {id}}}
subscription StoryLikeSubscription ($input: StoryLikeSubscribeInput) {storyLikeSubscribe (input: $input) {story {likers {count} likeSentence {text}}}}
... frag on Friend {foo (size: $size bar: $b obj: {key: "value"})}
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


itParses :: HasCallStack => String -> Text -> Document -> Spec
itParses description input document =
  it ("parses " ++ description)
    (shouldParse
      (parse getDocument "" input)
      document)


itPrettyPrints :: HasCallStack => String -> Document -> Text -> Spec
itPrettyPrints description document output =
  it ("pretty prints " ++ description)
    (shouldBe
      (renderStrict (layoutPretty (LayoutOptions Unbounded) (prettyDocument document)))
      output)
