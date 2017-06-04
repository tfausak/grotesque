import Grotesque
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

main :: IO ()
main = hspec . parallel $ do

  test
    "parses an empty document"
    " "
    Document
      { documentValue = []
      }

  test
    "parses an empty query shorthand"
    " { } "
    Document
      { documentValue =
        [ DefinitionOperation OperationDefinition
          { operationDefinitionOperationType = OperationTypeQuery
          , operationDefinitionName = Nothing
          , operationDefinitionVariableDefinitions = Nothing
          , operationDefinitionDirectives = Nothing
          , operationDefinitionSelectionSet = SelectionSet
            { selectionSetValue = []
            }
          }
        ]
      }

  test
    "parses a field name"
    " { f } "
    Document
      { documentValue =
        [ DefinitionOperation OperationDefinition
          { operationDefinitionOperationType = OperationTypeQuery
          , operationDefinitionName = Nothing
          , operationDefinitionVariableDefinitions = Nothing
          , operationDefinitionDirectives = Nothing
          , operationDefinitionSelectionSet = SelectionSet
            { selectionSetValue =
              [ SelectionField Field
                { fieldAlias = Nothing
                , fieldName = Name
                  { nameValue = "f"
                  }
                , fieldArguments = Nothing
                , fieldDirectives = Nothing
                , fieldSelectionSet = Nothing
                }
              ]
            }
          }
        ]
      }

  test
    "parses a field alias"
    " { a : f } "
    Document
      { documentValue =
        [ DefinitionOperation OperationDefinition
          { operationDefinitionOperationType = OperationTypeQuery
          , operationDefinitionName = Nothing
          , operationDefinitionVariableDefinitions = Nothing
          , operationDefinitionDirectives = Nothing
          , operationDefinitionSelectionSet = SelectionSet
            { selectionSetValue =
              [ SelectionField Field
                { fieldAlias = Just Alias
                  { aliasValue = Name
                    { nameValue = "a"
                    }
                  }
                , fieldName = Name
                  { nameValue = "f"
                  }
                , fieldArguments = Nothing
                , fieldDirectives = Nothing
                , fieldSelectionSet = Nothing
                }
              ]
            }
          }
        ]
      }

  test
    "parses empty field arguments"
    " { f ( ) } "
    Document
      { documentValue =
        [ DefinitionOperation OperationDefinition
          { operationDefinitionOperationType = OperationTypeQuery
          , operationDefinitionName = Nothing
          , operationDefinitionVariableDefinitions = Nothing
          , operationDefinitionDirectives = Nothing
          , operationDefinitionSelectionSet = SelectionSet
            { selectionSetValue =
              [ SelectionField Field
                { fieldAlias = Nothing
                , fieldName = Name
                  { nameValue = "f"
                  }
                , fieldArguments = Just Arguments
                  { argumentsValue = []
                  }
                , fieldDirectives = Nothing
                , fieldSelectionSet = Nothing
                }
              ]
            }
          }
        ]
      }

  test
    "parses a null field argument"
    " { f ( a : null ) } "
    Document
      { documentValue =
        [ DefinitionOperation OperationDefinition
          { operationDefinitionOperationType = OperationTypeQuery
          , operationDefinitionName = Nothing
          , operationDefinitionVariableDefinitions = Nothing
          , operationDefinitionDirectives = Nothing
          , operationDefinitionSelectionSet = SelectionSet
            { selectionSetValue =
              [ SelectionField Field
                { fieldAlias = Nothing
                , fieldName = Name
                  { nameValue = "f"
                  }
                , fieldArguments = Just Arguments
                  { argumentsValue =
                    [ Argument
                      { argumentName = Name
                        { nameValue = "a"
                        }
                      , argumentValue = ValueNull
                      }
                    ]
                  }
                , fieldDirectives = Nothing
                , fieldSelectionSet = Nothing
                }
              ]
            }
          }
        ]
      }

  test
    "parses a list field argument"
    " { f ( a : [ ] ) } "
    Document
      { documentValue =
        [ DefinitionOperation OperationDefinition
          { operationDefinitionOperationType = OperationTypeQuery
          , operationDefinitionName = Nothing
          , operationDefinitionVariableDefinitions = Nothing
          , operationDefinitionDirectives = Nothing
          , operationDefinitionSelectionSet = SelectionSet
            { selectionSetValue =
              [ SelectionField Field
                { fieldAlias = Nothing
                , fieldName = Name
                  { nameValue = "f"
                  }
                , fieldArguments = Just Arguments
                  { argumentsValue =
                    [ Argument
                      { argumentName = Name
                        { nameValue = "a"
                        }
                      , argumentValue = ValueList []
                      }
                    ]
                  }
                , fieldDirectives = Nothing
                , fieldSelectionSet = Nothing
                }
              ]
            }
          }
        ]
      }

  test
    "parses an enum field argument"
    " { f ( a : e ) } "
    Document
      { documentValue =
        [ DefinitionOperation OperationDefinition
          { operationDefinitionOperationType = OperationTypeQuery
          , operationDefinitionName = Nothing
          , operationDefinitionVariableDefinitions = Nothing
          , operationDefinitionDirectives = Nothing
          , operationDefinitionSelectionSet = SelectionSet
            { selectionSetValue =
              [ SelectionField Field
                { fieldAlias = Nothing
                , fieldName = Name
                  { nameValue = "f"
                  }
                , fieldArguments = Just Arguments
                  { argumentsValue =
                    [ Argument
                      { argumentName = Name
                        { nameValue = "a"
                        }
                      , argumentValue = ValueEnum Name
                        { nameValue = "e"
                        }
                      }
                    ]
                  }
                , fieldDirectives = Nothing
                , fieldSelectionSet = Nothing
                }
              ]
            }
          }
        ]
      }

  test
    "parses a boolean field argument"
    " { f ( a : false ) } "
    Document
      { documentValue =
        [ DefinitionOperation OperationDefinition
          { operationDefinitionOperationType = OperationTypeQuery
          , operationDefinitionName = Nothing
          , operationDefinitionVariableDefinitions = Nothing
          , operationDefinitionDirectives = Nothing
          , operationDefinitionSelectionSet = SelectionSet
            { selectionSetValue =
              [ SelectionField Field
                { fieldAlias = Nothing
                , fieldName = Name
                  { nameValue = "f"
                  }
                , fieldArguments = Just Arguments
                  { argumentsValue =
                    [ Argument
                      { argumentName = Name
                        { nameValue = "a"
                        }
                      , argumentValue = ValueBoolean False
                      }
                    ]
                  }
                , fieldDirectives = Nothing
                , fieldSelectionSet = Nothing
                }
              ]
            }
          }
        ]
      }

  test
    "parses a variable field argument"
    " { f ( a : $ v ) } "
    Document
      { documentValue =
        [ DefinitionOperation OperationDefinition
          { operationDefinitionOperationType = OperationTypeQuery
          , operationDefinitionName = Nothing
          , operationDefinitionVariableDefinitions = Nothing
          , operationDefinitionDirectives = Nothing
          , operationDefinitionSelectionSet = SelectionSet
            { selectionSetValue =
              [ SelectionField Field
                { fieldAlias = Nothing
                , fieldName = Name
                  { nameValue = "f"
                  }
                , fieldArguments = Just Arguments
                  { argumentsValue =
                    [ Argument
                      { argumentName = Name
                        { nameValue = "a"
                        }
                      , argumentValue = ValueVariable Variable
                        { variableValue = Name
                          { nameValue = "v"
                          }
                        }
                      }
                    ]
                  }
                , fieldDirectives = Nothing
                , fieldSelectionSet = Nothing
                }
              ]
            }
          }
        ]
      }

  test
    "parses an int field argument"
    " { f ( a : 0 ) } "
    Document
      { documentValue =
        [ DefinitionOperation OperationDefinition
          { operationDefinitionOperationType = OperationTypeQuery
          , operationDefinitionName = Nothing
          , operationDefinitionVariableDefinitions = Nothing
          , operationDefinitionDirectives = Nothing
          , operationDefinitionSelectionSet = SelectionSet
            { selectionSetValue =
              [ SelectionField Field
                { fieldAlias = Nothing
                , fieldName = Name
                  { nameValue = "f"
                  }
                , fieldArguments = Just Arguments
                  { argumentsValue =
                    [ Argument
                      { argumentName = Name
                        { nameValue = "a"
                        }
                      , argumentValue = ValueInt 0
                      }
                    ]
                  }
                , fieldDirectives = Nothing
                , fieldSelectionSet = Nothing
                }
              ]
            }
          }
        ]
      }

  test
    "parses a float field argument"
    " { f ( a : 1.2 ) } "
    Document
      { documentValue =
        [ DefinitionOperation OperationDefinition
          { operationDefinitionOperationType = OperationTypeQuery
          , operationDefinitionName = Nothing
          , operationDefinitionVariableDefinitions = Nothing
          , operationDefinitionDirectives = Nothing
          , operationDefinitionSelectionSet = SelectionSet
            { selectionSetValue =
              [ SelectionField Field
                { fieldAlias = Nothing
                , fieldName = Name
                  { nameValue = "f"
                  }
                , fieldArguments = Just Arguments
                  { argumentsValue =
                    [ Argument
                      { argumentName = Name
                        { nameValue = "a"
                        }
                      , argumentValue = ValueFloat 1.2
                      }
                    ]
                  }
                , fieldDirectives = Nothing
                , fieldSelectionSet = Nothing
                }
              ]
            }
          }
        ]
      }

  test
    "parses a string field argument"
    " { f ( a : \"\" ) } "
    Document
      { documentValue =
        [ DefinitionOperation OperationDefinition
          { operationDefinitionOperationType = OperationTypeQuery
          , operationDefinitionName = Nothing
          , operationDefinitionVariableDefinitions = Nothing
          , operationDefinitionDirectives = Nothing
          , operationDefinitionSelectionSet = SelectionSet
            { selectionSetValue =
              [ SelectionField Field
                { fieldAlias = Nothing
                , fieldName = Name
                  { nameValue = "f"
                  }
                , fieldArguments = Just Arguments
                  { argumentsValue =
                    [ Argument
                      { argumentName = Name
                        { nameValue = "a"
                        }
                      , argumentValue = ValueString ""
                      }
                    ]
                  }
                , fieldDirectives = Nothing
                , fieldSelectionSet = Nothing
                }
              ]
            }
          }
        ]
      }

  test
    "parses an object field argument"
    " { f ( a : { } ) } "
    Document
      { documentValue =
        [ DefinitionOperation OperationDefinition
          { operationDefinitionOperationType = OperationTypeQuery
          , operationDefinitionName = Nothing
          , operationDefinitionVariableDefinitions = Nothing
          , operationDefinitionDirectives = Nothing
          , operationDefinitionSelectionSet = SelectionSet
            { selectionSetValue =
              [ SelectionField Field
                { fieldAlias = Nothing
                , fieldName = Name
                  { nameValue = "f"
                  }
                , fieldArguments = Just Arguments
                  { argumentsValue =
                    [ Argument
                      { argumentName = Name
                        { nameValue = "a"
                        }
                      , argumentValue = ValueObject []
                      }
                    ]
                  }
                , fieldDirectives = Nothing
                , fieldSelectionSet = Nothing
                }
              ]
            }
          }
        ]
      }

  test
    "parses an object field argument with a field"
    " { f ( a : { k : null } ) } "
    Document
      { documentValue =
        [ DefinitionOperation OperationDefinition
          { operationDefinitionOperationType = OperationTypeQuery
          , operationDefinitionName = Nothing
          , operationDefinitionVariableDefinitions = Nothing
          , operationDefinitionDirectives = Nothing
          , operationDefinitionSelectionSet = SelectionSet
            { selectionSetValue =
              [ SelectionField Field
                { fieldAlias = Nothing
                , fieldName = Name
                  { nameValue = "f"
                  }
                , fieldArguments = Just Arguments
                  { argumentsValue =
                    [ Argument
                      { argumentName = Name
                        { nameValue = "a"
                        }
                      , argumentValue = ValueObject
                        [ ObjectField
                          { objectFieldName = Name
                            { nameValue = "k"
                            }
                          , objectFieldValue = ValueNull
                          }
                        ]
                      }
                    ]
                  }
                , fieldDirectives = Nothing
                , fieldSelectionSet = Nothing
                }
              ]
            }
          }
        ]
      }

  test
    "parses a query"
    " query { } "
    Document
      { documentValue =
        [ DefinitionOperation OperationDefinition
          { operationDefinitionOperationType = OperationTypeQuery
          , operationDefinitionName = Nothing
          , operationDefinitionVariableDefinitions = Nothing
          , operationDefinitionDirectives = Nothing
          , operationDefinitionSelectionSet = SelectionSet
            { selectionSetValue = []
            }
          }
        ]
      }

  test
    "parses a mutation"
    " mutation { } "
    Document
      { documentValue =
        [ DefinitionOperation OperationDefinition
          { operationDefinitionOperationType = OperationTypeMutation
          , operationDefinitionName = Nothing
          , operationDefinitionVariableDefinitions = Nothing
          , operationDefinitionDirectives = Nothing
          , operationDefinitionSelectionSet = SelectionSet
            { selectionSetValue = []
            }
          }
        ]
      }

  test
    "parses a subscription"
    " subscription { } "
    Document
      { documentValue =
        [ DefinitionOperation OperationDefinition
          { operationDefinitionOperationType = OperationTypeSubscription
          , operationDefinitionName = Nothing
          , operationDefinitionVariableDefinitions = Nothing
          , operationDefinitionDirectives = Nothing
          , operationDefinitionSelectionSet = SelectionSet
            { selectionSetValue = []
            }
          }
        ]
      }

  test
    "parses an empty variable definition"
    " query ( ) { } "
    Document
      { documentValue =
        [ DefinitionOperation OperationDefinition
          { operationDefinitionOperationType = OperationTypeQuery
          , operationDefinitionName = Nothing
          , operationDefinitionVariableDefinitions = Just VariableDefinitions
            { variableDefinitionsValue = []
            }
          , operationDefinitionDirectives = Nothing
          , operationDefinitionSelectionSet = SelectionSet
            { selectionSetValue = []
            }
          }
        ]
      }

  test
    "parses a variable definition with a named type"
    " query ( $ v : t ) { } "
    Document
      { documentValue =
        [ DefinitionOperation OperationDefinition
          { operationDefinitionOperationType = OperationTypeQuery
          , operationDefinitionName = Nothing
          , operationDefinitionVariableDefinitions = Just VariableDefinitions
            { variableDefinitionsValue =
              [ VariableDefinition
                { variableDefinitionVariable = Variable
                  { variableValue = Name
                    { nameValue = "v"
                    }
                  }
                , variableDefinitionType = TypeNamed NamedType
                  { namedTypeValue = Name
                    { nameValue = "t"
                    }
                  }
                , variableDefinitionDefaultValue = Nothing
                }
              ]
            }
          , operationDefinitionDirectives = Nothing
          , operationDefinitionSelectionSet = SelectionSet
            { selectionSetValue = []
            }
          }
        ]
      }

  test
    "parses a variable definition with a list type"
    " query ( $ v : [ t ] ) { } "
    Document
      { documentValue =
        [ DefinitionOperation OperationDefinition
          { operationDefinitionOperationType = OperationTypeQuery
          , operationDefinitionName = Nothing
          , operationDefinitionVariableDefinitions = Just VariableDefinitions
            { variableDefinitionsValue =
              [ VariableDefinition
                { variableDefinitionVariable = Variable
                  { variableValue = Name
                    { nameValue = "v"
                    }
                  }
                , variableDefinitionType = TypeList ListType
                  { listTypeValue = TypeNamed NamedType
                    { namedTypeValue = Name
                      { nameValue = "t"
                      }
                    }
                  }
                , variableDefinitionDefaultValue = Nothing
                }
              ]
            }
          , operationDefinitionDirectives = Nothing
          , operationDefinitionSelectionSet = SelectionSet
            { selectionSetValue = []
            }
          }
        ]
      }

  test
    "parses a variable definition with a non-null named type"
    " query ( $ v : t ! ) { } "
    Document
      { documentValue =
        [ DefinitionOperation OperationDefinition
          { operationDefinitionOperationType = OperationTypeQuery
          , operationDefinitionName = Nothing
          , operationDefinitionVariableDefinitions = Just VariableDefinitions
            { variableDefinitionsValue =
              [ VariableDefinition
                { variableDefinitionVariable = Variable
                  { variableValue = Name
                    { nameValue = "v"
                    }
                  }
                , variableDefinitionType = TypeNonNull (NonNullTypeNamed NamedType
                  { namedTypeValue = Name
                    { nameValue = "t"
                    }
                  })
                , variableDefinitionDefaultValue = Nothing
                }
              ]
            }
          , operationDefinitionDirectives = Nothing
          , operationDefinitionSelectionSet = SelectionSet
            { selectionSetValue = []
            }
          }
        ]
      }

  test
    "parses a variable definition with a non-null list type"
    " query ( $ v : [ t ] ! ) { } "
    Document
      { documentValue =
        [ DefinitionOperation OperationDefinition
          { operationDefinitionOperationType = OperationTypeQuery
          , operationDefinitionName = Nothing
          , operationDefinitionVariableDefinitions = Just VariableDefinitions
            { variableDefinitionsValue =
              [ VariableDefinition
                { variableDefinitionVariable = Variable
                  { variableValue = Name
                    { nameValue = "v"
                    }
                  }
                , variableDefinitionType = TypeNonNull (NonNullTypeList ListType
                  { listTypeValue = TypeNamed NamedType
                    { namedTypeValue = Name
                      { nameValue = "t"
                      }
                    }
                  })
                , variableDefinitionDefaultValue = Nothing
                }
              ]
            }
          , operationDefinitionDirectives = Nothing
          , operationDefinitionSelectionSet = SelectionSet
            { selectionSetValue = []
            }
          }
        ]
      }

  test
    "parses a variable definition with a default value"
    " query ( $ v : t = null ) { } "
    Document
      { documentValue =
        [ DefinitionOperation OperationDefinition
          { operationDefinitionOperationType = OperationTypeQuery
          , operationDefinitionName = Nothing
          , operationDefinitionVariableDefinitions = Just VariableDefinitions
            { variableDefinitionsValue =
              [ VariableDefinition
                { variableDefinitionVariable = Variable
                  { variableValue = Name
                    { nameValue = "v"
                    }
                  }
                , variableDefinitionType = TypeNamed NamedType
                  { namedTypeValue = Name
                    { nameValue = "t"
                    }
                  }
                , variableDefinitionDefaultValue = Just DefaultValue
                  { defaultValueValue = ValueNull
                  }
                }
              ]
            }
          , operationDefinitionDirectives = Nothing
          , operationDefinitionSelectionSet = SelectionSet
            { selectionSetValue = []
            }
          }
        ]
      }

  test
    "parses a directive"
    " query @ d { } "
    Document
      { documentValue =
        [ DefinitionOperation OperationDefinition
          { operationDefinitionOperationType = OperationTypeQuery
          , operationDefinitionName = Nothing
          , operationDefinitionVariableDefinitions = Nothing
          , operationDefinitionDirectives = Just Directives
            { directivesValue =
              [ Directive
                { directiveName = Name
                  { nameValue = "d"
                  }
                , directiveArguments = Nothing
                }
              ]
            }
          , operationDefinitionSelectionSet = SelectionSet
            { selectionSetValue = []
            }
          }
        ]
      }

  test
    "parses a directive with empty arguments"
    " query @ d ( ) { } "
    Document
      { documentValue =
        [ DefinitionOperation OperationDefinition
          { operationDefinitionOperationType = OperationTypeQuery
          , operationDefinitionName = Nothing
          , operationDefinitionVariableDefinitions = Nothing
          , operationDefinitionDirectives = Just Directives
            { directivesValue =
              [ Directive
                { directiveName = Name
                  { nameValue = "d"
                  }
                , directiveArguments = Just Arguments
                  { argumentsValue = []
                  }
                }
              ]
            }
          , operationDefinitionSelectionSet = SelectionSet
            { selectionSetValue = []
            }
          }
        ]
      }

  test
    "parses a directive with an argument"
    " query @ d ( a : null ) { } "
    Document
      { documentValue =
        [ DefinitionOperation OperationDefinition
          { operationDefinitionOperationType = OperationTypeQuery
          , operationDefinitionName = Nothing
          , operationDefinitionVariableDefinitions = Nothing
          , operationDefinitionDirectives = Just Directives
            { directivesValue =
              [ Directive
                { directiveName = Name
                  { nameValue = "d"
                  }
                , directiveArguments = Just Arguments
                  { argumentsValue =
                    [ Argument
                      { argumentName = Name
                        { nameValue = "a"
                        }
                      , argumentValue = ValueNull
                      }
                    ]
                  }
                }
              ]
            }
          , operationDefinitionSelectionSet = SelectionSet
            { selectionSetValue = []
            }
          }
        ]
      }

  test
    "parses a fragment spread"
    " { ... s } "
    Document
      { documentValue =
        [ DefinitionOperation OperationDefinition
          { operationDefinitionOperationType = OperationTypeQuery
          , operationDefinitionName = Nothing
          , operationDefinitionVariableDefinitions = Nothing
          , operationDefinitionDirectives = Nothing
          , operationDefinitionSelectionSet = SelectionSet
            { selectionSetValue =
              [ SelectionFragmentSpread FragmentSpread
                { fragmentSpreadName = FragmentName
                  { fragmentNameValue = Name
                    { nameValue = "s"
                    }
                  }
                , fragmentSpreadDirectives = Nothing
                }
              ]
            }
          }
        ]
      }

  test
    "parses a fragment spread with a directive"
    " { ... s @ d } "
    Document
      { documentValue =
        [ DefinitionOperation OperationDefinition
          { operationDefinitionOperationType = OperationTypeQuery
          , operationDefinitionName = Nothing
          , operationDefinitionVariableDefinitions = Nothing
          , operationDefinitionDirectives = Nothing
          , operationDefinitionSelectionSet = SelectionSet
            { selectionSetValue =
              [ SelectionFragmentSpread FragmentSpread
                { fragmentSpreadName = FragmentName
                  { fragmentNameValue = Name
                    { nameValue = "s"
                    }
                  }
                , fragmentSpreadDirectives = Just Directives
                  { directivesValue =
                    [ Directive
                      { directiveName = Name
                        { nameValue = "d"
                        }
                      , directiveArguments = Nothing
                      }
                    ]
                  }
                }
              ]
            }
          }
        ]
      }

  test
    "parses an inline fragment"
    " { ... { } } "
    Document
      { documentValue =
        [ DefinitionOperation OperationDefinition
          { operationDefinitionOperationType = OperationTypeQuery
          , operationDefinitionName = Nothing
          , operationDefinitionVariableDefinitions = Nothing
          , operationDefinitionDirectives = Nothing
          , operationDefinitionSelectionSet = SelectionSet
            { selectionSetValue =
              [ SelectionInlineFragment InlineFragment
                { inlineFragmentTypeCondition = Nothing
                , inlineFragmentDirectives = Nothing
                , inlineFragmentSelectionSet = SelectionSet
                  { selectionSetValue = []
                  }
                }
              ]
            }
          }
        ]
      }

  test
    "parses an inline fragment with a type condition"
    " { ... on t { } } "
    Document
      { documentValue =
        [ DefinitionOperation OperationDefinition
          { operationDefinitionOperationType = OperationTypeQuery
          , operationDefinitionName = Nothing
          , operationDefinitionVariableDefinitions = Nothing
          , operationDefinitionDirectives = Nothing
          , operationDefinitionSelectionSet = SelectionSet
            { selectionSetValue =
              [ SelectionInlineFragment InlineFragment
                { inlineFragmentTypeCondition = Just TypeCondition
                  { typeConditionValue = NamedType
                    { namedTypeValue = Name
                      { nameValue = "t"
                      }
                    }
                  }
                , inlineFragmentDirectives = Nothing
                , inlineFragmentSelectionSet = SelectionSet
                  { selectionSetValue = []
                  }
                }
              ]
            }
          }
        ]
      }

  test
    "parses an inline fragment with a directive"
    " { ... @ d { } } "
    Document
      { documentValue =
        [ DefinitionOperation OperationDefinition
          { operationDefinitionOperationType = OperationTypeQuery
          , operationDefinitionName = Nothing
          , operationDefinitionVariableDefinitions = Nothing
          , operationDefinitionDirectives = Nothing
          , operationDefinitionSelectionSet = SelectionSet
            { selectionSetValue =
              [ SelectionInlineFragment InlineFragment
                { inlineFragmentTypeCondition = Nothing
                , inlineFragmentDirectives = Just Directives
                  { directivesValue =
                    [ Directive
                      { directiveName = Name
                        { nameValue = "d"
                        }
                      , directiveArguments = Nothing
                      }
                    ]
                  }
                , inlineFragmentSelectionSet = SelectionSet
                  { selectionSetValue = []
                  }
                }
              ]
            }
          }
        ]
      }

  test
    "parses a fragment definition"
    " fragment f on t { } "
    Document
      { documentValue =
        [ DefinitionFragment FragmentDefinition
          { fragmentName = FragmentName
            { fragmentNameValue = Name
              { nameValue = "f"
              }
            }
          , fragmentTypeCondition = TypeCondition
            { typeConditionValue = NamedType
              { namedTypeValue = Name
                { nameValue = "t"
                }
              }
            }
          , fragmentDirectives = Nothing
          , fragmentSelectionSet = SelectionSet
            { selectionSetValue = []
            }
          }
        ]
      }

  test
    "parses a fragment definition with a directive"
    " fragment f on t @ d { } "
    Document
      { documentValue =
        [ DefinitionFragment FragmentDefinition
          { fragmentName = FragmentName
            { fragmentNameValue = Name
              { nameValue = "f"
              }
            }
          , fragmentTypeCondition = TypeCondition
            { typeConditionValue = NamedType
              { namedTypeValue = Name
                { nameValue = "t"
                }
              }
            }
          , fragmentDirectives = Just Directives
            { directivesValue =
              [ Directive
                { directiveName = Name
                  { nameValue = "d"
                  }
                , directiveArguments = Nothing
                }
              ]
            }
          , fragmentSelectionSet = SelectionSet
            { selectionSetValue = []
            }
          }
        ]
      }

test :: String -> String -> Document -> Spec
test description input document =
  it description (shouldParse (parse getDocument "" input) document)
