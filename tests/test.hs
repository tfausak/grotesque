{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Grotesque
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

main :: IO ()
main = hspec . parallel $ do

  context "parser" $ do

    itParses
      "an empty document"
      " "
      Document
        { documentValue = []
        }

    itParses
      "an empty query shorthand"
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

    itParses
      "a field name"
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

    itParses
      "a field alias"
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

    itParses
      "empty field arguments"
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

    itParses
      "a null field argument"
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

    itParses
      "a list field argument"
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

    itParses
      "an enum field argument"
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

    itParses
      "a boolean field argument"
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

    itParses
      "a variable field argument"
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

    itParses
      "an int field argument"
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

    itParses
      "a float field argument"
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

    itParses
      "a string field argument"
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

    itParses
      "an object field argument"
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

    itParses
      "an object field argument with a field"
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

    itParses
      "a query"
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

    itParses
      "a mutation"
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

    itParses
      "a subscription"
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

    itParses
      "an empty variable definition"
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

    itParses
      "a variable definition with a named type"
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

    itParses
      "a variable definition with a list type"
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

    itParses
      "a variable definition with a non-null named type"
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

    itParses
      "a variable definition with a non-null list type"
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

    itParses
      "a variable definition with a default value"
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

    itParses
      "a directive"
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

    itParses
      "a directive with empty arguments"
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

    itParses
      "a directive with an argument"
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

    itParses
      "a fragment spread"
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

    itParses
      "a fragment spread with a directive"
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

    itParses
      "an inline fragment"
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

    itParses
      "an inline fragment with a type condition"
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

    itParses
      "an inline fragment with a directive"
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

    itParses
      "a fragment definition"
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

    itParses
      "a fragment definition with a directive"
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

  context "pretty printer" $ do

    itPrettyPrints
      "an empty document"
      Document
        { documentValue = []
        }
      ""

    itPrettyPrints
      "a query"
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
      "query    {}"

    itPrettyPrints
      "a field"
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
      "query    { f   }"

    itPrettyPrints
      "a field alias"
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
      "query    {a: f   }"

    itPrettyPrints
      "empty arguments"
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
      "query    { f ()  }"

    itPrettyPrints
      "a null"
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
      "query    { f (a: null)  }"

    itPrettyPrints
      "a list"
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
      "query    { f (a: [])  }"

    itPrettyPrints
      "an enum"
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
      "query    { f (a: e)  }"

    itPrettyPrints
      "a boolean"
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
      "query    { f (a: false)  }"

    itPrettyPrints
      "a variable"
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
      "query    { f (a: $v)  }"

    itPrettyPrints
      "an int"
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
      "query    { f (a: 0)  }"

    itPrettyPrints
      "a float"
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
      "query    { f (a: 1.2)  }"

    itPrettyPrints
      "a string"
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
      "query    { f (a: \"\")  }"

    itPrettyPrints
      "an object"
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
      "query    { f (a: {})  }"

    itPrettyPrints
      "an object with a field"
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
      "query    { f (a: {k: null})  }"

    itPrettyPrints
      "a mutation"
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
      "mutation    {}"

    itPrettyPrints
      "a subscription"
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
      "subscription    {}"

    itPrettyPrints
      "an empty variable definition"
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
      "query  ()  {}"

    itPrettyPrints
      "a named variable definition"
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
      "query  ($v: t )  {}"

    itPrettyPrints
      "a list variable definition"
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
      "query  ($v: [t] )  {}"

    itPrettyPrints
      "a non-null named variable definition"
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
      "query  ($v: t! )  {}"

    itPrettyPrints
      "a non-null list variable definition"
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
      "query  ($v: [t]! )  {}"

    itPrettyPrints
      "a variable definition with a default value"
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
      "query  ($v: t = null)  {}"

    itPrettyPrints
      "a directive"
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
      "query   @d  {}"

    itPrettyPrints
      "a directive with empty arguments"
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
      "query   @d () {}"

    itPrettyPrints
      "a directive with an argument"
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
      "query   @d (a: null) {}"

    itPrettyPrints
      "a fragment spread"
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
      "query    {... s }"

    itPrettyPrints
      "a fragment spread with a directive"
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
      "query    {... s @d }"

    itPrettyPrints
      "an inline fragment"
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
      "query    {...   {}}"

    itPrettyPrints
      "an inline fragment with a type condition"
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
      "query    {... on t  {}}"

    itPrettyPrints
      "an inline fragment with a directive"
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
      "query    {...  @d  {}}"

    itPrettyPrints
      "a fragment definition"
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
      "... f on t  {}"

    itPrettyPrints
      "a fragment definition with a directive"
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
      "... f on t @d  {}"

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
      (renderStrict (layoutPretty defaultLayoutOptions (prettyDocument document)))
      output)
