# Grotesque

[![Build badge][]][build]

> A style of ornamentation characterized by fanciful combinations of
> intertwined forms.

Grotesque is an implementation of [GraphQL][] in Haskell. It is a work in
progress. Currently it can parse and pretty-print GraphQL queries and schemas.
For example:

``` hs
:set -XOverloadedStrings
:module + Grotesque

let Right document = parseDocument "{ field }"
-- Document { documentValue =
--   [ DefinitionOperation OperationDefinition
--     { operationDefinitionOperationType = OperationTypeQuery
--     , operationDefinitionName = Nothing
--     , operationDefinitionVariableDefinitions = Nothing
--     , operationDefinitionDirectives = Nothing
--     , operationDefinitionSelectionSet = SelectionSet { selectionSetValue =
--       [ SelectionField Field
--         { fieldAlias = Nothing
--         , fieldName = Name { nameValue = "field" }
--         , fieldArguments = Nothing
--         , fieldDirectives = Nothing
--         , fieldSelectionSet = Nothing } ] } } ] }

prettyPrintDocument document
-- "query {field}"
```

If you're looking for a production-ready implementation, check out [the
reference implementation][] in JavaScript or [the C++ query parser][].

If you're looking for a more mature or featureful Haskell implementation, check
out [jdnavarro/graphql-haskell][], [dropbox/datagraph][], or
[jml/graphql-api][].

[Build badge]: https://travis-ci.org/tfausak/grotesque.svg?branch=master
[build]: https://travis-ci.org/tfausak/grotesque
[GraphQL]: http://graphql.org
[the reference implementation]: https://github.com/graphql/graphql-js
[the C++ query parser]: https://github.com/graphql/libgraphqlparser
[jdnavarro/graphql-haskell]: https://github.com/jdnavarro/graphql-haskell
[dropbox/datagraph]: https://github.com/dropbox/datagraph
[jml/graphql-api]: https://github.com/jml/graphql-api
