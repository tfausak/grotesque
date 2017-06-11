{-# LANGUAGE QuasiQuotes #-}

import Grotesque

import Data.Text (Text)

import qualified Criterion as C
import qualified Criterion.Main as C
import qualified Data.Text as Text
import qualified Text.Heredoc as Heredoc

main :: IO ()
main = C.defaultMain
  [ C.bench "parseDocument" (C.nf parseDocument text)
  , C.bench "prettyPrintDocument" (C.nf prettyPrintDocument document)
  ]

text :: Text
text = Text.pack [Heredoc.here|
# https://github.com/graphql/graphql-js/blob/c4f26569acdb9a7dfbceb48b9786b46d65a4b11b/src/language/__tests__/kitchen-sink.graphql
# Copyright (c) 2015, Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

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

# https://raw.githubusercontent.com/graphql/graphql-js/c4f26569acdb9a7dfbceb48b9786b46d65a4b11b/src/language/__tests__/schema-kitchen-sink.graphql
# Copyright (c) 2015, Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

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

document :: Document
Right document = parseDocument text
