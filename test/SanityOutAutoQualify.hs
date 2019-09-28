-- |

module Bar where

import Data.List
import Bar as Foo
import Foo.Bar
import  qualified   Bar as Foo
import   qualified   "wibble" Bar as Foo
import Bar as X
import  qualified   Bar  as Foo(blob)
import  qualified   Foo.Bar (blob)
import   qualified   "wibble" Bar as X (y)
