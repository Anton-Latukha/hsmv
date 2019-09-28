-- |

module Bar where

import Data.List
import Bar
import Foo.Bar
import  qualified   Bar
import   qualified   "wibble" Bar
import Bar as X
import  qualified   Bar (blob)
import  qualified   Foo.Bar (blob)
import   qualified   "wibble" Bar as X (y)
