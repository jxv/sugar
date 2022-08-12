
![Logo](./assets/sugar_logo.svg)

Sugar is a pretty, sweet data language. Alternative to: JSON, YAML, TOML, et cetera.

[![Hackage](https://img.shields.io/hackage/v/sugar.svg)](http://hackage.haskell.org/package/sugar) [![Build Status](https://github.com/jxv/sugar/actions/workflows/main.yml/badge.svg?branch=main)](https://github.com/jxv/sugar/actions/workflows/main.yml)

---

### Basic idea

There is one type represented by 4 values: `Unit`, `Text`, `List`, & `Map`. _Optional: Attaching a variadic note_

```
sg ::= unit <sg...>? | text <sg...>?  | [sg] <sg...>?| {sg:sg} <sg...>?
```
___

### Quick examples

`Text` to `Text` pairs

```racket
; Top-level of a file is a `Map`.

; `key0` and `key1` are text types. `value0` and "value one within..." are text types too.
key0 value0
key1 "value one within quotes for spaces"
```

`Text` to `List` pairs

```racket
; key to list
key2 (list of 4 items)
key3 (nested [list of values])
```

`Text` to `Map` pairs

```racket
; key to Map
key4 {keyA valueA keyB valueB}
key5 {keyA {nestedKey nestedValue} keyB valueB}
```

Non-`Text` keys in pairs

```racket
#| Keys aren't limited to text types. Lists can be used as the key.
   Maps can also be used as the key. |#
(list as keys) value
{id0 value0 id1 value1} value
```

Note examples

```rakcet
; A note can annotate any data type by immediately following it.
; And a note captures any data type.
key6<note> value6<"note as text again">
key7<can be (a list)> value7<{or be a key value pair}>
{key8 key9}<still-works-here> {and here with unit}<()>
```

More examples are found [here](https://github.com/jxv/sugar/tree/main/examples).

---

### Data

| Type | Example |
| ----- | ------------ |
| Unit | `()` |
| Text | `textWithoutSpace` or `text-with-dashes_and_underscores` or `"text with spaces"` |
| List | `[item1 item2 item3]` or `(i1 i2 i3)` or `[i1, i2, i3]` or `[i1, i2, i3,]`|
| Map | `{}` or `{key1 value1}` or `{k1 v1 k2 v2}` or `{k1 v1, k2 v2}` or `{k1 v1, k2 v2,}`|
| Note | `()<note>` or `text<note note>` or`(a b c)<{note note}>` or `{k v}<note (a b c) () {et cetera}>` |

_\*Notice the lack of number and boolean types. Unit can be thought of as null._

### Comments

| Type | Example |
| ---- | ------- |
| Single-line | `; comment` |
| Multi-line | ```#\| comment content \|#``` |

---

### Other tips

* Top level of a file is already a `Map` but without curly braces
* `Map` can have non-unique keys
* `Map` must maintain pair ordering
* `()` is a `Unit` and `( )` is an empty `List` because of the whitespace
* Each comma in `List` or `Map` is optional
* Formatting (or lack thereof) is a preference by the user
