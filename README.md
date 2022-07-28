
![Logo](./assets/sugar_logo.svg)


Sugar is a general purpose data language for humans. Alternative to: JSON, YAML, TOML, et cetera.

---

### Basic idea

Sugar is one type represented by 4 values: `Unit`, `Text`, `List`, & `Map`.
And there is an option to attach a note on each value.

```
sg ::= unit <sg...>? | text <sg...>?  | [sg] <sg...>?| {sg:sg} <sg...>?
```
___

### Quick examples

More are found [here](https://github.com/jxv/sugar/tree/main/examples).

`Text` to `Text` pairs

```racket
; Top-level of file is a `Map`.

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

---

### Data

| Type | Example |
| ----- | ------------ |
| Unit | `()` |
| Text | `textWithoutSpace` or `text-with-dashes` or `"text with spaces"` |
| List | `(item1 item2 item3)` with parens or `[item1 item2 item3]` with square-brackets |
| Map | `{}` or `{key1 value1}` or `{key1 value1 key2 value2}` |
| Note | `()<note>` or `text<note note>` or`(a b c)<{note note}>` or `{k v}<note (a b c) () {et cetera}>` |

_\*Notice the lack of number and boolean types. Unit can be thought of as null._

### Comments

| Type | Example |
| ---- | ------- |
| Single-line | `; comment` |
| Multi-line | ```#\| comment content \|#``` |
