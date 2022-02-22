# Sugar

Legible data

🚧 API will break

___

### Example

```sugar
; This is a comment
; This snippet shows the top-level map of key-value pairs.
; It is the default.

; key0 and key1 are text types. value0 and "value one within..." are text types too.
key0 value0
key1 "value one within quotes for spaces"

; key to list
key2 (list of 4 items)
key3 (nested [list of values])

; key to Map
key4 {keyA valueA keyB valueB}
key5 {keyA {nestedKey nestedValue} keyB valueB}

#| Key aren't limited to text types. Lists can be used as the key.
   Maps can also be used as the key. #|
(list as keys) value
{id0 value0 id1 value1} value

; A note can annotate any data type by immediately following it.
; And a note captures any data type.
key6<note> value6<"note as text again">
key7<(can be a list)> value7<{or be a key value pair}>
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
| Note | `()<note>` or `text<note>` or`(a b c)<note>` or `{k v}<note>` |

_\*Notice the lack of number and boolean types. Unit can be thought of as null._

### Comments

| Type | Example |
| ---- | ------- |
| Single-line | `; comment` |
| Multi-line | `#\| comment content \|#` |


