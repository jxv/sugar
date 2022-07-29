# json2sg

Lossy conversion from JSON to Sugar

#### Usage

Pass JSON through `stdin` and export out Sugar through `stdout`

```shell
json2sg < input.json > output.sg
```

## Format comparsion

JSON

```json
{
  "id": "62e32f511b845f8b3e1f577a",
  "index": 0,
  "quantity": 555.55,
  "guid": null,
  "isActive": false,
  "isExpired": true,
  "about": "Aliquip dolor adipisicing excepteur exercitation labore cupidatat non.",
  "tags": [
    "ex",
    "proident",
    "cupidatat"
  ],
  "nested": ["hello", ["world",	["!"]]],
  "assoc": [
    {
      "id": 0,
      "name": "duis"
    },
    {
      "id": 1,
      "name": "nisi"
    },
    {
      "id": 2,
      "name": "minim"
    }
  ]
}
```

Sugar

```racket
about "Aliquip dolor adipisicing excepteur exercitation labore cupidatat non."
assoc [
  {
    id 0
    name duis
  }
  {
    id 1
    name nisi
  }
  {
    id 2
    name minim
  }
]
guid ()
id 62e32f511b845f8b3e1f577a
index 0
isActive #f
isExpired #t
nested [hello (world [!])]
quantity 555.55
tags [ex proident cupidatat]
```