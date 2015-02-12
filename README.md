# purescript-jtable

A small but powerful Purescript library to render heterogeneous arrays of JSON into HTML nodes that display multi-dimensional tables. The rendering degrades gracefully for flat data (to ordinary tables).


Turn this:

```json
{
  "userId": 8927524,
  "profile": {
    "name":   "Mary Jane",
    "age":    29,
    "gender": "female"
  },
  "comments": [{
    "id":       "F2372BAC",
    "text":     "I concur.",
    "replyTo":  [9817361, "F8ACD164F"],
    "time":     "2015-02-03"
  }, {
    "id":       "GH732AFC",
    "replyTo":  [9654726, "A44124F"],
    "time":     "2015-03-01"
  }]
}
```

Into this:

```
|----------|----------------------------|------------------------------------------------------------|
|          |           profile          |                           comments                         |
|----------|-----------|-----|----------|----------|--------------|---------------------|------------|
|  userId  |    name   | age |  gender  |    id    |     text     |       replyTo       |    time    |
|----------|-----------|-----|----------|----------|--------------|---------------------|------------|
|  8927524 | Mary Jane |  29 |  female  | F2372BAC | I concur.    | 9817361 | F8ACD164F | 2015-02-03 |
|          |           |     |          |----------|--------------|---------------------|------------|
|          |           |     |          | GH732AFC |              | 9654726 | A44124F   | 2015-03-01 |
|----------|-----------|-----|----------|------------------------------------------------------------|
```

[Try it](/examples/try.html) with your own data or some samples.


## How?

TODO: API

The `Json` and `JCursor` data types that appear in the API are from [purescript-argonaut](https://github.com/purescript-contrib/purescript-argonaut). The `Markup` data type that appears in the API is from [purescript-smolder](https://github.com/bodil/purescript-smolder). 

Both of these libraries are listed as dependencies in [bower.json](bower.json).


## Tests

TODO.

The library contains an extensive suite of unit tests written using [purescript-strongcheck](https://github.com/purescript-contrib/purescript-strongcheck), which verify correct translation of nested, heterogeneous, array-filled JSON data into clean tabular markup.
