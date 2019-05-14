# XML Notes

### Tags, elements, and attributes

Here's an example:
```xml
<address>
  <name>
    <title>Mrs.</title>
    <first-name>
      Mary
    </first-name>
    <last-name>
      McGoon
    </last-name>
  </name>
  <street>
    1401 Main Street
  </street>
  <city state="NC">Anytown</city>
  <postal-code>
    34829
  </postal-code>
</address>
```

- A `tag` is the text between the `<` and the `>`
   - Starting tags use the format `<name>`
   - Ending tags use the format `</name>`
- An `element` is the `<`, `>`, and everything in between
   - `elements` can contain child `elements`
- An `attribute` is a name-value pair inside the starting `tag` of an `element`
   - In this example, `state` is an `attribute` of the `<city>` `element`
