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

### Rules of XML

#### Invalid, valid, and well-formed documents
There are three kinds of XML documents:
- **Invalid documents** don't follow the syntax rules defined by the XML specification.
- **Valid documents** follow both the XML syntax rules and the rules defined in their DTD or schema.
- **Well-formed documents** follow the XML syntax rules but don't have a DTD or schema.

#### The root element
An XML document must be contained in a single element: the **root element**

Legal example:
```xml
<?xml version="1.0"?>
<!-- A well-formed document -->
<greeting>
  Hello, World!
</greeting>
```

Illegal example:
```xml
<?xml version="1.0"?>
<!-- An invalid document -->
<greeting>
  Hello, World!
</greeting>
<greeting>
  Hola, el Mundo!
</greeting>
```

#### Element's can't overlap
Illegal example:
```xml
<!-- NOT legal XML markup -->
<p>
  <b>I <i>really
  love</b> XML.
  </i>
</p>
```

Legal example:
```xml
<!-- legal XML markup -->
<p>
  <b>I <i>really
  love</i></b>
  <i>XML.</i>
</p>
```

#### End tags are required
Illegal example:
```xml
<!-- NOT legal XML markup -->
<p>Yada yada yada...
<p>
