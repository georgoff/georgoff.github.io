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
<p>Yada yada yada...
<p>...
```

#### Empty elements
If an element contains no markup at all it is called an **empty element**. In empty elements in XML documents, you can put the closing slash in the start tag:
```xml
<!-- Two equivalent break elements -->
<br></br>
<br />

<!-- Two equivalent image elements -->
<img src="../img/c.gif"></img>
<img src="../img/c.gif" />
```

#### Elements are case sensitive
In HTML, `<h1>` and `<H1>` are the same; in XML, they're not:
```xml
<!-- NOT legal XML markup -->
<h1>Elements are
  case sensitive</H1>

<!-- legal XML markup -->
<h1>Elements are
  case sensitive</h1>
```

#### Attributes must have quoted values
There are two rules for attributes in XML documents:
- Attributes must have values
- Those values must be enclosed within quotation marks

```xml
<!-- NOT legal XML markup -->
<ol compact>

<!-- legal XML markup -->
<ol compact="yes">
```

You can use either single or double quotes, as long as you're consistent.

If the value of the attribute contains a single or double quote, you can use the other kind of quote to surround the value (as in `name="Doug's car"`), or use the entities `&quot;` for a double quote and `&apos;` for a single quote. An _entity_ is a symbol, such as `&quot;`, the the XML parser replaces with other text, such as `"`.

#### XML declarations
XML declarations
- Provide basic information about the document to the parser
- Most documents start with them
- Recommended, but not required
- If present, must be the first thing in the document

The declaration can contain up to three name-value pairs:
- `version`
   - The version of XML used
- `encoding`
   - The character set used in the document
   - Example: `ISO-8859-1` includes all of the characters used by most Western European languages
   - If no `encoding` is specified, the parser assumes the `UTF-8` set, which includes virtually every character and ideograph from the world's languages
- `standalone`
   - `yes` or `no`
   - Defines whether the document can be processed without reading any other files
   - Default is `no`

#### Other things in XML documents
##### Comments
- Can appear anywhere in the document, even before or after the root element
- Begins with `<!--` and ends with `-->`
- Can't contain a double hyphen ( `--` ) except at the end
- Any markup inside a comment is ignored

##### Processing Instructions
- Markup intended for a particular piece of code
Example:
```xml
<!-- Here's a PI for Cocoon: -->
<?cocoon-process type="sql"?>
```
In the above example, Cocoon looks for processing instructions that begin with `cocoon-process`, then processes the XML document accordingly. In this example, the `type="sql"` attribute tells Cocoon that the XML document contains a SQL statement.

##### Entities
```xml
<!-- Here's an entity: -->
<!ENTITY dw "developerWorks">
```
The above example defines an _entity_ for the document. Anywhere the XML processor finds the string `&dw;`, it replaces the entity with the string `developerWorks`. The XML spec also defines five entities you can use in place of various special characters:
| entity | character | written meaning |
| :---: | :---: | :---: |
| `&lt;` | `<` | less-than sign |
| `&gt;` | `>` | greater-than sign |
| `&quot;` | `"` | double-quote |
| `&apos;` | `'` | single-quote (apostrophe) |
| `&amp;` | `&` | ampersand |
