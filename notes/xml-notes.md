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

##### Namespaces
Elements are used to describe the data within them, but sometimes they can have multiple meanings. For example, `<title>` could refer to:
- a person's courtesy title
- the title of a book
- the title to a piece of property
How do you tell if a given `<title>` element refers to a person, a book, or a piece of property? With _namespaces_.

To use a namespace, you define a _namespace prefix_ and map it to a particular string. Here's an example:
```xml
<?xml version="1.0"?>
<customer_summary
  xmlns:addr="http://www.xyz.com/addresses/"
  xmlns:books="http://www.zyx.com/books/"
  xmlns:mortgage="http://www.yyz.com/title/"
>
... <addr:name><title>Mrs.</title> ... </addr:name> ...
... <books:title>Lord of the Rings</books:title> ...
... <mortgage:title>NC2948-388-1983</mortgage:title> ...
```

In this example, the three namespace prefixes are `addr`, `books`, and `mortgage`. Notice that defining a namespace for a particular element means that all of its child elements belong to the same namespace. The first `<title>` element belongs to the `addr` namespace because its parent element, `<addr:Name>`, does.

**The string in a namespace definition is just a string**. The only thing that's important about the namespace string is that it's unique; that's why most namespace definitions look like URLs. The XML parser does not go to `http://www.zyx.com/books/` to search for DTD or schema, it simply uses that text as a string.

### Defining Document Content
There are two ways of defining document content:
- **Document Type Definition**, or **DTD**. A DTD defines:
   - the elements that can appear in an XML document
   - the order in which they can appear
   - how they can be nested inside each other
   - other basic details of XML document structure
- **XML Schema**. A schema can define all of the document structures that you can put in a DTD, and it can also define data types and more complicated rules than a DTD can.

#### Document Type Definitions
A DTD allows you to specify the basic structure of an XML document. Here's an example:
```xml
<!-- address.dtd -->
<!ELEMENT address (name, street, city, state, postal-code)>
<!ELEMENT name (title?, first-name, last-name)>
<!ELEMENT title (#PCDATA)>
<!ELEMENT first-name (#PCDATA)>
<!ELEMENT last-name (#PCDATA)>
<!ELEMENT street (#PCDATA)>
<!ELEMENT city (#PCDATA)>
<!ELEMENT state (#PCDATA)>
<!ELEMENT postal-code (#PCDATA)>
```
This DTD defines all of the elements used in the document. It defines three basic things:
- An `<address>` elements contains a `<name>`, a `<street>`, a `<city>`, a `<state>`, and a `<postal-code>`
   - All of those elements must appear, and they must appear in that order
- A `<name>` element contains an optional `<title>` element (the question mark means the title is optional), followed by a `<first-name>` and a `<last-name>` element
- All of the other elements contain text
   - `#PCDATA` stands for parsed character data; you can't include another element in these elements

The DTD makes it clear what combinations of elements are legal. An address document that has a `<postal-code>` element before the `<state>` element isn't legal, and neither is one that has no `<last-name>` element.

Note that **DTD syntax is different from ordinary XML syntax.**

##### Symbols in DTDs
There are a few symbols used in DTDs to indicate how often (or whether) something may appear in an XML document
- **Comma** (`,`) - indicates a list of items
   - Example: `<!ELEMENT address (name, city, state)>`
      - The `<address>` element must contain a `<name>`, a `<city>`, and a `<state>` element, in that order
      - All of the elements are required
- **Question Mark** (`?`) - indicates that an item is optional; it can appear once or not at all
   - Example: `<!ELEMENT name (title?, first-name, last-name)>`
      - The `<name>` element contains an optional `<title>` element, followed by a mandatory `<first-name>` and a `<last-name>` element
- **Plus Sign** (`+`) - indicates that an item must appear at least once, but can appear any number of times
   - Example: `<!ELEMENT addressbook (address+)>`
      - An `<addressbook>` element contains one or more `<address>` elements
      - You can have as many `<address>` elements as you need, but there has to be at least one
- **Asterisk** (`*`) - indicates that an item can appear any number of times, including zero
   - Example: `<!ELEMENT private-addresses (address*)>`
      - A `<private-addresses>` element contains zero or more `<address>` elements
- **Vertical Bar** (`|`) - indicates a list of choices; you can choose only one item from the list
   - Example: `<!ELEMENT name (title?, first-name, (middle-initial | middle-name)?, last-name)>`
      - A `<name>` element contains an optional `<title>` element, followed by a `<first-name>` element, possibly followed by either a `<middle-initial>` or a `<middle-name>` element, followed by a `<last-name>` element
      - Both `<middle-initial>` and `<middle-name>` are optional, and you can have only one of the two
   - Example: `<!ELEMENT name ((title?, first-name, last-name) | (surname, mothers-name, given-name))>`
      - The `<name>` element can contain one of two sequences:
        - An optional `<title>`, followed by a `<first-name>` and a `<last-name>`
        - A `<surname>`, a `<mothers-name>`, and a `<given-name>`

##### Defining Attributes
You can define attributes for the elements that will appear in your XML document. Using a DTD, you can also:
- Define which attributes are required
- Define default values for attributes
- List all of the valid values for a given attribute

Example:
```xml
<!ELEMENT city (#PCDATA)>
<!ATTLIST city state CDATA #REQUIRED>
```

The `ATTLIST` declaration _lists the attributes_ of the element. The name `city` inside the attribute list tells the parser that these attributes are defined for the `<city>` element.

The name `state` is the name of the attribute, and the keywords `CDATA` and `#REQUIRED` tell the parser that the `state` attribute contains text and is required (if it's optional, `CDATA #IMPLIED` will do the trick).

To define multiple attributes for an element:
```xml
<!ELEMENT city (#PCDATA)>
<!ATTLIST city state CDATA #REQUIRED
               postal-code CDATA #REQUIRED>
```

Finally, DTDs allow you to define default values for attributes and enumerate all of the valid values for an attribute:
```xml
<!ELEMENT city (#PCDATA)>
<!ATTLIST city state CDATA (AZ|CA|NV|OR|UT|WA) "CA">
```

In this example, the default state is California.