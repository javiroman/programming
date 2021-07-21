# An Introduction to JSON

JSON - short for JavaScript Object Notation — is a format for sharing data. As
its name suggests, JSON is derived from the JavaScript programming language.

The initial target for JSON format was the follwoing description:

* When exchanging data between a browser and a server, the data can only be text.  
* JSON is text, and we can convert any JavaScript object into JSON, and send JSON
to the server.
* We can also convert any JSON received from the server into JavaScript objects.
* This way we can work with the data as JavaScript objects, with no complicated
parsing and translations.

Currently JSON is a text format that is completely language independent but
uses conventions that are familiar to programmers of the C-family of languages,
including C, C++, C#, Java, JavaScript, Perl, Python, and many others. These
properties make JSON an ideal data-interchange language.

JSON is built on two structures:

1. A collection of name/value pairs. In various languages, this is realized as an
object, record, struct, dictionary, hash table, keyed list, or associative
array.
2. An ordered list of values. In most languages, this is realized as an array,
vector, list, or sequence.

These are universal data structures. Virtually all modern programming languages
support them in one form or another. It makes sense that a data format that is
interchangeable with programming languages also be based on these structures.

A JSON object is a key-value data format that is typically rendered in curly
braces.

In simplest form a JSON object looks something like this:

```
{
  "first_name" : "Sammy",
  "last_name" : "Shark",
  "location" : "Ocean",
  "online" : true,
  "followers" : 987 
}
```

Key-value pairs have a colon between them as in "key" : "value". Each key-value
pair is separated by a comma, so the middle of a JSON looks like this: "key" :
"value", "key" : "value", "key": "value". 

JSON can be expecified in one single line, but writing the JSON format on
multiple lines often makes it much more readable, especially when dealing with
a large data set. Because JSON ignores whitespace between its elements, you can
space out your colons and key-value pairs in order to make the data even more
human readable.

## JSON keys

JSON keys are on the left side of the colon. They need to be wrapped in double
quotation marks, as in "key", and can be any valid string. Within each object,
keys need to be unique. These key strings can include whitespaces, as in "first
name", but that can make it harder to access when you’re programming, so it’s
best to use underscores, as in "first_name".

## JSON values

JSON values are found to the right of the colon. At the granular level, these
need to be one of 6 simple data types:

1. strings
2. numbers
3. objects
4. arrays
5. Booleans (true or false)
6. null

At the broader level, values can also be made up of the complex data types of
JSON object or array,

## Complex Types in JSON

### Nested Objects

In the users.json file below, for each of the four users ("sammy", "jesse",
"drew", "jamie") there is a nested JSON object passed as the value for each of
the users, with its own nested keys.

```
{ 
  "sammy" : {
    "username"  : "SammyShark",
    "location"  : "Indian Ocean",
    "online"    : true,
    "followers" : 987
  },
  "jesse" : {
    "username"  : "JesseOctopus",
    "location"  : "Pacific Ocean",
    "online"    : false,
    "followers" : 432
  },
  "drew" : {
    "username"  : "DrewSquid",
    "location"  : "Atlantic Ocean",
    "online"    : false,
    "followers" : 321
  },
  "jamie" : {
    "username"  : "JamieMantisShrimp",
    "location"  : "Pacific Ocean",
    "online"    : true,
    "followers" : 654
  }
}
```

Note: Just like any other value, when using objects, commas are used to 
separate elements.


### Nested Arrays

Data can also be nested within the JSON format by using JavaScript arrays that 
are passed as a value. JavaScript uses square brackets [ ] on either end of its 
array type. Arrays are ordered collections and can contain values of differing 
data types.

We may use an array when we are dealing with a lot of data that can be easily 
grouped together, like when there are various websites and social media 
profiles associated with a single user.

With the first nested array highlighted, a user profile for Sammy may look 
like this:

```
{ 
  "first_name" : "Sammy",
  "last_name" : "Shark",
  "location" : "Ocean",
  "websites" : [ 
    {
      "description" : "work",
      "URL" : "https://www.digitalocean.com/"
    },
    {
      "desciption" : "tutorials",
      "URL" : "https://www.digitalocean.com/community/tutorials"
    }
  ],
  "social_media" : [
    {
      "description" : "twitter",
      "link" : "https://twitter.com/digitalocean"
    },
    {
      "description" : "facebook",
      "link" : "https://www.facebook.com/DigitalOceanCloudHosting"
    },
    {
      "description" : "github",
      "link" : "https://github.com/digitalocean"
    }
  ]
}
```

The "websites" key and "social_media" key each use an array to nest information 
belonging to Sammy’s 2 website links and 3 social media profile links. We know 
that those are arrays because of the use of square brackets.

## JSON Schema

JSON Schema itself is written in JSON. It is data itself, not a computer
program. It’s just a declarative format for “describing the structure of other
data”. This is both its strength and its weakness (which it shares with other
similar schema languages). It is easy to concisely describe the surface
structure of data, and automate validating data against it. However, since a
JSON Schema can’t contain arbitrary code, there are certain constraints on the
relationships between data elements that can’t be expressed. Any “validation
tool” for a sufficiently complex data format, therefore, will likely have two
phases of validation: one at the schema (or structural) level, and one at the
semantic level. The latter check will likely need to be implemented using a
more general-purpose programming language.



