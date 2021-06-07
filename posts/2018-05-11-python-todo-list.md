---
author: Rebecca Skinner
title: Building A Todo List with Python and Flask
categories: ["python", "flask"]
tags: ["python", "flask", "practice", "projects"]
description: A guide on building a basic python application
---

# Introduction

NB: The full source code to this application is available [on
github](https://github.com/rebeccaskinner/py-todo).

The TODO List is a perrenial favorite of developers looking to learn a new
language, framework, or technique.  I recently joined a team that uses Python as
one of their languages, and decided to create my own implementation of a basic
todo list using python3 and Flask in order to hone my very rusty skills with
dynamically typed OO languages.  I decided to write up my thoughts here both to
help other developers who are interested in understanding how to get a start
with python, as well as offering a perspective to experienced python developers
on how those of us coming from outside the python ecosystem may approach solving
problems, and what hiccups we can encounter.

# Defining the API

The TODO list that we're building is going to focus on acting as a backend
service for some hypothetical rich frontend application.  We aren't concerned
with serving up HTML, CSS, or JavaScript.  Instead, we will concern ourselves
with implementing a simple JSON API, outlined below.

## The API Endpoint

The first thing I wanted to do as I was working through this project was to
ensure that my application's interfaces were well specified.  I wanted to limit
the scope of the project to something that could be accomplished in a few hours,
so rather than dive into understanding the python libraries for data interchange
formats like protobufs, and tools like swagger, I decided to create an ad-hoc
schema format for my API specification.  The schema describes both the routes
supported by the application, along with their HTTP methods and expected input
and output types, as well as describing the types themselves.  All data is
exchanged using JSON.

The schema that I'm using is:

```json
{
  "description": "human readable documentation string",
  "endpoints": {
    "routename": {
      "http-method": {
        "accepts": "expected json type",
        "returns": "json schema of the returned payload",
        "description": "human readable documentation string"
      }
    }
  },
  "types": {
    "type-name": {
      "description": "humand-readable documentation string",
      "type": "json type (object|array|number|string|bool) | typename",
      "fields": {
        "object-key-name": {
          "description": "object field description",
          "type": "json type (object|array|number|string|bool) | typename",
          "required": "bool"
        }
      },
      "values": ["allowed-values"]
      "key-type": "the type of keys in an object type"
      "value-type": "the type of the values in an object type"
    }
  }
}
```

Throughout the post I'll be incuding snippets of the JSON schema in the above
format to disambiguate any discussion around the contract for the TODO list API.

The API description is exposed at the `/api` route:

```json
"api": {
  "get": {
    "returns": "object",
    "description": "returns this json document"
  }
```

## Tickets

A ticket in our TODO is a JSON object with two fields:

- `status`: A textual description of the todo list item in human readable format
- `status`: A `status-string` type

The status of a ticket describes it's current status.  The status type is a
string enumeration consisting of the following values:

- `todo`: Indicates an item that has not been started
- `in-progress`: Indicates an item that has been started but not completed
- `abandon`: Indicates an item that was canceled before completing
- `done`: Indicates an item that has been completed

The full description of the `ticket-json` and `status-string` types are shown
below:

```json
{
  "ticket-json": {
    "description": "json document describing a single ticket",
    "type": "object",
    "fields": {
      "status": {
        "description": "the status of a ticket",
        "type": "status-string",
        "required": true
      },
      "summary": {
        "description": "human readable description of the ticket",
        "type": "string",
        "required": false
      }
    }
  },
  "status-string": {
    "description": "string enumeration of ticket status",
    "type": "string",
    "values": [
      "todo",
      "in-progress",
      "abandoned",
      "done"
    ]
  }
}
```

### The Ticket Map

Tickets in our application are referenced by ID numbers.  These ticket ID
numbers are simple JSON numbers, and we have created a type alias in our schema
for them to provide documentation:

```json
"ticket-id": {
  "description": "numeric id representing a ticket",
  "type": "number"
}
```

The `ticket-map` type represents our association between ticket IDs and the
actual ticket values.  It's a javascript object keyed on string representations
of the `ticket-id` type and containing `ticket-json` values:

```json
"ticket-map": {
  "description": "map of ticket IDs to values",
  "type": "object",
  "key-type": "ticket-id",
  "value-type": "ticket-json"
}
```

## TODO CRUD Operations

The TODO list supports basic CRUD operations.  We can view the todo list, either
in it's entirety or based on the state of the items.  Individual items in the
list can have their text updated, or can be moved from one state to another.
There is no limit on state transitions.

### Global Operations

```json
"items": {
  "get": {
    "returns": "ticket-map",
    "description": "returns a list of all todo list items"
  }
},
"pending": {
  "get": {
    "returns": "ticket-map",
    "description": "returns a list of all pending todo list items"
  }
},
"open": {
  "get": {
    "returns": "ticket-map",
    "description": "returns a list of all open todo list items"
  }
},
"complete": {
  "get": {
    "returns": "ticket-map",
    "description": "returns a list of all complete todo list items"
  }
},
"abandoned": {
  "get": {
    "returns": "ticket-map",
    "description": "returns a list of all abandoned todo list items"
  }
},
"add": {
  "post": {
    "accepts": "ticket-json",
    "returns": "ticket-id",
    "description": "adds a new todo list item"
  }
}
```

### Ticket Operations

Individual tickets can be accessed by a route equal to the ticket's
`ticket-id`.  Tickets support `GET`, `DELETE`, and `PUT` methods to fetch,
remove, and update them respectively.

```json
  "<ticket-id>": {
    "get": {
      "returns": "ticket-json",
      "description": "returns information about the given ticket"
    },
    "delete": {
      "description": "deletes a given ticket"
    },
    "put": {
      "accepts": "ticket-json",
      "returns": "ticket-json",
      "description": "updates ticket information"
    }
  }
```


# Defining the Data Model

We define three object types that will represent the data we need in our
application.  The `TodoStatus` type is a specialization of the `Enum` class that
provides a mechanism to parse and display specific string representations of our
enum values.  The `TodoItem` type is a simple mutable object that tracks a given
item's description and completion state.  The `List` type is a mutable
collection of associations between todo item indexes and the items themselves.
Each of these types are described in more detail below.

## The Status Type

Although python does not allow us to leverage a type system to detect errors at
compile time, we can still leverage the concept of sum types to allow us to
effective express the expected states of a todo item.  We accomplish this by
creating a subclass of the builtin enumeration type, `Enum`, and defining
numeric value mappings for each intended state.

```python
class TodoStatus(Enum):
    TODO = 0
    INPROGRESS = 1
    ABANDON = 2
    DONE = 3
```

For convenience, we wish for our TodoStatus to provide an instance of the
stringification method `__str__`, allowing us to use basic print formatting
strings to generate appropriate human-readable names.  In this case, we will
explicitly exist the application if we find ourselves in an unexpected state in
order to avoid hiding any bugs caused by the unsoundness of our type system:

```python
def __str__(self):
    if self == TodoStatus.TODO:
        return "todo"
    elif self == TodoStatus.INPROGRESS:
        return "in-progress"
    elif self == TodoStatus.ABANDON:
        return "abandoned"
    elif self == TodoStatus.DONE:
        return "done"
    else:
        print("enumeration invariant failed: value out of range")
        sys.exit()
```

Finally, as we will be wanting to parse user input in order to set the state of
a todo list item, we need to create a mapping from strings to our todo status.
We consider an invalid string to be part of the domain of our inputs, and return
`None` if we receive an invalid string.

Of particular note here is that we do not leverage a nullable container, as the
lack of compile-time enforcement of type constraints would render the benefits
of a nullable type nearly moot and would come at the cost of rather more verbose
interfaces.

```python
def parse(str):
    if str == "todo":
        return  TodoStatus.TODO
    elif str == "in-progress":
        return  TodoStatus.INPROGRESS
    elif str == "abandoned":
        return  TodoStatus.ABANDON
    elif str == "done":
        return  TodoStatus.DONE
    else:
        return None
```

### Testing The Ticket Status

We will create unit tests for stringification and parsing of our status types.
Due to the very limited nature these are simple tests with full coverage.

```python
class TestTodoStatus(unittest.TestCase):
    def test_to_string(self):
        self.assertEqual("todo", todo.TodoStatus.TODO.__str__())
        self.assertEqual("in-progress", todo.TodoStatus.INPROGRESS.__str__())
        self.assertEqual("abandoned", todo.TodoStatus.ABANDON.__str__())
        self.assertEqual("done", todo.TodoStatus.DONE.__str__())
    def test_parse_string(self):
        self.assertEqual(todo.TodoStatus.parse("todo"), todo.TodoStatus.TODO)
        self.assertEqual(todo.TodoStatus.parse("in-progress"), todo.TodoStatus.INPROGRESS)
        self.assertEqual(todo.TodoStatus.parse("abandoned"), todo.TodoStatus.ABANDON)
        self.assertEqual(todo.TodoStatus.parse("done"), todo.TodoStatus.DONE)
 ```

## The Todo Item Type

A todo list item is a mutable object that contains two fields: The `summary` is
the textual description of the contents of the todo list item.  The `status` is
the `TodoStatus` that describes the state of the ticket.

Todo items only have four basic operations:

- Get the summary
- Set the summary
- Get the status
- Set the status

We have three unit tests that cover these primary use cases:

```python
def test_new_item_sets_summary(self):
    summary1 = "test summary"
    item = todo.TodoItem(summary1)
    self.assertEqual(summary1, item.description())
def test_set_summary(self):
    summary1 = "test summary 1"
    summary2 = "test summary 2"
    item = todo.TodoItem(summary1)
    item.set_summary(summary2)
    self.assertEqual(summary2, item.description())
def test_new_item_status_is_todo(self):
    item = todo.TodoItem("summary1")
    self.assertEqual(todo.TodoStatus.TODO, item.status())
```

Our implementations are straightforward:

```python
class TodoItem:
    def __init__(self, summary):
        self._summary = summary
        self._status = TodoStatus.TODO
    def status(self):
        return self._status
    def set_status(self, status):
        self._status = status
    def description(self):
        return self._summary
    def set_summary(self, summary):
        self._summary = summary
```

We also provide several convenience methods that allow us to quickly set the
status to one of our pre-defined todo status types:

```python
def todo(self):
    self._status = TodoStatus.TODO
def inprogress(self):
    self._status = TodoStatus.INPROGRESS
def abandon(self):
    self._status = TodoStatus.ABANDON
def complete(self):
    self._status = TodoStatus.DONE
```

One challenge that we'll face when serving our API is the need to convert our
todo items into the appropriate JSON representation as defined by our API.
Python does not provide a reasoanble serialization combinator to allow us to
easily encode our object directly into JSON, but we may abuse heterogenous
dictionary types in order to generate an object whose natural serialization
format is equivalent to our desired output format.

To accomplish this we define a `json_dict` function that will generate a
dictionary whose keys are the desired key names of the generated JSON object,
and whose values are likewise defined according to our specification:

```python
def json_dict(self):
      return {"summary": self._summary, "status": self._status.__str__()}
```

## The Todo List Type

Our todo list is a simple container wrapping a dictionary keyed by ticket ID
numbers with values of type `TodoItem`. Our list type will support basic CRUD
operations as well as allowing us to filter and extract items based on their
completion state.

We start by writing tests that allow us to create an empty list, insert items
into it, and to verify that they exist:

```python
def test_new_list(self):
    list = todo.List()
    self.assertEqual(0, list.total_count())
def test_add_item_returns_element_index(self):
    list = todo.List()
    summary = "summary1"
    id = list.add_item(summary)
    self.assertEqual(summary, list.lookup(id).description())
```

Creating our basic list is simple.  We define two internal private variables,
`_list`, which is initialized to an empty dictionary, and `_idx`, a stateful
method of tracking the last inserted index.

```python
class List:
    def __init__(self):
        self._list = {}
        self._idx = 0
    def add_item(self, summary):
        idx = self._idx
        self._list[idx] = TodoItem(summary)
        self._idx = self._idx + 1
        return idx
    def lookup(self, idx):
        if idx not in self._list:
            return None
        return self._list[idx]
```

Next we provide several capabilities around looking at the total volume if
items, as well as removing items:

```python
def test_remove_item_when_item_exists(self):
    list = todo.List()
    idx = list.add_item("summary")
    list.remove_item(idx)
    self.assertEqual(None, list.lookup(idx))
def test_remove_item_when_item_not_exists(self):
    list = todo.List()
    list.remove_item(999)
def test_list_size_increases_when_new_item_added(self):
    list = todo.List()
    list.add_item("summary")
    self.assertEqual(1, list.total_count())
def test_list_todo_item_shows_number_of_todo_items(self):
    list = todo.List()
    list.add_item("summary")
    self.assertEqual(1, list.total_count())
```

Implementing these functions requires little more than wrapping built-in
functions for dictionaries:

```python
def total_count(self):
    return len(self._list)
def remove_item(self, idx):
    if idx in self._list:
        del self._list[idx]
```

We provide tests that prove that we can access collections of tickets given a
specific desired ticket state:

```python
def test_list_open_items_returns_inprogress_items(self):
    list = todo.List()
    idx = list.add_item("summary")
    list.lookup(idx).inprogress()
    self.assertEqual({idx: list.lookup(idx)}, list.open_items())
    list.lookup(idx).complete()
    self.assertEqual({}, list.open_items())
def test_list_todo_items_returns_todo_items(self):
    list = todo.List()
    idx = list.add_item("summary")
    self.assertEqual({idx: list.lookup(idx)}, list.todo_items())
    list.lookup(idx).inprogress()
    self.assertEqual({}, list.todo_items())
```

Although we provide convenience methods to access all tickets for each of our
defined states, we abstract the actual lookup into a single method,
`items_by_state`:

```python
def items_by_state(self, state):
    found = {}
    for key, item in self._list.items():
        if item.status() == state:
            found[key] = item
    return found
def open_items(self):
    return self.items_by_state(TodoStatus.INPROGRESS)
def todo_items(self):
    return self.items_by_state(TodoStatus.TODO)
def abandoned_items(self):
    return self.items_by_state(TodoStatus.ABANDON)
def complete_items(self):
    return self.items_by_state(TodoStatus.DONE)
```

The final step to finishing up or List class is, as with `TodoItem`, providing a
way to easily generate serialized json.  We will define a `json_dict` for our
`List` type as well, and simply call `json_dict` on each member of our
dictionary to generate the values in our JSON object:

```python
def json_dict(self):
    items = {}
    for k, v in self._list.items():
        items[k] = v.json_dict()
    return items
```

# Implementing the Web Service

Having defined the core components of our web service, we need to expose it to
users via HTTP.  We'll be using the [Flask web
framework](http://flask.pocoo.org) to impelement our HTTP service.

We'll start by creating a flask application, this will be what handles our
application routing and manages the HTTP connections themselves:

```python
app = Flask(__name__)
```

Next we need to create a `List` to use for user interactions:

```python
list = List()
```

Flask uses the `.route` decorator to attach python functions to routes with the
framework.  The example below provides a very based route for `/` that directs
the user to review the documentation available at the `/api` route:

```python
@app.route('/')
def home():
    return "Not much to see here!  See '/api' for API documentation"
```

The return value of functions that are attached to routes are polymorphic over
three value types:

- string: Where we return the HTTP response body as a simple string
- (string, Dict string string): Returns a tuple with the http response body as a
  string and a dictionary whose keys are HTTP response header names, and whose
  values are the values for the associated header
- (string, integer, Dict string string):   Returns a thruple of the http
  response body as a string, the numeric HTTP status code, and a dictionary of
  header names to header values

## Adapting Ticket Sets

Many of our list functions return a dict mapping integers to tickets.  We need
to convert these returned types into an appropriate JSON-serializable type.  To
accomplish this we implement a function `convertDict` that will perform the
appropriate conversion:

```python
def convertDict(item_map):
    result_map = {}
    for k, v in item_map.items():
        result_map[k] = v.json_dict()
    return result_map
```

## Basic Query Routes

The majority of our HTTP endpoints are simply allowing the user to list tickets
with some basic filtration.  These each follow a similar basic pattern and are
shown collectively below:

```python
@app.route('/api')
def help():
    with open("api.json") as f:
        contents = f.read()
    return (contents, {"Content-Type": "application/json"})

@app.route("/items")
def items():
    return (json.dumps(list.json_dict()), {"Content-Type": "application/json"})

@app.route("/open")
def open_items():
    return (json.dumps(convertDict(list.open_items())), {"Content-Type": "application/json"})

@app.route("/complete")
def complete_items():
    return (json.dumps(convertDict(list.complete_items())), {"Content-Type": "application/json"})

@app.route("/abandoned")
def abandoned_items():
    return (json.dumps(convertDict(list.abandoned_items())), {"Content-Type": "application/json"})

@app.route("/pending")
def pending_items():
    return (json.dumps(convertDict(list.todo_items())), {"Content-Type": "application/json"})
```

## Creating New Tickets

The `add` route is the first route we've encountered that use an HTTP method
other than `GET`.  In this case, we expect the user to `POST` data to our
endpoint.  The POST body should contain JSON data that deserializes into a
dictionary containing a required summary and optional stating status.

```python
@app.route("/add", methods=['POST'])
def add():
    data = request.get_json(force = True)
    f = lambda x: x
    if "summary" not in data:
        return ("missing summary field", 400, {})
    if "status" in data:
        status = TodoStatus.parse(data["status"])
        if status is None:
            return ("invalid status", 400, {})
        f = lambda x: x.set_status(status)
    idx = list.add_item(data["summary"])
    f(list.lookup(idx))
    return ("{}".format(idx), {"Content-Type": "application/plaintext"})
```

To implement this route, we use `request.get_json` to fetch the body of the HTTP
message as a dictionary generated by deserializing JSON (the `force = True`
parameter tells Flask to not require the user set the `Content-Type:
application/json` header- a conveninece for testing with curl.

After performing some basic validation to ensure the summary is set, and that if
a status has been set that it is a valid status, we create and configure the
todo list item, finally sending a plaintext response containing the stringified
numeric index of the newly added item.

## Item RUD Operations

The individual ticket endpoints are the msot complicated of our routes.  We need
to capture the route into a variable and then support three separate requests
depending on the HTTP method used.

To implement this we start by creating a top level function for the route:

```python
@app.route("/<int:item_id>", methods=['GET','PUT','DELETE'])
def item(item_id):
    if request.method == 'GET':
        return get_item(item_id)
    elif request.method == 'PUT':
        return update_item(item_id,request.get_json(force = True))
    elif request.method == 'DELETE':
        return remove_item(item_id)
```

The `<int:item_id>` syntax allows us to capture a variable from our route, and
provide Flask with a hint as to the type we expect to get.  The `request.method`
value allows us to look at the HTTP request type.  We use this information to
call out to one of three specialized functions that will handle the work for
each of the actions dictated by the HTTP methods:

### Getting an Item

Getting an item is straighforward.  We look in our list for the item ID.  If
it's found we convert it to JSON and return the value, otherwise we return an
HTTP status code 404.

```python
def get_item(item_id):
    item = list.lookup(item_id)
    if item is None:
        return ("no such item", 404, {})
    return (json.dumps(item.json_dict()), {"Content-Type": "application/json"})
```

### Removing an Item

Removing an item will never fail, allowing the user to remove items without
first querying to see if they have already been removed.  Our remove function
simply calls `list.remove_item` and then returns an HTTP status code 200.

```python
def remove_item(item_id):
    list.remove_item(item_id)
    return ("",200,{})
```

### Updating an Item

Our update process works very much like our `add` endpoint, with the addition of
a verification step to ensure that the item already exists.  We also treat the
summary field as optional rather than required as it is when we are creating a
new item.

```python
def update_item(item_id, item_map):
    item = list.lookup(item_id)
    if item is None:
        return ("no such item", 404, {})
    if "status" in item_map:
        parsed = TodoStatus.parse(item_map["status"])
        if parsed is None:
            return ("invalid status", 400, {})
        item.set_status(parsed)
    if "summary" in item_map:
        item.set_summary(item_map["summary"])
    return (json.dumps(item.json_dict()), {"Content-Type": "application/json"})
```

# Conclusion

Without much experience developing python applications, this simple application
ended up taking me approximately 5 hours to build from start to finish.  I found
approximately 80% of that time amounted to looking for errors that would have
been detected by the type system of even a rudimentary typed language.

Compared to Go, I found the APIs for defining endpoints a bit simpler due to the
lack of support for pluggable middleware systems, but the lack of obvious
support of mocking the HTTP server makes code testability a problem.  Comapred
to haskell with Servant I found the process of API definition tedious, the JSON
serialization absurdly contorted, and the lack of type safety more than doubled
the amount of time required to complete the application.

While I have a much greater apprecation for the capabilities of Python for
developing web applications, I am unlikely to pick it up as a first, or even
fourth or fifth option for any restful services I develop in the future.
