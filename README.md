# com.fingerhutpress.text.unicode

Some experimental Clojure functions and macros for dealing with
Unicode strings.  May not go anywhere, but I've learned something
about Unicode in the process.

## Usage

See documentation for individual functions in the source code.

### Reminder to self on how to run the optional tests

Test selectors used in the test file now:

```
% lein test :slow    ; takes many minutes right now
% lein test :char-types
% lein test :normalized-forms
% lein test :test-unicode-property-names
```

TBD: Consider putting some of those in separate test namespaces, then
use `lein test namespace.name` to run only the tests in that
namespace.


## License

Copyright (C) 2012 John Andrew Fingerhut (andy_fingerhut@alum.wustl.edu)

Distributed under the Eclipse Public License, the same as Clojure.
