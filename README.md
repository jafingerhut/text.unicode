# com.fingerhutpress.text.unicode

Some experimental Clojure functions and macros for dealing with
Unicode strings.  May not go anywhere, but I've learned something
about Unicode in the process.

## Usage

See documentation for individual functions in the source code.

### Reminder to self on how to run the optional tests

Test selectors used in the test files now:

```
% lein test :slow    ; takes many minutes right now
% lein test :char-types
% lein test :normalized-forms
% lein test :property-names
```


## License

Copyright (C) 2012 John Andrew Fingerhut (andy_fingerhut@alum.wustl.edu)

Distributed under the Eclipse Public License, the same as Clojure.
