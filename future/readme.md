``` common-lisp
(asdf:load-system "paren" :force t)
(asdf:test-system "paren" :force t)
```

### TODOs

+ Basic Functionalities

  + [X] Type System
  + [X] Implement control flow: block, label, goto.
  + [X] Lisp Interopts: The c-operator `lisp`.
  + [ ] Preprocessor Directives
    + [X] Newlines need to be separated by `\`.
    + [X] `#define`: Defines macros.
    + [X] `#undef`: Undefines macros.
    + [X] `#include`: Includes external files.
    + [ ] `#if, #ifdef, #ifndef`: Conditional compilation.
    + [ ] `#else, #elif, #endif`: Provides alternatives in conditional compilation.
    + [ ] `#pragma`: Compiler-specific instructions.
    + [ ] `#error`: Causes a compilation error with a message.
    + [ ] `#line`: Sets line numbers and filenames for error messages.

+ Documentations and Examples (as Programmatic Tests)

  + [ ] Transpile the C-implementation of MAL.
  + [ ] Transpile examples from Learn C the Hard Way.
  + [ ] Transpile examples from selected parts of [Emacs's source code](https://github.com/emacs-mirror/emacs/blob/master/src/bytecode.c).
  + [ ] Transpile [sectorlisp/lisp.c](https://github.com/jart/sectorlisp/blob/main/lisp.c).
  + [ ] Give persuading example showing superiority of lisp macro.
  + [ ] After `Paren` is stable, use it to implement `BOCL`.
  + [ ] Package as docker container.
  + [ ] Find whether there's a c linter that can get rid of unnecessary parentheses.
