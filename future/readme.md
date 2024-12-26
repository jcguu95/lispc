``` common-lisp
(asdf:load-system "paren")
(asdf:test-system "paren")
```

### TODOs

+ Basic Functionalities

  + [X] New Type System
    + [X] Type Spec
    + [X] Type Formatter
    + [X] Hook into main code / main test
  + [ ] Preprocessor Directives
    + [X] Newlines need to be separated by `\`.
    A list of preproc directives:
    + [X] `#define`: Defines macros.
    + [X] `#undef`: Undefines macros.
    + [X] `#include`: Includes external files.
    + [ ] `#if, #ifdef, #ifndef`: Conditional compilation.
    + [ ] `#else, #elif, #endif`: Provides alternatives in conditional compilation.
    + [ ] `#pragma`: Compiler-specific instructions.
    + [ ] `#error`: Causes a compilation error with a message.
    + [ ] `#line`: Sets line numbers and filenames for error messages.
        
  + [X] Implement control flow: block, label, goto.
  + [X] Lisp Interopts: The c-operator `lisp`.
  + [X] Remove extra parenthesis for expansion.

+ Workflow

  + [X] compile-lsp-file 
  
    Add a function that directly executes a `*.lsp` file by compiling into
    `C`. executing, and printing its output to repl.
    
  + [ ] Run automatic tests in `./examples/`.
  
    Under the `./examples/` directory, process all `*.lsp` files, compile them
    with `paren`, and compare the results with the corresponding `*.c` files.
    
    + [X] Fix (nested) indentation.
    + [X] Fix semicolon and newline issue.
    + [X] Build comparison test workflow against example files.
    + [X] Build integration test for the behavior of generated c code.
    + [ ] Fix the code in `./examples/` and start running the tests.

+ Documentations and Examples (as Programmatic Tests)

  + [ ] Transpile the C-implementation of MAL.
  + [ ] Transpile examples from Learn C the Hard Way.
  + [ ] Transpile examples from selected parts of [Emacs's source code](https://github.com/emacs-mirror/emacs/blob/master/src/bytecode.c).
  + [ ] Transpile [sectorlisp/lisp.c](https://github.com/jart/sectorlisp/blob/main/lisp.c).
  + [ ] Give persuading example showing superiority of lisp macro.
  + [ ] After `Paren` is stable, use it to implement `BOCL`.
  + [ ] Package as docker container.
  + [ ] Find whether there's a c linter that can get rid of unnecessary parentheses.
