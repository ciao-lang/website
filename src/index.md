\title The Ciao programming language

Ciao is a programming language that builds up from a logic-based
simple kernel, and is designed to be extensible and modular. Its supports:

 - [**constraint**](/ciao/build/doc/ciao.html/ExtendLang.html) logic
   programming (and, in particular, [**Prolog**](/ciao/build/doc/ciao.html/BasicLang.html), supporting the
   [ISO-Prolog standard](/ciao/build/doc/ciao.html/ciaointro.html#ISO-Prolog%20compliance%20and%20extensibility),
 - different levels of modularity (from small to large scale):
   - [**modules**](/ciao/build/doc/ciao.html/modules.html) as (analysis-friendly) compilation units,
   - [**bundles**](/ciao/build/doc/ciao.html/bundles_doc.html) as collections of modules, 
 - [**packages**](/ciao/build/doc/ciao.html/packages.html) as modules implementing language extensions
   (syntactic definitions, compilation options, compiler plugins), 
 - [**assertions**](/ciao/build/doc/ciao.html/AssrtLang.html) (as a homogeneous framework that allows static and
   dynamic verification to work cooperatively in a unified way), 
 - [**multiparadigm**](/ciao/build/doc/ciao.html/ExtendLang.html) features (meta-programming, higher-order,
   mutables, concurrency, functions, etc.) and interfacing with
   [**foreign**](/ciao/build/doc/ciao.html/foreign_interface_doc.html) code.

The system implements some advanced features such as separate and
incremental compilation, global program analysis and static debugging
and optimization (via source to source program transformation,
CiaoPP preprocessor), a build automation system, documentation
generator, debugger, and integrated development environment.
