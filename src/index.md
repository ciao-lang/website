\title The Ciao Prolog system

Ciao is a **modern Prolog** implementation that builds up from a
logic-based simple kernel designed to be portable, extensible, and
modular. It features:

\begin{cartouche}
 - [**constraint**](/ciao/build/doc/ciao.html/ExtendLang.html) logic
   programming and, in particular, [**Prolog**](/ciao/build/doc/ciao.html/BasicLang.html), supporting the
   [ISO-Prolog standard](/ciao/build/doc/ciao.html/ciaointro.html#ISO-Prolog%20compliance%20and%20extensibility),

 - [**multiparadigm**](/ciao/build/doc/ciao.html/ExtendLang.html)
   programming (meta-programming, higher-order, concurrency,
   functions, etc.)

 - user [**packages**](/ciao/build/doc/ciao.html/packages.html) for
   syntactic and semantic language extensions, and interface with
   [**foreign**](/ciao/build/doc/ciao.html/foreign_interface_doc.html)
   code,

 - [**modules**](/ciao/build/doc/ciao.html/modules.html) and
   [**bundles**](/ciao/build/doc/ciao.html/bundles_doc.html) for large
   scale development, and
   [**assertions**](/ciao/build/doc/ciao.html/AssrtLang.html) for
   unified static and dynamic verification.

\end{cartouche}

Ciao can [**install natively**](install.html) in your operating system
or [**run in the browser**](/playground/index.html) without
installation.

```ciao_runnable
%! \begin{miniplayground}
% Edit me!
% Code is loaded on the fly.
%
% Then type a query on the right,
%   ?- app(X,Y,[1,2,3]).

app([],Ys,Ys).
app([X|Xs],Ys,[X|Zs]) :-
     app(Xs,Ys,Zs).
%! \end{miniplayground}
```

The system implements some **advanced features** such as separate and
incremental compilation, global program analysis and static debugging
and optimization (via source to source program transformation,
[**CiaoPP preprocessor**](/ciao/build/doc/ciaopp.html)), a [**build
automation**](/ciao/build/doc/ciao_builder.html) system, [**LPdoc
documentation generator**](/ciao/build/doc/lpdoc.html), debugger, and
integrated [**development environment**](/ciao/build/doc/ciao.html/DevEnv.html).


