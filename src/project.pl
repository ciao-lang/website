:- use_package(assertions).
:- use_package(doccomments).
:- doc(filetype, part).
%! \title The Project
%  \module
%
%  Ciao is an open source project developed by the Ciao team and many
%  contributors. The core Ciao system is provided under the GNU
%  @href{LGPL}{LGPL} license.
%
%  The code is available as Git repositories both at:
%
%   - [Ciao organization at github](https://github.com/ciao-lang)
%   - [Our local gitlab instance](https://gitlab.software.imdea.org/ciao-lang)
%
%  # Contribute
%
%  Ciao is an open source project. We welcome contributions in any form:
%
%   - links to @bf{third-party} code using Ciao, 
%   - report of @bf{bugs} (both correctness and efficiency) and
%     mistakes in the documentation,  
%   - request of @bf{improvements} and new @bf{features} (compliance
%     with standards, new libraries, support for specific operating
%     system or platforms, installers, etc.), 
%   - source code @bf{patches}.
%
%  Preferably, use the the following moderated mailing lists (they reach
%  the Ciao developers only):
%
%  - @email{ciao-bug@@cliplab.org}
%  - @email{ciao@@cliplab.org} (for more specific issues, extended internal discussions)
%
%  Additionally, you can also fill [github issues](https://github.com/ciao-lang/ciao/issues)
%  at our public repository mirror or [gitlab issues](https://gitlab.software.imdea.org/ciao-lang/ciao-devel/-/issues)
%  (intented mainly for internal use; requires login).
%
%  # The Team
%
%  Ciao is in very active and continuous development since the early
%  90's by an international team, coordinated by [CLIP group](https://cliplab.org)
%  members at [UPM](http://www.upm.es/internacional) and 
%  the [IMDEA Software Institute](https://software.imdea.org).
%
%  Ciao builds on its predecessor, &-Prolog, developed between 1984
%  and the mid 90's at the University of Texas at Austin, USA, the
%  Microelectronics and Technology Corporation, and UPM. 
%  Past versions of Ciao are available at the 
%  [legacy archive](https://ciao-lang.org/legacy).

% TODO: add?
%
%  The repository [ciao](https://github.com/ciao-lang/ciao) contains a
%  minimum working system, that contains:
%   - @href{/catalog_ui?g='catalog_ui.catalog_at'(bundle(core))&s=[]}{core}: compiler and standard libraries
%   - @href{/catalog_ui?g='catalog_ui.catalog_at'(bundle(builder))&s=[]}{builder}: build system
