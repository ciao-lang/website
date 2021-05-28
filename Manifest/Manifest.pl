:- bundle(website).
version('1.1').
depends([core, lpdoc]).
alias_paths([ciao_website = '.']).

service(website, [redirect('/index.html'), http]).
