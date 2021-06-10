:- bundle(website).
version('1.1').
depends([
    core,
    lpdoc,
    wui
    % ciaoviz % TODO: make it optional
]).
alias_paths([
    ciao_website = '.',
    catalog_ui = 'catalog_ui'
]).
lib('catalog_ui').

service(catalog_ui, [main='catalog_ui/catalog_ui', actmod]).
service(website, [redirect('/index.html'), http]).
