:- bundle(website).
version('1.2').
depends([
    core,
    lpdoc,
    ciao_playground
    % ciaoviz % TODO: make it optional
]).
alias_paths([
    ciao_website = 'src',
    catalog_ui = 'catalog_ui'
]).
%lib('src').
lib('catalog_ui').

%service(catalog_ui, [main='catalog_ui/catalog_ui', actmod]).
service(website, [redirect('/index.html'), http]).
