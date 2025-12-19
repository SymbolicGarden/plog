:- module(plog_server, [
    server/1,
    server/2
]).

:- dynamic content/1.

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(uri)).
:- use_module(library(pcre)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_files)).

:- use_module(plog_markdown).
:- use_module(plog_style).


:- http_handler(root(.), list_blogs, []).
:- http_handler(root(blogs), list_blog, []).
:- http_handler(root('rss.xml'), rss_handler, []).

:- multifile http:location/3.
http:location(images, root(images), []).
:- multifile http:location/3.
http:location(contents, root(contents), []).

:- multifile http:location/3.
http:location(images, root(images), []).
:- multifile http:location/3.
http:location(contents, root(contents), []).

:- multifile user:file_search_path/2.
user:file_search_path(images, 'images').
:- multifile user:file_search_path/2.
user:file_search_path(contents, 'contents').

:- multifile plog:site_title/1, plog:site_link/1, plog:site_description/1.
site_title(T) :- (plog:site_title(T) -> true ; T = 'BlauAnarchy\'s Blogs').
site_link(L)        :- (plog:site_link(L)  -> true ; L = 'https://blauanarchy.org').
site_description(D) :- (plog:site_description(D) -> true ; D = 'A Blog site on Symbolic Coherence, written in pure prolog.').

:- http_handler(images(.), image_handler, [prefix]).
:- http_handler(contents(.), content_handler, [prefix]).

image_handler(Request) :-
    http_reply_from_files('images', [], Request).
content_handler(Request) :-
    http_reply_from_files('contents', [], Request).

rss_handler(_) :-
    generate_rss(XML),
    format('Content-type: application/rss+xml~n~n'),
    format('~w', [XML]).

content_files(Files) :-
    absolute_file_name(contents, Dir, [ file_type(directory), access(read)]),
    directory_files(Dir, Raw),
    exclude(is_dot, Raw, Files).
is_dot('.').
is_dot('..').


file_info(File, Size, Modified) :-
    absolute_file_name(contents(File), Path, [access(read)]),
    size_file(Path, Size),
    time_file(Path, Modified).

format_timestamp(Stamp, Time) :-
    stamp_date_time(Stamp, DT, 'UTC'),
    format_time(string(Time), '%Y-%m-%d %H:%M:%S', DT).

server(Port) :-
    server(Port, []).
server(Port, Options) :-
    set_paths_from_options(Options),
    http_server(http_dispatch, [port(Port)]),
    thread_get_message(never).

set_paths_from_options(Options) :-
    (   member(content_dir(ContentsDir), Options)
    ->  retractall(user:file_search_path(contents, _)),
        asserta(user:file_search_path(contents, ContentsDir))
    ;   true
    ),
    (   member(images_dir(ImagesDir), Options)
    ->  retractall(user:file_search_path(images, _)),
        asserta(user:file_search_path(images, ImagesDir))
    ;   true
    ).

list_blogs(_Request) :-
    site_title(Title),
    content_files(Files),
    predsort(compare_by_published_desc, Files, Sorted),
    reply_html_page(
        title(Title),
        [ \page_style ],
        [
            div([id(content)], [
                h1(Title),
                table(
                    [
                        \header|
                        \blogs(Sorted)
                    ]
                ),
                \pack_about
            ])
        ]
    ).

pack_about -->
    html(footer([id(meta)], [
        p([
            'Built by ',
            strong('Zhongying Qiao'),
            ' using ',
            code('Plog'),
            ', an SWI-Prolog blog engine that renders Markdown at request time,',
            ' in ',
            strong('Pure Prolog.')
        ]),
        p([
            a([ href('https://github.com/cryptoque/prolog-blog-engine')
              , target('_blank')
              , rel('noopener noreferrer')
              ],
              'View the source on GitHub')
        ])
    ])).



header -->
    html(tr([   th([class(title)], 'Title'), th([class(desc)],  ''), th([class(time)], 'Last Updated At')])).

blogs([]) --> [].
blogs([H|T]) -->
    {get_blog_display_name(H, H0)},
    html(tr([td([class(title)], \blog_link(H0, H)), td([class(desc)], ''), td([class(time)], \get_published_at(H))])),
    blogs(T).

get_published_at(Blog) -->
    { file_info(Blog, _, Created) },
    { format_timestamp(Created, CreatedFormatted) },
    html(CreatedFormatted).

compare_by_published_desc(Order, BlogA, BlogB) :-
    file_info(BlogA, _, TimeA),
    file_info(BlogB, _, TimeB),
    compare(TimeOrder, TimeB, TimeA),
    (   TimeOrder == (=) ->  compare(Order, BlogA, BlogB);
        Order = TimeOrder
    ).

                    

blog_link(Blog, Display) -->
    { http_link_to_id(list_blog, [name=Display], HREF) },
    html(a(href(HREF), Blog)).

list_blog(Request) :-
    http_parameters(Request, [name(Blog, [])]),
    read_blog_files(Blog, Paragraphs),
    [Innerparagraphs] = Paragraphs,
    split_string(Innerparagraphs, "\n", "", ParagraphLines),
    plog_markdown:render_paragraphs(ParagraphLines, HtmlParagraphs),
    reply_html_page(
        title('Title: ~w'-[Blog]),
        [ \blog_style ],
        [
          div(id(content), HtmlParagraphs)
        ]
    ).

get_blog_display_name(Blog, Path) :-
    re_replace("_" /g , " ", Blog, Path0),
    re_replace(".pl" /g , "", Path0, Path).

read_blog_files(Blog, Paragraphs) :-
    retractall(content(_)),
    absolute_file_name(contents(Blog), Path, [access(read)]),
    consult(Path),
    findall(P, content(P), Paragraphs).

generate_rss(XML) :-
    site_title(Title),
    site_link(Root),
    site_description(Desc),
    atomic_list_concat([Root, '/rss.xml'], FeedURL),

    content_files(Files),
    predsort(compare_by_published_desc, Files, Sorted),

    findall(Item,
        ( member(Blog, Sorted),
          rss_item(Blog, Item)
        ),
        Items),

    atomic_list_concat(Items, "\n", ItemsXML),

    format(string(XML),
'<?xml version=\'1.0\' encoding=\'ISO-8859-1\'?>
<rss version=\'2.0\' xmlns:atom=\'http://www.w3.org/2005/Atom\'>
<channel>
<title>~w</title>
<link>~w</link>
<description>~w</description>
<atom:link href=\'~w\' rel=\'self\' type=\'application/rss+xml\' />
~w
</channel>
</rss>',
    [Title, Root, Desc, FeedURL, ItemsXML]).


rss_date(Timestamp, RSS) :-
    stamp_date_time(Timestamp, DT, 'UTC'),
    format_time(string(RSS),
        '%a, %d %b %Y %H:%M:%S GMT', DT).

rss_item(BlogFile, XML) :-
    get_blog_display_name(BlogFile, Display),
    file_info(BlogFile, _, Created),
    rss_date(Created, PubDate),
    http_link_to_id(list_blog, [name=BlogFile], RelLink),
    site_link(Root),
    uri_resolve(RelLink, Root, Link),
    format(string(XML),
'<item>
<title>~w</title>
<link>~w</link>
<pubDate>~w</pubDate>
<guid isPermaLink="true">~w</guid>
</item>',
    [Display, Link, PubDate, Link]).
