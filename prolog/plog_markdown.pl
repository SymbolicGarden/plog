% Copyright © 2025 Zhongying Qiao
% Licensed under the Apache License 2.0.
% See the LICENSE file for details or http://www.apache.org/licenses/LICENSE-2.0.

:- module(plog_markdown, [
    render_paragraphs/2,
    inline/2
]).

:- use_module(library(pcre)).
:- use_module(library(http/html_write)).

render_paragraphs([], []).
render_paragraphs([Line|Rest], [HTML|Out]) :-
    (
        re_matchsub("^# +(.*)", Line, D, []) -> HTML = h1(D.get(1));
        re_matchsub("^## +(.*)", Line, D, []) -> HTML = h2(D.get(1));
        re_matchsub("^### +(.*)", Line, D, []) -> HTML = h3(D.get(1));
        re_matchsub("^> +(.*)", Line, D, []) -> HTML = blockquote(p(D.get(1)));
        re_matchsub("^---$", Line, _, []) -> HTML = hr([]);
        inline(Line, Parts),
        HTML = p(Parts)
    ),
    render_paragraphs(Rest, Out).

inline(Line, Parts) :-
    (   re_matchsub("^(.*?)\\!\\[(.*?)\\]\\((.*?)\\)(.*)$", Line, D, []) ->
        B = D.get(1),
        T = D.get(2),
        U = D.get(3),
        A = D.get(4),
        inline(B, PBefore),
        inline(A, PAfter),
        append(PBefore,
               [img([src(U), width('450'), height('400'), alt(T)])|PAfter],
               Parts)
    ;   re_matchsub("^(.*?)\\[(.*?)\\]\\((.*?)\\)(.*)$", Line, D, []) ->
        B = D.get(1),
        T = D.get(2),
        U = D.get(3),
        A = D.get(4),
        inline(B, PBefore),
        inline(A, PAfter),
        append(PBefore, [a([href(U)], T)|PAfter], Parts)
    ;   re_matchsub("^(.*?)`(.*?)`(.*)$", Line, D, []) ->
        B = D.get(1),
        M = D.get(2),
        A = D.get(3),
        inline(B, PBefore),
        inline(A, PAfter),
        append(PBefore, [code(M)|PAfter], Parts)
    ;   re_matchsub("^(.*?)\\*\\*(.*?)\\*\\*(.*)$", Line, D, []) ->
        B = D.get(1),
        M = D.get(2),
        A = D.get(3),
        inline(B, PBefore),
        inline(A, PAfter),
        append(PBefore, [b(M)|PAfter], Parts)
    ;   re_matchsub("^(.*?)_(.*?)_(.*)$", Line, D, []) ->
        B = D.get(1),
        M = D.get(2),
        A = D.get(3),
        inline(B, PBefore),
        inline(A, PAfter),
        append(PBefore, [i(M)|PAfter], Parts)
    ;   Parts = [Line]
    ).

