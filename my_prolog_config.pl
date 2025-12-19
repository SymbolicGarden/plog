:- module(my_plog_config, []).
:- multifile plog:site_title/1, plog:site_link/1, plog:site_description/1.

plog:site_title('BlauAnarchy\'s Blogs'). % Change this for our own blog title
plog:site_link('https://blauanarchy.org'). % Change this for our own blog link
plog:site_description('A Blog site on Symbolic Coherence, written in pure prolog.'). % Change this for our own blog description

