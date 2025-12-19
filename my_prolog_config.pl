% Copyright © 2025 Zhongying Qiao
% Licensed under the Apache License 2.0.
% See the LICENSE file for details or http://www.apache.org/licenses/LICENSE-2.0.

:- module(my_plog_config, []).
:- multifile plog:site_title/1, plog:site_link/1, plog:site_description/1.

plog:site_title('BlauAnarchy\'s Blogs'). % Change this with your own blog title
plog:site_link('https://blauanarchy.org'). % Change this with your own blog link
plog:site_description('A Blog site on Symbolic Coherence, written in pure prolog.'). % Change this with your own blog description

