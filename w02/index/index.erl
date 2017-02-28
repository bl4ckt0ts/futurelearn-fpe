-module(index).
-export([tests/0,index/1]).

% Used to read a file into a list of lines.
% Example files available in:
%   gettysburg-address.txt (short)
%   dickens-christmas.txt  (long)
  
%% TODO:
%%
%% - Need to remove trailing commas and full stop to avoid having both
%% "but" and "but," or "yes" and "yes.".
%%
%% - Remove words smaller than 3 characters.
%%
%% - Remove words from a list of unwated words
%%
%% - Normalizing for common plurals and endings not be repeated.
%% 
%% - Think about a better data representation.


% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.
get_file_contents(Name) ->
    {ok,File} = file:open(Name,[read]),
    Rev = get_all_lines(File,[]),
    lists:reverse(Rev).

% Auxiliary function for get_file_contents.
% Not exported.
get_all_lines(File,Partial) ->
    case io:get_line(File,"") of
        eof -> file:close(File),
               Partial;
        Line -> {Strip,_} = lists:split(length(Line)-1,Line),
                get_all_lines(File,[Strip|Partial])
    end.

%% % Show the contents of a list of strings.
%% % Can be used to check the results of calling get_file_contents.
%% show_file_contents([L|Ls]) ->
%%     io:format("~s~n",[L]),
%%     show_file_contents(Ls);
%% show_file_contents([]) ->
%%     ok.    

% Needed to skip / drop: comma, point, empty lines, dash-dash, dash,
% semi-colon, colon,

index(Filename) ->
    Lines = get_file_contents(Filename),
    % starting at line 1 (not 0).
    index(Lines, [], 1).

% Let's iterate on the lines
index([], Acc, _LineCount) ->
    % no more lines to process, return the accumulator.
    Acc;
index([L|Tail], Acc, LineCount) ->
    IndexedLine = 
	index_words(string:tokens(string:to_lower(L), " "), LineCount),
    MergedAcc = merge(Acc, IndexedLine),
    index(Tail, MergedAcc, LineCount + 1).

% Iterate on words. This function takes a list of words as arguments.
index_words(Line, LineCount) ->
    index_words(Line, LineCount, []).

index_words([], _LineCount, Acc) ->
    Acc;
index_words([Word|Ws], LineCount, Acc) ->
    case clean_word(Word) of
	[] ->
	    % word has been blacklisted
	    index_words(Ws, LineCount, Acc);
	NewWord ->
	    % word is the same or has been modified
	    index_words(Ws, LineCount, 
			merge(Acc,[{NewWord, [{LineCount,LineCount}]}] ))
    end.

% Drop words shorter than 3 chars. The idea is make a list of
% functions we want to apply to a word to clean it and then iterate
% over that list, applying each functions with apply/3 on the word
% until we either drop the word or finish to apply all functions.
clean_word(Word) ->
    FunList = [{filter,non_an,[]},
	       {filter,size,[]},
	       {filter,plurals,[]},
	       {filter,endings,[]},
	       {filter,blacklist,[]}],
    clean_word(FunList, Word).
	   
clean_word([], Word) ->
    Word;
clean_word([{M,F,_A} | FunList], Word) ->
    case apply(M,F,[Word]) of
	[] -> [];
	NewWord -> 
	    clean_word(FunList, NewWord)
    end.

% Merge two Indexes into one.
merge(I1, I2)->
    % Syntactic sugar
    merge(I1, I2, []).

merge([],[],Acc)->
    % Nothing to merge, return the merged index.
    lists:reverse(Acc);
merge([], [W | Ws], Acc) ->
    % First index empty, just merge the second with the final one.
    merge([], Ws, [W|Acc]);
merge([W|Ws], [], Acc) ->
    % Second index empty, just merge the first one with the final one.
    merge(Ws, [], [W|Acc]);
merge([{Word, Refs1} | Tail1], [{Word, Refs2} | Tail2], Acc) ->
    % same words, merge references, iterate on both indexes.
    merge(Tail1, Tail2, [{Word, merge_refs(Refs1, Refs2)} | Acc]);
merge([{Word1, _Refs1}=A | Tail1], [{Word2, _Refs2} | _Tail2]=B, Acc) 
  when Word1 < Word2 ->
    % different words, put A in the final index. Only iterate on A.
    merge(Tail1, B, [A|Acc]);
merge([{Word1, _Refs1} | _Tail1]=A, [{Word2, _Refs2}=B | Tail2], Acc)
    when Word1 > Word2 ->
    % different words with Word1 > Word2. Put B in the final index,
    % iterate only on Word2. 
    merge(A, Tail2, [B|Acc]).

% Merge two references of the type [{3,5},{7,7},{11,13}] and
% [{3,5},{8,9},{11,13}] into [{3,5},{7,9},{11,13}].
merge_refs(A, B) ->
    build(explode(A, B)).

% Explode two references lists into a unique, sorted lists of pages.
explode(A,B)->
    lists:usort(explode(A) ++ explode(B)).

explode([]) ->
    [];
explode([{From,To}|Tail])->
    lists:seq(From,To) ++ explode(Tail).

% Build a list of references from an ordered list of pages.
build([])->
    [{}];
build([From|Tail]) ->
    build(From, From, Tail, []).

build(From, To, [], Acc) ->
    % No page number anymore to handle, add the current range and
    % return.
    lists:reverse([{From,To}|Acc]);
build(From, To, [Next|Tail], Acc) when (Next - To) =< 1 ->
    % in a range, itearting
    build(From, Next, Tail, Acc);
build(From, To, [Next|Tail], Acc) ->
    % not in a range anymore, add a ref to the list and iterate.
    build(Next, Next, Tail, [{From,To}|Acc]).


tests()->
    % Test if our code can merge two lists of references. This is
    % testing merge_refs/2, explode/1, explode/2, build/1 and build/4
    [{1,7},{11,12},{17,25}] = 
	merge_refs([{1,1},{3,7},{11,11}],[{2,4},{12,12},{17,25}]),
    % Test the merge/2 and merge/3 functions
    I1 = [{alpha,[{1,1},{3,5},{17,17}]},{beta,[{2,2},{6,8},{19,19}]},{gamma,[{96,96}]}], 
    I2 = [{beta,[{2,3},{6,20}]},{delta,[{95,117}]}],
    MergedIndexes = [{alpha,[{1,1},{3,5},{17,17}]}, {beta,[{2,3},{6,20}]},
		     {delta,[{95,117}]},{gamma,[{96,96}]}],
    MergedIndexes = merge(I1,I2),

    % Test the index_words/2 function
    Index = [{"first",[{0,0}]}, 
	     {"is",[{0,0}]}, 
	     {"line",[{0,0}]}, 
	     {"my",[{0,0}]}, 
	     {"this",[{0,0}]}],
    Index = 
	index_words(
	  string:tokens(
	    string:to_lower("This is my first line"), " "), 0),
    ok.
