-module(filter).

-export([non_an/1,size/1,plurals/1,endings/1,blacklist/1]).
-define(MINSIZE,3).

% Remove non-alphanumeric characters
non_an(Word) ->
    non_an(Word, []).

non_an([], Acc) ->
    lists:reverse(Acc);
non_an([Letter|Tail], Acc)
  when Letter < $a; Letter > $z ->
    % Drop the letter
    non_an(Tail, Acc);
non_an([Letter|Tail], Acc) ->
    % This is a letter we want, keep it.
    non_an(Tail, [Letter|Acc]).

% Remove words smaller than 3 chars
size(Word) when length(Word) < ?MINSIZE ->
    [];
size(Word)->
    Word.

% Won't implement that one, seems too complicated to me.
plurals(Word)->
    Word.

% Not sure what they mean by that one.
endings(Word)->
    Word.

% Drop the word if it's in a blacklist. Just filtering for a few words
% to demonstrate the mechanism is working.
blacklist(Word)->
    Bl = ["all", "and", "are", "any"],
    case lists:member(Word, Bl) of
	true -> [];
	false -> Word
    end.
