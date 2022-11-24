:- ensure_loaded(library(clpfd)). 

% puzzle_solution(+Puzzle, +WordList)
% program entry
puzzle_solution(Puzzle, WordList) :-
    get_all_slots(Puzzle, Slots),
    fill_puzzle(Slots, WordList).

/*
  get_all_slots(+Rows, -Slots)
  get_all_slots/2 get all valid slots(-Slots) of the puzzle in both vertical 
  and horizontal directions, combine them together to a list Slot
*/
get_all_slots(Rows, Slots) :-
    transpose(Rows, Cols),
    append(Rows, Cols, Lists),
    get_slots(Lists, S),
    exclude(lengthFilter, S, Slots).

/*
  lengthFilter(+List)
  lengthFilter/1 is used to judge whether the input list has length < 2
*/
lengthFilter(List) :-
    length(List, X),
    X < 2.

/*
  get_slots(+Lists, -Slots)
  get_slots/2 goes throw whole (+Lists) and get all valid Slots in each List
  +Lists: the puzzle lists
  -Slot: is all valid slots in each list combined together
*/
get_slots([], []).
get_slots([Lst|Lsts], Slots) :-
    split(Lst, LSlots),
    get_slots(Lsts, RSlots),
    append(LSlots, RSlots, Slots), !.

/*
  split(+List, -Slots)
  split/2 splits a input List into many sublist via seperator #
  +List: List want to split
  -Slots: Splitted list of input List
*/
split(List, [Left|Rest]) :-
    append(Left, [Sep|Right], List), 
    Sep == #, !, 
    split(Right, Rest), !.
split(List, [List]).


/*
  fill_puzzle(+Slots, +WordList)
  fill_puzzle/2 checks whether Slots can be unified by the WordList, unifies
  the first slot in the SortedSlots each term
  +Slots: list of Slot
  +WordList: wordlist contains words to check unification
*/
fill_puzzle(Slots, WordList) :-
    (Slots == []
    ->   WordList = []
         % given all valid slots, sort Slots
    ;    sort_slots(Slots, WordList, [Slot|SortedSlots]),
         % Unify the first slot in the SortedSlots
         member(Slot, WordList),
         % remove the unified word in the WordList
         select(Slot, WordList, NWordList),
         % unify the rest of the Slots
         fill_puzzle(SortedSlots, NWordList)).


/*
  sort_slots(+Slots, +WordList, -SortedSlots)
  Given all valid Slots, and WordList. sort the Slots
  +Slots: list of Slot
  +WordList: wordlist contains words to check unification
  -SortedSlots: the Slots sorted by the number of the unifiable words 
  in WordList each slot(ascending order)
  
  inspired by https://www.metalevel.at/prolog/sorting
  and https://stackoverflow.com/a/61417246
*/
sort_slots(Slots, WordList, SortedSlots) :-
    % generates a Map, according to the number of words can be unified each slot
    maplist(slot_uni_num(WordList), Slots, Map),
    keysort(Map, SortedMap),
    remove_map_num(SortedMap, SortedSlots).


/*
  slot_uni_num(+Slot, +WordList, -SlotMap)
  slot_uni_num/3 Generate a SlotMap in the form of [Num-Slot]
  +Slot: one slot
  +WordList: wordlist contains words to check unification
  -SlotMap: [Num-Slot]
*/
slot_uni_num(WordList, Slot, SlotMap) :-
    slot_uni_num(WordList, Slot, 0, SlotMap).

% slot_uni_num/4, same with slot_uni_num/3 except a accumulator
% +Num: Accumulator
slot_uni_num([], Slot, Num, Num-Slot).
slot_uni_num([Word|WordList], Slot, Num, SlotMap) :-
    (unifiable(Word, Slot, _)
    -> slot_uni_num(WordList, Slot, Num + 1, SlotMap)
    ;  slot_uni_num(WordList, Slot, Num, SlotMap)).


/*
  remove_map_num(+SlotsMapping, -Slots)
  remove_map_num/2 removes the Num- in the mapping
  +SlotsMapping: list of [Num-Slot]
  -Slots: list of [Slot]
*/
remove_map_num([], []).
remove_map_num([_-S|Maps], [S|Slots]) :-
    remove_map_num(Maps, Slots).
