BeginPackage["CurationGrid`"]

CurationGrid::usage= "This is a usage message ;)"
sortList::usage= "Another usage message!"
sortList2::usage= "Another usage message!"

Begin["Private`"]

$historyLength = 30
ClearAll[CurationGrid]
SetAttributes[CurationGrid, HoldFirst]
CurationGrid[data_]:=Module[
	{
		varInName = ToString[HoldForm[data]]
	},
	Print[varInName]
]

ClearAll[sortList];
sortby = OrderedQ;
sortList[list_]:= (state= None; Dynamic[Grid[
	Join[{{
		Button["alpha", Switch[state,
			"Ascending", state = "Descending"; sortby = OrderedQ,
			_, state = "Ascending"; sortby = (Not@OrderedQ[##]) &
		]],
		Button["num", Switch[state,
			"Ascending", state = "Descending"; sortby = (#1[[2]] < #2[[2]] &),
			_, state = "Ascending"; sortby = (#1[[2]] > #2[[2]] &)
		]],
		state (* Just to see to see what it's set to *)
	}}, Sort[list, sortby]]
]])

ClearAll[sortList2];
Attributes[HoldFirst]
sortList2[listIn_, colTypes_List, opts: OptionsPattern[]]:= DynamicModule[
	{
		state= ReplaceAll[colTypes, {"Sort" -> " ", "Choice" -> " "}],
		hideFlag = False,
		sortButton, choiceButton,
		sortby,
		list = MapThread[Join, {Table[{j, True}, {j, Range[Length[listIn]]}], listIn}],
		choiceArray= Array[Undefined&, {Length[colTypes]+2, Length[listIn]}],
		choiceDisplay = "x"
	},
	sortButton[i_] := Dynamic[Button[Switch[state[[i]], "^", "\[DoubleUpArrow]", "v", "\[DoubleDownArrow]", _, Style["\[DoubleDownArrow]", Hue[0, 0, 0, 0](*use invisible arrow to ensure same width*)]],
		Switch[state[[i]],
			"^", state[[i]] = "v"; state = MapAt[If[MemberQ[{"^", "v"}, #], " ", #]&, state, {{1;;i-1}, {i+1;;}}]; sortby = OrderedQ[{#1[[i+2]], #2[[i+2]]}]&,
			_, state[[i]] = "^"; state = MapAt[If[MemberQ[{"^", "v"}, #], " ", #]&, state, {{1;;i-1}, {i+1;;}}]; sortby = OrderedQ[{#2[[i+2]], #1[[i+2]]}]&
		]
	]];
	choiceButton[i_] := Dynamic[Button[state[[i]], 
		Switch[state[[i]],
			"o", state[[i]] = "x"; choiceDisplay = "x"(*RadioButtonBar[Dynamic[choiceArray[[i, #[[1]]]]], #[[i+2]], Appearance -> "Vertical"]&*),
			_, state[[i]] = "o"; choiceDisplay = "o"
		]
	]];
	Dynamic[Grid[
		Join[{{
			Button[If[hideFlag, "u", "h"], Switch[hideFlag,
				True, hideFlag = False,
				False, hideFlag = True
			]],
			Sequence @@ (MapThread[#[#2]&, {colTypes /. {"Sort"-> sortButton, "Choice"-> choiceButton}, Range[Length[colTypes]]}] /. "None"[_]-> " ")
		}},
		If[#[[2]] || ! hideFlag, Prepend[#[[3;;]], Button[If[#[[2]]," ", "x"], list= MapAt[Not, list, {First[#], 2}]]], Nothing]& /@ Sort[MapAt[choiceDisplay, list, {All, 4}], sortby]],
		If[{opts} =!= {}, FilterRules[{opts}, Options[Grid]], Unevaluated[Sequence[Frame -> All, Background -> {None, {{Lighter[LightBlue], None}}}]]]
	]]
]


End[]

EndPackage[]
