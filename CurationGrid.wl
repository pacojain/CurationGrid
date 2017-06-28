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
		numExtraFields= 2,
		showHideFieldNum= 2,
		state= ReplaceAll[colTypes, {"Sort" -> "   ", "Choice" -> "   "}],
		hideFlag = False,
		sortButton, choiceButton,
		sortby,
		list = MapThread[Join, {Table[{j, True}, {j, Range[Length[listIn]]}], listIn}],
		choiceArray= Array[0&, {Length[colTypes]+2, Length[listIn]}],
		displayList
	},
	displayList = list;
	sortButton[i_] := Dynamic[Button[Switch[state[[i]], "^", "\[DoubleUpArrow]", "v", "\[DoubleDownArrow]", _, Style["\[DoubleDownArrow]", Hue[0, 0, 0, 0](*use invisible arrow to ensure same width*)]],
		Switch[state[[i]],
			"^", state[[i]] = "v"; state = MapAt[If[MemberQ[{"^", "v"}, #], " ", #]&, state, {{1;;i-1}, {i+1;;}}]; sortby = OrderedQ[{#1[[i+numExtraFields]], #2[[i+numExtraFields]]}]&,
			_, state[[i]] = "^"; state = MapAt[If[MemberQ[{"^", "v"}, #], " ", #]&, state, {{1;;i-1}, {i+1;;}}]; sortby = OrderedQ[{#2[[i+numExtraFields]], #1[[i+numExtraFields]]}]&
		]
	]];
	choiceButton[i_] := Dynamic[Button[state[[i]], 
		Switch[state[[i]],
			" o ", state[[i]] = " x "; displayList= list,
			_, state[[i]] = " o "; displayList= MapIndexed[If[#2[[2]] === i + numExtraFields, {#, #2}, #]&, list, {2}]
		]
	]];
	Dynamic[Grid[
		(* hide/unhide button *)
		Join[{{
			Button[If[hideFlag, "u", "h"], Switch[hideFlag,
				True, hideFlag = False,
				False, hideFlag = True
			]],
			(* column buttons based on colTypes *)
			Sequence @@ (MapThread[#[#2]&, {colTypes /. {"Sort"-> sortButton, "Choice"-> choiceButton}, Range[Length[colTypes]]}] /. "None"[_]-> " ")
		}},
		If[#[[2]] || ! hideFlag, Prepend[#[[numExtraFields+1;;]], Button[If[#[[2]], " ", "x"], displayList= MapAt[Not, displayList, {First[#], 2}]]], Nothing]& /@ Sort[displayList, sortby]],
		If[{opts} =!= {}, FilterRules[{opts}, Options[Grid]], Unevaluated[Sequence[Frame -> All, Background -> {None, {{Lighter[LightBlue], None}}}]]]
	]]
]


End[]

EndPackage[]
