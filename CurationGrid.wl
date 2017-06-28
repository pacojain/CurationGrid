BeginPackage["CurationGrid`"]

CurationGrid::usage= "This is a usage message ;)"
sortList::usage= "Another usage message!"

Begin["Private`"]

$historyLength = 30 (* possibly add undo/redo *)
ClearAll[CurationGrid]
SetAttributes[CurationGrid, HoldFirst]
CurationGrid[listIn_, colTypes_List, opts: OptionsPattern[]]:= DynamicModule[
	{
		varInName = ToString[HoldForm[listIn]], outVar,
		numExtraFields = 2,
		showHideFieldNum = 2,
		state= ReplaceAll[colTypes, {"Sort" -> "   ", "Choice" -> "   "}],
		hideFlag = False,
		sortButton, choiceButton,
		sortby,
		list = MapThread[Join, {Table[{j, True}, {j, Range[Length[listIn]]}], listIn}],
		choiceArray, displayList
	},
	outVar= Symbol["Global`" <> varInName <> "Out"];
	choiceArray= Array["???"&, {Length[listIn], Length[colTypes]+numExtraFields}];
	displayList = list;
	sortButton[i_] := Dynamic[Button[Switch[state[[i]], "^", "\[DoubleUpArrow]", "v", "\[DoubleDownArrow]", _, Style["\[DoubleDownArrow]", Hue[0, 0, 0, 0](*use invisible arrow to ensure same width*)]],
		Switch[state[[i]],
			"^", state[[i]] = "v"; state = MapAt[If[MemberQ[{"^", "v"}, #], " ", #]&, state, {{1;;i-1}, {i+1;;}}]; sortby = OrderedQ[{#1[[i+numExtraFields]], #2[[i+numExtraFields]]}]&,
			_, state[[i]] = "^"; state = MapAt[If[MemberQ[{"^", "v"}, #], " ", #]&, state, {{1;;i-1}, {i+1;;}}]; sortby = OrderedQ[{#2[[i+numExtraFields]], #1[[i+numExtraFields]]}]&
		]
	]];
	choiceButton[i_] := Dynamic[Button[state[[i]], 
		Switch[state[[i]],
			" o ", state[[i]] = " x "; displayList= MapIndexed[If[#2[[2]] === i + numExtraFields, choiceArray[[##]]&@@#2, #]&, list, {2}],
			_, state[[i]] = " o "; displayList= MapIndexed[If[#2[[2]] === i + numExtraFields, RadioButtonBar[Dynamic[choiceArray[[##]]]&@@#2, Prepend[#1, "???"], Appearance -> "Vertical"], #]&, list, {2}]
		]
	]];
	Column[{
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
			(* display list elements (with row hide/unhide buttons) *)
			If[#[[showHideFieldNum]] || ! hideFlag, Prepend[#[[numExtraFields+1;;]], Button[If[#[[showHideFieldNum]], " ", "x"], list= MapAt[Not, list, {First[#], showHideFieldNum}]; displayList= MapAt[Not, displayList, {First[#], showHideFieldNum}]]], Nothing]& /@ Sort[displayList, sortby]],
			(* grid options *)
			If[{opts} =!= {}, FilterRules[{opts}, Options[Grid]], Unevaluated[Sequence[Frame -> All, Background -> {None, {{Lighter[LightBlue], None}}}]]]
		]],
		Row[{Button["Store grid data", outVar = displayList], InputField[Dynamic[outVar], FieldSize -> {8, 1.5}, BaselinePosition -> Scaled[0.3], Alignment -> {Left, Center}]}]
	}]
]

(* sortList example by andrews *)
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

End[]

EndPackage[]
