#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Section:: *)
(*Rindler maps in Poincar\[EAcute] Subscript[AdS, 3] - Extended!*)
(*Basics: derivations*)


(* ::Subsection:: *)
(*NOTE: Do NOT evaluate ALL cells at once! It will take forever!*)
(*Instead, run the cells you are interested in, and let Mathematica INITIALIZE the notebook for you.*)


(* ::Subsection:: *)
(*When asked for initialization, please click [Yes].*)
(*It will take ~30 seconds (to be further optimized).*)


(* ::Item:: *)
(*Note to self: style images as "Output" such that they are not committed into version controlled source.*)


(* ::Input::Initialization:: *)
SetDirectory@NotebookDirectory[]
(*<<"Physica/MathUtils.wl"*)


(* ::Subsection::Closed:: *)
(*Setup*)


(* ::Input::Initialization:: *)
$Assumptions=And[
u\[Element]Reals,v\[Element]Reals,y\[Element]Reals,
lu>0,lv>0
];

uvExchange={u->v,v->u,u0->v0,v0->u0,lu->lv,lv->lu,U->V,V->U,Tu->Tv,Tv->Tu};
sameT={Tu->T,Tv->T,\[Delta]Tu->\[Delta]T,\[Delta]Tv->\[Delta]T};
sameL={lu->l,lv->l};
sameLc={lvc->lc,luc->lc};

params={Tu,Tv,\[Delta]Tu,\[Delta]Tv,T,lu,lv,l(*,(rc^2)*)};
constParams=(Dt[#]->0&)/@params


(* ::Input::Initialization:: *)
uv2xt={u->x+t,v->x-t};
UV2XT={U->(X+T)/2,V->(X-T)/2};
xt2uv=Solve[uv2xt/.Rule->Equal,{x,t}]//First


(* ::Input::Initialization:: *)
metricSign=-1;
poincareCoord={u,v,z};
poincareLength=(Dt[u]Dt[v]+Dt[z]^2)/z^2;


(* ::Input::Initialization:: *)
lengthToMetric[quadraticForm_,coord_:coord]:=Table[
1/2 D[quadraticForm,Dt[i],Dt[j]],
{i,coord},{j,coord}
];


(* ::Subsection::Closed:: *)
(*Utilities*)


(* ::Input::Initialization:: *)
ClearAll[timedSimplify];
timedSimplify[expr_,assumpt_:{},timeout_:{1,30}]:=Quiet[
FullSimplify[expr,assumpt,TimeConstraint->timeout]
,{FullSimplify::time,FullSimplify::gtime}
]


(* ::Item:: *)
(*Assumptions for Solve make it think too much and fail.*)


(* ::Item:: *)
(*New feature / bug in Mathematica 13.*)


(* ::Input::Initialization:: *)
Quiet[SetOptions[Solve,Assumptions->True],{SetOptions::optnf}]


(* ::Section::Closed:: *)
(*Modular flow: basics*)


(* ::Subsection:: *)
(*Killing vectors*)


(* ::Input::Initialization:: *)
{ju,jv}={
Function[{n},\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{"{", 
RowBox[{
RowBox[{"-", "1"}], ",", "0", ",", "0"}], "}"}], 
RowBox[{"n", "==", 
RowBox[{"-", "1"}]}]},
{
RowBox[{"{", 
RowBox[{
RowBox[{"-", "u"}], ",", "0", ",", 
RowBox[{"-", 
FractionBox["z", "2"]}]}], "}"}], 
RowBox[{"n", "==", "0"}]},
{
RowBox[{"{", 
RowBox[{
RowBox[{"-", 
SuperscriptBox["u", "2"]}], ",", 
SuperscriptBox["z", "2"], ",", 
RowBox[{
RowBox[{"-", "u"}], " ", "z"}]}], "}"}], 
RowBox[{"n", "==", "1"}]},
{"0", 
TagBox["True",
"PiecewiseDefault",
AutoDelete->True]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)],
Function[{n},\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{"{", 
RowBox[{"0", ",", 
RowBox[{"-", "1"}], ",", "0"}], "}"}], 
RowBox[{"n", "==", 
RowBox[{"-", "1"}]}]},
{
RowBox[{"{", 
RowBox[{"0", ",", 
RowBox[{"-", "v"}], ",", 
RowBox[{"-", 
FractionBox["z", "2"]}]}], "}"}], 
RowBox[{"n", "==", "0"}]},
{
RowBox[{"{", 
RowBox[{
SuperscriptBox["z", "2"], ",", 
RowBox[{"-", 
SuperscriptBox["v", "2"]}], ",", 
RowBox[{
RowBox[{"-", "v"}], " ", "z"}]}], "}"}], 
RowBox[{"n", "==", "1"}]},
{"0", 
TagBox["True",
"PiecewiseDefault",
AutoDelete->True]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)]
};


(* ::Subsection:: *)
(*The y-gauge*)


(* ::Input:: *)
(*poincareLength*)


(* ::Item:: *)
(*We introduce a new gauge:*)


(* ::Input::Initialization:: *)
yCoord={u,v,y};
yLength=poincareLength/.z->Sqrt[y]//Simplify//Apart

coord=yCoord;
metric=lengthToMetric[yLength,coord]

<<diffgeoM`
Einstein-metric


(* ::Input::Initialization:: *)
zFunction=Function[y,Sign[y]Sqrt[Abs[y]]];
(*
">>> This is nice but singular <<<";
zFunction=Function[y,y/Sqrt[Abs[y]]];
*)
z2y=(z->zFunction[y])
y2z=(y->z^2 Sign[z]);


(* ::Input:: *)
(*y/.y2z/.z2y//FullSimplify*)
(*z/.z2y/.y2z//FullSimplify*)


(* ::Subsection:: *)
(*Isometries & modular flow*)


(* ::Input::Initialization:: *)
(ju/@Range[-1,1]) . Transpose[({
 {1, 0, 0},
 {0, 1, 0},
 {0, 0, \!\(
  \*SubscriptBox[\(\[PartialD]\), \(z\)]
  \*SuperscriptBox[\(z\), \(2\)]\)}
})]/.z->Sqrt[y]
\[Xi]u={-(lu/2),0,2/lu} . %

(jv/@Range[-1,1]) . Transpose[({
 {1, 0, 0},
 {0, 1, 0},
 {0, 0, \!\(
  \*SubscriptBox[\(\[PartialD]\), \(z\)]
  \*SuperscriptBox[\(z\), \(2\)]\)}
})]/.z->Sqrt[y]
\[Xi]v={-(lv/2),0,2/lv} . %

\[Xi]=2\[Pi] (\[Xi]u-\[Xi]v)/2//FullSimplify


(* ::Input:: *)
(*Lie[\[Xi]u,\[Xi]v,{up}]//Simplify*)


(* ::Item:: *)
(*The modular flow lines correspond to the \!\(\*OverscriptBox[\(t\), \(~\)]\) lines in the Rindler space.*)


(* ::Item:: *)
(*It is generated by the signed average of Subscript[\[Xi], (u)] and Subscript[\[Xi], (v)]. *)


(* ::Item:: *)
(*Subscript[\[Xi], (u)] and Subscript[\[Xi], (v)] are commuting isometries, which maps to the \!\(\*OverscriptBox[\(u\), \(~\)]\),\!\(\*OverscriptBox[\(v\), \(~\)]\) coordinates in the Rindler space.*)


(* ::Subsection::Closed:: *)
(*Fixed points & the HRT surface*)


(* ::Input:: *)
(*\[Xi]//FullSimplify*)


(* ::Item:: *)
(*Let's look at the fixed points of the modular flow.*)


(* ::Input:: *)
(*\[Xi]/.y->0*)


(* ::Item:: *)
(*This gives us 4 fixed points (\[PlusMinus]Subscript[l, u]/2,\[PlusMinus]Subscript[l, v]/2) at the tips of the diamond.*)


(* ::Item:: *)
(*If y!=0 then the fixed points must land on the hypersurface u/Subscript[l, u]=v/Subscript[l, v]. *)


(* ::Input:: *)
(*\[Xi]/.v->u lv/lu//FullSimplify*)
(*Solve[Thread[%=={0,0,0}]//Evaluate,y]//Simplify*)


(* ::Input:: *)
(*y->(lu lv)/4-u v/.v->u lv/lu*)


(* ::Item:: *)
(*This is the RT surface, and the result holds for both signs of y. *)


(* ::Input:: *)
(*\[Xi]/.y->(lu lv)/4-u v/.v->u lv/lu//FullSimplify*)


(* ::Section::Closed:: *)
(*Rindler maps: results*)


(* ::Subsection:: *)
(*General result*)


(* ::Input::Initialization:: *)
flowGeneral=Thread[{u,v,y}->{
lu/2 Coth[U] (y0+(u0+Tanh[U])(u0+Coth[V]))/(y0+(u0+Coth[U])(u0+Coth[V])),
lv/2 Coth[V] (y0+(u0+Coth[U])(u0+Tanh[V]))/(y0+(u0+Coth[U])(u0+Coth[V])),
y0 (lu lv)/4 ((Csch[U]Csch[V])/(y0+(u0+Coth[U])(u0+Coth[V])))^2
}]


(* ::Input::Initialization:: *)
flowGeneralY=flowGeneral/.{u0->u0[Y],y0->y0[Y]};


(* ::Subsection:: *)
(*Special cases*)


(* ::Item:: *)
(*The usual cutoff Rindler map:*)


(* ::Input::Initialization:: *)
flowCutoff=Thread[{u,v,y}->{lu/2 (Tanh[U]+Y Tanh[V])/(1+Y Tanh[U]Tanh[V]),lv/2 (Tanh[V]+Y Tanh[U])/(1+Y Tanh[U]Tanh[V]),(lu lv)/4 (Y Sech[U]^2 Sech[V]^2)/(1+Y Tanh[U]Tanh[V])^2}];


(* ::Input::Initialization:: *)
rindlerCoord={U,V,Y};
rindlerLength=yLength/.flowCutoff/.constParams//Simplify
rindlerMetric=lengthToMetric[%,rindlerCoord]
FullSimplify[rindlerLength]


(* ::Input:: *)
(*flowCutoff/.{U->0,V->0}//Simplify*)


(* ::Item:: *)
(*Rindler map for the complement & the glue-on region:*)


(* ::Input::Initialization:: *)
flowPhased=Thread[{u,v,y}->{lu/2 (Coth[U]+Y Coth[V])/(1+Y Coth[U]Coth[V]),lv/2 (Coth[V]+Y Coth[U])/(1+Y Coth[U]Coth[V]),(lu lv)/4 (Y Csch[U]^2 Csch[V]^2)/(1+Y Coth[U]Coth[V])^2}];


(* ::Input::Initialization:: *)
flowGlueon=Thread[{u,v,y}->{lu/2 (Cosh[U+x0] Sinh[V+x0]-Y Cosh[U] Sinh[V])/(Sinh[U+x0] Sinh[V+x0]-Y Sinh[U] Sinh[V]),lv/2 (Cosh[V+x0] Sinh[U+x0]-Y Cosh[V] Sinh[U])/(Sinh[U+x0] Sinh[V+x0]-Y Sinh[U] Sinh[V]),-((lu lv)/4)Y (Sinh[x0]/(Sinh[U+x0] Sinh[V+x0]-Y Sinh[U] Sinh[V]))^2}];


(* ::Item:: *)
(*These are all special cases for the general map:*)


(* ::Input:: *)
(*flowGeneral/.{u0->0,y0->Y}//Simplify;*)
(*(yCoord/.%)==yCoord/.flowCutoff//Simplify*)


(* ::Input:: *)
(*flowGeneral/.{u0->0,y0->1/Y}//Simplify;*)
(*(yCoord/.%)==yCoord/.flowPhased//Simplify*)


(* ::Input:: *)
(*flowGeneral/.y0->(1-u0^2)Y/.u0->Coth[x0]//Simplify;*)
(*(yCoord/.%)==yCoord/.flowGlueon//Simplify*)


(* ::Item:: *)
(*... which we shall derive as follows.*)


(* ::Section::Closed:: *)
(*Rindler maps: derivations*)


(* ::Subsection::Closed:: *)
(*Flow equations & solutions*)


(* ::Input:: *)
(*flow=#[s]&/@coord*)


(* ::Item:: *)
(*We aim to compute the flow lines for Subscript[\[Xi], (u)],Subscript[\[Xi], (v)]. Let us look at Subscript[\[Xi], (u)] first.*)


(* ::Item:: *)
(*This amounts to solving the following ODE:*)


(* ::Input:: *)
(*flowEqs=Thread[\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(s\)]flow\)==(\[Xi]u/.Thread[coord->flow])]*)
(*initEqs=Thread[(flow/.s->0)=={u0,v0,y0}]*)
(**)
(*DSolve[flowEqs~Join~initEqs,flow,s]*)
(**)
(*{solsInside,solsOutside}={%[[{3,4}]],%[[{1,2}]]};*)


(* ::Item:: *)
(*There are two types of solutions, one stays inside the wedge, while the other goes outside the wedge.*)


(* ::Item:: *)
(*Only half of the solutions satisfy the initial & boundary conditions:*)


(* ::Input:: *)
(*Block[{u0=2,v0=2,y0=-5,lu=10,lv=10},*)
(**)
(*Show[*)
(*ParametricPlot3D[*)
(*flow/.solsInside//Evaluate,{s,0,.5},*)
(*AxesLabel->coord,*)
(*PlotRange->{{-5,5},{-5,5},{-10,10}},*)
(*MaxRecursion->15*)
(*],*)
(**)
(*Graphics3D[Ball[{u0,v0,y0},.2]]*)
(*]*)
(*]*)


(* ::Item:: *)
(*Note that this works for both signs of Subscript[y, 0]!*)


(* ::Item:: *)
(*<<< saved image here >>>*)


(* ::Input:: *)
(*flowInside=solsInside[[2]]*)


(* ::Item:: *)
(*For the glue-on side we need to choose the solution that goes outside of the causal diamond. *)


(* ::Input:: *)
(*Block[{u0=2,v0=2,y0=-3,lu=1,lv=1},*)
(**)
(*Show[*)
(*ParametricPlot3D[*)
(*flow/.solsOutside//Evaluate,{s,0,.3},*)
(*AxesLabel->coord,*)
(*PlotRange->{{-5,5},{-5,5},{0,-10}},*)
(*MaxRecursion->15*)
(*],*)
(**)
(*Graphics3D[Ball[{u0,v0,y0},.2]]*)
(*]*)
(*]*)


(* ::Input:: *)
(*Block[{u0=3,v0=3,y0=-5,lu=2,lv=2},*)
(**)
(*Show[*)
(*ParametricPlot3D[*)
(*flow/.solsOutside[[2]]//Evaluate,{s,0,10},*)
(*AxesLabel->coord,*)
(*PlotRange->{{-5,5},{-5,5},{0,-8}},*)
(*MaxRecursion->15*)
(*],*)
(**)
(*Graphics3D[Ball[{u0,v0,y0},.2]],*)
(*Graphics3D[Ball[{lu/2,lv/2,0},.2]]*)
(*]*)
(*]*)


(* ::Item:: *)
(*Also the flow stays outside of the causal diamond (on the right hand side). This is what we expect. *)


(* ::Item:: *)
(*<<< saved images >>>*)


(* ::Input:: *)
(*flowOutside=solsOutside[[2]]*)


(* ::Subsection::Closed:: *)
(*Simplifications: flow inside the wedge*)


(* ::Item:: *)
(*Initiate the flow from (Subscript[u, 0],Subscript[v, 0],Subscript[y, 0])=(Subscript[l, u]/2 Subscript[x, 0],Subscript[l, v]/2 Subscript[x, 0],Subscript[y, 0]), the s parameter is the new coordinate \!\(\*OverscriptBox[\(u\), \(~\)]\), or U for simplicity.*)


(* ::Item:: *)
(*We've rescaled (Subscript[u, 0],Subscript[v, 0],Subscript[y, 0]); effectively we've set Subscript[l, u]=Subscript[l, v]=2:*)


(* ::Item:: *)
(*Flow along the U direction:*)


(* ::Input:: *)
(*flowInside/.Thread[{u0,v0,y0}->{lu/2 u0,lv/2 u0,(lu lv)/4 y0}](*/.u0\[Rule]Tanh[x0]*)/.s->U//Simplify[#,{u0<1}]&*)
(*%/.{u[U]->u0,v[U]->v0,y[U]->y0}*)


(* ::Item:: *)
(*Now for the flow along the V direction, following Subscript[\[Xi], (v)]. The equations are identical under u<->v:*)


(* ::Input:: *)
(*Thread[\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(s\)]flow\)==(\[Xi]v/.Thread[{u,v,y}->flow])]*)


(* ::Input:: *)
(*flowInside/.uvExchange*)
(*flowUVY=flow/.%/.{u0->1/2 lu Tanh[U+ArcSech[Sqrt[1-u0^2]]],v0->(lu lv u0-lu lv u0^3-4 u0 y0+4 y0 Tanh[U+ArcSech[Sqrt[1-u0^2]]])/(2 lu-2 lu u0^2),y0->(y0 Sech[U+ArcSech[Sqrt[1-u0^2]]]^2)/(1-u0^2)}/.s->V/.y0->(lu lv)/4 y0//Simplify*)


(* ::Item:: *)
(*The v-component is much nicer than the u-component, but they should be identical under u<->v. So we have:*)


(* ::Input:: *)
(*flowUVY={1/2 lu Tanh[U+ArcSech[Sqrt[1-(u0 (-1+u0^2+y0)-y0 Tanh[V+ArcSech[Sqrt[1-u0^2]]])^2/(-1+u0^2)^2]]],1/2 lv Tanh[V+ArcSech[Sqrt[1-(u0 (-1+u0^2+y0)-y0 Tanh[U+ArcSech[Sqrt[1-u0^2]]])^2/(-1+u0^2)^2]]],(lu lv (-1+u0^2) y0 Sech[U+ArcSech[Sqrt[1-u0^2]]]^2 Sech[V+ArcSech[Sqrt[1-(u0 (-1+u0^2+y0)-y0 Tanh[U+ArcSech[Sqrt[1-u0^2]]])^2/(-1+u0^2)^2]]]^2)/(4 (-1+u0^6+u0^4 (-3+2 y0)+u0^2 (3-2 y0+y0^2)-2 u0 y0 (-1+u0^2+y0) Tanh[U+ArcSech[Sqrt[1-u0^2]]]+y0^2 Tanh[U+ArcSech[Sqrt[1-u0^2]]]^2))}//Simplify*)


(* ::Input:: *)
(*flowUVY/.y0->0//Simplify*)


(* ::Item:: *)
(*Manual simplification of square roots:*)


(* ::Input:: *)
(*Tanh[U+ArcSech[x]]//TrigExpand//Simplify*)


(* ::Input:: *)
(*(Sqrt[1-x^2]Cosh[U]+Sinh[U])/(Cosh[U]+Sqrt[1-x^2]Sinh[U])/.x->Sqrt[1-(u0 (-1+u0^2+y0)-y0 Tanh[V+ArcSech[Sqrt[1-u0^2]]])^2/(-1+u0^2)^2]//Simplify*)


(* ::Input:: *)
(*(Sinh[U]+Cosh[U] (u0 (-1+u0^2+y0)-y0 Tanh[V+ArcSech[Sqrt[1-u0^2]]])/(-1+u0^2))/(Cosh[U]+Sinh[U] (u0 (-1+u0^2+y0)-y0 Tanh[V+ArcSech[Sqrt[1-u0^2]]])/(-1+u0^2))//Simplify*)


(* ::Input:: *)
(*Tanh[V+ArcSech[x]]//TrigExpand//Simplify*)


(* ::Input:: *)
(*(Sqrt[1-x^2]Cosh[V]+Sinh[V])/(Cosh[V]+Sqrt[1-x^2]Sinh[V])/.x->Sqrt[1-u0^2]//Simplify*)


(* ::Input:: *)
(*((-1+u0^2) Sinh[U]+Cosh[U] (u0 (-1+u0^2+y0)-y0 Tanh[V+ArcSech[Sqrt[1-u0^2]]]))/((-1+u0^2) Cosh[U]+Sinh[U] (u0 (-1+u0^2+y0)-y0 Tanh[V+ArcSech[Sqrt[1-u0^2]]]))/.Tanh[V+ArcSech[Sqrt[1-u0^2]]]->(u0 Cosh[V]+Sinh[V])/(Cosh[V]+u0 Sinh[V])//FullSimplify*)


(* ::Input:: *)
(*(Sinh[U] (Cosh[V]+u0 Sinh[V])+Cosh[U] (u0 Cosh[V]+(u0^2+y0) Sinh[V]))/(Cosh[U] (Cosh[V]+u0 Sinh[V])+Sinh[U] (u0 Cosh[V]+(u0^2+y0) Sinh[V]))//NumeratorDenominator*)
(**)
(*%/(Sinh[U]Sinh[V])//Simplify*)
(**)
(*%[[1]]/%[[2]]*)


(* ::Input:: *)
(*Coth[U] (y0+(u0+Tanh[U])(u0+Coth[V]))/(y0+(u0+Coth[U])(u0+Coth[V]))==(Sinh[U] (Cosh[V]+u0 Sinh[V])+Cosh[U] (u0 Cosh[V]+(u0^2+y0) Sinh[V]))/(Cosh[U] (Cosh[V]+u0 Sinh[V])+Sinh[U] (u0 Cosh[V]+(u0^2+y0) Sinh[V]))//Simplify*)


(* ::Input:: *)
(*HoldForm[Tanh[U] (y0 Coth[U]Tanh[V]+(1+u0 Coth[U])(1+u0 Tanh[V]))/(y0 Tanh[U]Tanh[V]+(1+u0 Tanh[U])(1+u0 Tanh[V]))==Coth[U] (y0+(u0+Tanh[U])(u0+Coth[V]))/(y0+(u0+Coth[U])(u0+Coth[V]))];*)
(*Framed[%]*)
(*Simplify[ReleaseHold[%%]]*)


(* ::Input:: *)
(*Tanh[U] (y0 Coth[U]Tanh[V]+(1+u0 Coth[U])(1+u0 Tanh[V]))/(y0 Tanh[U]Tanh[V]+(1+u0 Tanh[U])(1+u0 Tanh[V]))/.u0->0/.y0->Y*)
(*lu/2 %==u/.flowCutoff//Simplify*)


(* ::Item:: *)
(*This is the result if we keep Subscript[u, 0],Subscript[y, 0] unfixed.*)


(* ::Item:: *)
(*Check asymptotics:*)


(* ::Input:: *)
(*(u0+Coth[V]+Coth[U] (u0^2+y0)+u0 Coth[U]Coth[V])/(u0^2+y0+u0(Coth[V]+Coth[U])+Coth[U]Coth[V])/.y0->0//Simplify*)
(**)
(*%/.u0->Tanh[x0]//Simplify*)
(*%%/.u0->Coth[x0]//Simplify*)


(* ::Item:: *)
(*It seems that it would be convenient to introduce Subscript[u, 0]=coth Subscript[x, 0] or Subscript[u, 0]=tanh Subscript[x, 0], depending on the range of Subscript[u, 0]. *)


(* ::Input:: *)
(*(u0+Coth[V]+Coth[U] (u0^2+y0)+u0 Coth[U]Coth[V])/(u0^2+y0+u0(Coth[V]+Coth[U])+Coth[U]Coth[V])/.y0->1-u0^2//FullSimplify*)
(**)
(*%/.u0->Tanh[x0]//Simplify*)
(*%%/.u0->Coth[x0]//Simplify*)


(* ::Item:: *)
(*Great!*)


(* ::Input:: *)
(*(u0+Coth[V]+Coth[U] (u0^2+y0)+u0 Coth[U]Coth[V])/(u0^2+y0+u0(Coth[V]+Coth[U])+Coth[U]Coth[V])/.y0->(1-u0^2)Y/.u0->Tanh[x0]*)
(**)
(*%/.{U->Ux-x0,V->Vx-x0}//Simplify*)
(**)
(*%//NumeratorDenominator//Collect[#,Y,FullSimplify]&*)
(**)
(*%[[1]]/%[[2]]/.{Ux->U+x0,Vx->V+x0}//Simplify*)


(* ::Input:: *)
(*(Y Cosh[U] Sinh[V]+Sinh[U+x0] Cosh[V+x0])/(Y Sinh[U] Sinh[V]+Cosh[U+x0] Cosh[V+x0]);*)
(**)
(*Framed[%]*)
(*%%/.{{Y->0},{Y->1}}//FullSimplify*)


(* ::Item:: *)
(*The coth Subscript[x, 0] results can be obtained by sinh<->cosh for the terms containing Subscript[x, 0]. *)


(* ::Input:: *)
(*Sinh[U+ArcCoth[Tanh[x0]]]//TrigExpand//FullSimplify*)


(* ::Item:: *)
(*We can also choose some special Subscript[u, 0],Subscript[y, 0].*)


(* ::Input:: *)
(*(Y Cosh[U] Sinh[V]+Sinh[U+x0] Cosh[V+x0])/(Y Sinh[U] Sinh[V]+Cosh[U+x0] Cosh[V+x0])/.x0->0*)
(**)
(*lu/2 %==(u/.flowCutoff)//Simplify*)


(* ::Item:: *)
(*This goes back to our previous result. *)


(* ::Item:: *)
(*What about the y component?*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*(lu lv (-1+u0^2) y0 Sech[U+ArcSech[Sqrt[1-u0^2]]]^2 Sech[V+ArcSech[Sqrt[1-(u0 (-1+u0^2+y0)-y0 Tanh[U+ArcSech[Sqrt[1-u0^2]]])^2/(-1+u0^2)^2]]]^2)/(4 (-1+u0^6+u0^4 (-3+2 y0)+u0^2 (3-2 y0+y0^2)-2 u0 y0 (-1+u0^2+y0) Tanh[U+ArcSech[Sqrt[1-u0^2]]]+y0^2 Tanh[U+ArcSech[Sqrt[1-u0^2]]]^2))/.Tanh[U+ArcSech[Sqrt[1-u0^2]]]->(u0 Cosh[U]+Sinh[U])/(Cosh[U]+u0 Sinh[U])//Simplify*)


(* ::Input:: *)
(*Sech[U+ArcSech[x]]^2 Sech[V+ArcSech[y]]^2//TrigExpand//Simplify*)
(**)


(* ::Input:: *)
(*(x^2 y^2)/((Cosh[U]^2+Sinh[U] (2 Sqrt[1-x^2] Cosh[U]+(1+x)Sinh[U]-x (1+x)Sinh[U])) (Cosh[V]^2+Sinh[V] (2 Sqrt[1-y^2] Cosh[V]+(1+y)Sinh[V]-y (1+y)Sinh[V])))/.{*)
(*x->Sqrt[1-u0^2],y->Sqrt[1-(u0 Cosh[U]+(u0^2+y0) Sinh[U])^2/(Cosh[U]+u0 Sinh[U])^2]*)
(*}//Simplify*)


(* ::Input:: *)
(*((1-u0^2) (1-(u0 Cosh[U]+(u0^2+y0) Sinh[U])^2/(Cosh[U]+u0 Sinh[U])^2))/((Cosh[U]^2+u0^2 Sinh[U]^2+u0 Sinh[2 U]) (Cosh[V]^2+Sinh[V]^2-(1-(u0 Cosh[U]+(u0^2+y0) Sinh[U])^2/(Cosh[U]+u0 Sinh[U])^2) Sinh[V]^2+(u0 Cosh[U]+(u0^2+y0) Sinh[U])/(Cosh[U]+u0 Sinh[U]) Sinh[2 V]))//Simplify*)


(* ::Input:: *)
(*((lu lv y0 (Cosh[U]+u0 Sinh[U])^2)/(4 (-1+u0^2) ((-1+u0^2) Cosh[U]^2-2 u0 Cosh[U] Sinh[U]+(u0^4+y0^2+u0^2 (-1+2 y0)) Sinh[U]^2+u0 (u0^2+y0) Sinh[2 U])))(((-1+u0^2) ((-1+u0^2) Cosh[U]^2-2 u0 Cosh[U] Sinh[U]+(u0^4+y0^2+u0^2 (-1+2 y0)) Sinh[U]^2+u0 (u0^2+y0) Sinh[2 U]))/((Cosh[U]+u0 Sinh[U])^2 (Cosh[U] (Cosh[V]+u0 Sinh[V])+Sinh[U] (u0 Cosh[V]+(u0^2+y0) Sinh[V]))^2))//Simplify*)


(* ::Input:: *)
(*y0 (lu lv)/4 ((Csch[U]Csch[V])/(y0+(u0+Coth[U])(u0+Coth[V])))^2==(lu lv y0)/(4 (Cosh[U] (Cosh[V]+u0 Sinh[V])+Sinh[U] (u0 Cosh[V]+(u0^2+y0) Sinh[V]))^2)//Simplify*)


(* ::Input:: *)
(*HoldForm[y0 (lu lv)/4 ((Csch[U]Csch[V])/(y0+(u0+Coth[U])(u0+Coth[V])))^2==y0 (lu lv)/4 ((Sech[U]Sech[V])/(y0 Tanh[U]Tanh[V]+(1+u0 Tanh[U])(1+u0 Tanh[V])))^2];*)
(*Framed[%]*)
(*Simplify[ReleaseHold[%%]]*)


(* ::Item:: *)
(*This is for generic Subscript[u, 0]. Setting Subscript[u, 0]=tanh Subscript[x, 0] and Subscript[y, 0]=(1-Subscript[u, 0]^2)Y*)


(* ::Input:: *)
(*(lu lv y0)/(4 (Cosh[U] (Cosh[V]+u0 Sinh[V])+Sinh[U] (u0 Cosh[V]+(u0^2+y0) Sinh[V]))^2)/.y0->(1-u0^2)Y/.u0->Tanh[x0]//Simplify*)


(* ::Input:: *)
(*-(-1+Y) Cosh[U-V]+Y Cosh[U+V]+Cosh[U+V+2 x0]//Collect[#,Y,FullSimplify]&//Simplify*)


(* ::Input:: *)
(*(lu lv)/4 Y (Cosh[x0]/(Cosh[U+x0] Cosh[V+x0]+Y Sinh[U] Sinh[V]))^2==(lu lv Y Cosh[x0]^2)/(-(-1+Y) Cosh[U-V]+Y Cosh[U+V]+Cosh[U+V+2 x0])^2//Simplify*)


(* ::Input:: *)
(*Framed[(lu lv)/4 Y (Cosh[x0]/(Cosh[U+x0] Cosh[V+x0]+Y Sinh[U] Sinh[V]))^2]//hideShow*)


(* ::Subsection::Closed:: *)
(*Simplifications: flow outside the wedge*)


(* ::Input:: *)
(*flowOutside*)


(* ::Item:: *)
(*Initiate the flow from (Subscript[u, 0],Subscript[v, 0],Subscript[y, 0])=(Subscript[l, u]/2 Subscript[x, 0],Subscript[l, v]/2 Subscript[x, 0],Subscript[y, 0]), the s parameter is the new coordinate \!\(\*OverscriptBox[\(u\), \(~\)]\), or U for simplicity.*)


(* ::Item:: *)
(*We've rescaled (Subscript[u, 0],Subscript[v, 0],Subscript[y, 0]); effectively we've set Subscript[l, u]=Subscript[l, v]=2:*)


(* ::Item:: *)
(*Flow along the U direction:*)


(* ::Input:: *)
(*flowOutside/.Thread[{u0,v0,y0}->{lu/2 u0,lv/2 u0,(lu lv)/4 y0}]/.s->U//Simplify[#,{u0>1}]&*)
(*%/.{u[U]->u0,v[U]->v0,y[U]->y0}*)


(* ::Item:: *)
(*Now for the flow along the V direction, following Subscript[\[Xi], (v)]. The equations are identical under u<->v:*)


(* ::Input:: *)
(*Thread[\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(s\)]flow\)==(\[Xi]v/.Thread[{u,v,y}->flow])]*)


(* ::Input:: *)
(*flowOutside/.uvExchange*)
(*flowUVY=flow/.%/.{u0->1/2 lu Tanh[U+ArcSech[-I Sqrt[-1+u0^2]]],v0->(lv (u0 (-1+u0^2+y0)-y0 Tanh[U+ArcSech[-I Sqrt[-1+u0^2]]]))/(2 (-1+u0^2)),y0->(lu lv y0 Sech[U+ArcSech[-I Sqrt[-1+u0^2]]]^2)/(4-4 u0^2)}/.s->V//Simplify*)


(* ::Item:: *)
(*The v-component is much nicer than the u-component, but they should be identical under u<->v. So we have:*)


(* ::Input:: *)
(*flowUVY={1/2 lu Tanh[U+ArcSech[-Sqrt[1-(u0 (-1+u0^2+y0)-y0 Tanh[V+ArcSech[-I Sqrt[-1+u0^2]]])^2/(-1+u0^2)^2]]],1/2 lv Tanh[V+ArcSech[-Sqrt[1-(u0 (-1+u0^2+y0)-y0 Tanh[U+ArcSech[-I Sqrt[-1+u0^2]]])^2/(-1+u0^2)^2]]],(lu lv y0 Sech[U+ArcSech[-I Sqrt[-1+u0^2]]]^2 Sech[V+ArcSech[-Sqrt[1-(u0 (-1+u0^2+y0)-y0 Tanh[U+ArcSech[-I Sqrt[-1+u0^2]]])^2/(-1+u0^2)^2]]]^2)/((4-4 u0^2) (1-(u0 (-1+u0^2+y0)-y0 Tanh[U+ArcSech[-I Sqrt[-1+u0^2]]])^2/(-1+u0^2)^2))}//Simplify*)


(* ::Item:: *)
(*Manual simplification of square roots:*)


(* ::Input:: *)
(*Tanh[U+ArcSech[x]]//TrigExpand//Simplify*)


(* ::Input:: *)
(*(Sqrt[1-x^2]Cosh[U]+Sinh[U])/(Cosh[U]+Sqrt[1-x^2]Sinh[U])/.x->-Sqrt[1-(u0 (-1+u0^2+y0)-y0 Tanh[V+ArcSech[-I Sqrt[-1+u0^2]]])^2/(-1+u0^2)^2]//Simplify*)


(* ::Input:: *)
(*(Sinh[U]+Cosh[U] (u0 (-1+u0^2+y0)-y0 Tanh[V+ArcSech[-I Sqrt[-1+u0^2]]])/(-1+u0^2))/(Cosh[U]+Sinh[U] (u0 (-1+u0^2+y0)-y0 Tanh[V+ArcSech[-I Sqrt[-1+u0^2]]])/(-1+u0^2))//Simplify*)


(* ::Input:: *)
(*Tanh[V+ArcSech[x]]//TrigExpand//Simplify*)


(* ::Input:: *)
(*(Sqrt[1-x^2]Cosh[V]+Sinh[V])/(Cosh[V]+Sqrt[1-x^2]Sinh[V])/.x->-I Sqrt[-1+u0^2]//Simplify*)


(* ::Input:: *)
(*((-1+u0^2) Sinh[U]+Cosh[U] (u0 (-1+u0^2+y0)-y0 Tanh[V+ArcSech[-I Sqrt[-1+u0^2]]]))/((-1+u0^2) Cosh[U]+Sinh[U] (u0 (-1+u0^2+y0)-y0 Tanh[V+ArcSech[-I Sqrt[-1+u0^2]]]))/.Tanh[V+ArcSech[-I Sqrt[-1+u0^2]]]->(u0 Cosh[V]+Sinh[V])/(Cosh[V]+u0 Sinh[V])//FullSimplify*)


(* ::Item:: *)
(*This is identical to the result obtained inside the wedge!*)


(* ::Input:: *)
(*(u/.flowGeneral)==lu/2 (Sinh[U] (Cosh[V]+u0 Sinh[V])+Cosh[U] (u0 Cosh[V]+(u0^2+y0) Sinh[V]))/(Cosh[U] (Cosh[V]+u0 Sinh[V])+Sinh[U] (u0 Cosh[V]+(u0^2+y0) Sinh[V]))//Simplify*)


(* ::Item:: *)
(*It seems that it would be convenient to introduce Subscript[u, 0]=coth Subscript[x, 0]:*)


(* ::Input:: *)
(*(u0+Coth[V]+Coth[U] (u0^2+y0)+u0 Coth[U]Coth[V])/(u0^2+y0+u0(Coth[V]+Coth[U])+Coth[U]Coth[V])/.y0->(u0^2-1)Y/.u0->Coth[x0]*)
(**)
(*%/.{U->Ux-x0,V->Vx-x0}//Simplify*)
(**)
(*%//NumeratorDenominator//Collect[#,Y,FullSimplify]&*)
(**)
(*%[[1]]/%[[2]]/.{Ux->U+x0,Vx->V+x0}//Simplify*)


(* ::Input:: *)
(*(Y Cosh[U] Sinh[V]+Cosh[U+x0] Sinh[V+x0])/(Y Sinh[U] Sinh[V]+Sinh[U+x0] Sinh[V+x0]);*)
(**)
(*Framed[%]*)
(*%%/.{{Y->0},{Y->-1}}//FullSimplify*)


(* ::Item:: *)
(*We can also choose some special Subscript[u, 0],Subscript[y, 0].*)


(* ::Item:: *)
(*Setting Subscript[u, 0]=1 seems convenient, although this point is somewhat singular (at the tip of the diamond):*)


(* ::Input:: *)
(*(y0 Coth[U]+(Coth[U]+1)(Coth[V]+1))/(y0+(Coth[U]+1)(Coth[V]+1))==(u0+Coth[V]+Coth[U] (u0^2+y0)+u0 Coth[U]Coth[V])/(u0^2+y0+u0(Coth[V]+Coth[U])+Coth[U]Coth[V])/.u0->1//Simplify*)


(* ::Item:: *)
(*For safety reasons, let's keep Subscript[u, 0]=coth Subscript[x, 0]. *)


(* ::Item:: *)
(*What about the y component?*)


(* ::Input:: *)
(*(lu lv y0 Sech[U+ArcSech[-I Sqrt[-1+u0^2]]]^2 Sech[V+ArcSech[-Sqrt[1-(u0 (-1+u0^2+y0)-y0 Tanh[U+ArcSech[-I Sqrt[-1+u0^2]]])^2/(-1+u0^2)^2]]]^2)/((4-4 u0^2) (1-(u0 (-1+u0^2+y0)-y0 Tanh[U+ArcSech[-I Sqrt[-1+u0^2]]])^2/(-1+u0^2)^2))/.Tanh[U+ArcSech[-I Sqrt[-1+u0^2]]]->(u0 Cosh[U]+Sinh[U])/(Cosh[U]+u0 Sinh[U])//Simplify*)


(* ::Input:: *)
(*Sech[U+ArcSech[x]]^2 Sech[V+ArcSech[y]]^2//TrigExpand//Simplify*)


(* ::Input:: *)
(*(x^2 y^2)/((Cosh[U]^2+Sinh[U] (2 Sqrt[1-x^2] Cosh[U]+(1+x)Sinh[U]-x (1+x)Sinh[U])) (Cosh[V]^2+Sinh[V] (2 Sqrt[1-y^2] Cosh[V]+(1+y)Sinh[V]-y (1+y)Sinh[V])))/.{*)
(*x->-I Sqrt[-1+u0^2],y->-Sqrt[1-(u0 Cosh[U]+(u0^2+y0) Sinh[U])^2/(Cosh[U]+u0 Sinh[U])^2]*)
(*}//Simplify*)


(* ::Input:: *)
(*((1-u0^2) (1-(u0 Cosh[U]+(u0^2+y0) Sinh[U])^2/(Cosh[U]+u0 Sinh[U])^2))/((Cosh[U]^2+u0^2 Sinh[U]^2+u0 Sinh[2 U]) (Cosh[V]^2+Sinh[V]^2-(1-(u0 Cosh[U]+(u0^2+y0) Sinh[U])^2/(Cosh[U]+u0 Sinh[U])^2) Sinh[V]^2+(u0 Cosh[U]+(u0^2+y0) Sinh[U])/(Cosh[U]+u0 Sinh[U]) Sinh[2 V]))//Simplify*)


(* ::Input:: *)
(*((lu lv y0 (Cosh[U]+u0 Sinh[U])^2)/(4 (-1+u0^2) ((-1+u0^2) Cosh[U]^2-2 u0 Cosh[U] Sinh[U]+(u0^4+y0^2+u0^2 (-1+2 y0)) Sinh[U]^2+u0 (u0^2+y0) Sinh[2 U])))(((-1+u0^2) ((-1+u0^2) Cosh[U]^2-2 u0 Cosh[U] Sinh[U]+(u0^4+y0^2+u0^2 (-1+2 y0)) Sinh[U]^2+u0 (u0^2+y0) Sinh[2 U]))/((Cosh[U]+u0 Sinh[U])^2 (Cosh[U] (Cosh[V]+u0 Sinh[V])+Sinh[U] (u0 Cosh[V]+(u0^2+y0) Sinh[V]))^2))//Simplify*)


(* ::Item:: *)
(*This is also identical to the result obtained inside the wedge!*)


(* ::Input:: *)
(*(y/.flowGeneral)==(lu lv y0)/(4 (Cosh[U] (Cosh[V]+u0 Sinh[V])+Sinh[U] (u0 Cosh[V]+(u0^2+y0) Sinh[V]))^2)//Simplify*)


(* ::Item:: *)
(*This is for generic Subscript[u, 0]. Setting Subscript[u, 0]=coth Subscript[x, 0] and Subscript[y, 0]=(1-Subscript[u, 0]^2)Y*)


(* ::Input:: *)
(*(lu lv y0)/(4 (Cosh[U] (Cosh[V]+u0 Sinh[V])+Sinh[U] (u0 Cosh[V]+(u0^2+y0) Sinh[V]))^2)/.y0->(1-u0^2)Y/.u0->Coth[x0]//Simplify*)


(* ::Input:: *)
(*(-1+Y) Cosh[U-V]-Y Cosh[U+V]+Cosh[U+V+2 x0]//Collect[#,Y,FullSimplify]&//Simplify*)


(* ::Input:: *)
(*-((lu lv)/4)Y (Sinh[x0]/(-Y Sinh[U] Sinh[V]+Sinh[U+x0] Sinh[V+x0]))^2==-((lu lv Y Sinh[x0]^2)/((-1+Y) Cosh[U-V]-Y Cosh[U+V]+Cosh[U+V+2 x0])^2)//Simplify*)


(* ::Input:: *)
(*Framed[-((lu lv)/4)Y (Sinh[x0]/(Y Sinh[U] Sinh[V]+Sinh[U+x0] Sinh[V+x0]))^2]//hideShow*)


(* ::Subsection:: *)
(*Checks for the general solution*)


(* ::Input:: *)
(*Limit[coord/.flowGeneral,{U->0,V->0}]*)


(* ::Input:: *)
(*\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(U\)]\((coord /. flowGeneral)\)\)==\[Xi]u/.flowGeneral//Simplify*)
(*\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(V\)]\((coord /. flowGeneral)\)\)==\[Xi]v/.flowGeneral//Simplify*)


(* ::Section:: *)
(*Epilog*)


(* ::Input::Initialization:: *)
saveScript[]:=FrontEndExecute[FrontEndToken[
EvaluationNotebook[],"Save",{
NotebookDirectory[]<>FileBaseName[NotebookFileName[]]<>".wl",
"Script"
}]];

saveScript[]
