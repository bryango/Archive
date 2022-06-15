#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Section:: *)
(*Rindler maps in Poincar\[EAcute] Subscript[AdS, 3] - Extended!*)


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


(* ::Subsection:: *)
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
(*(lu lv y0 (Cosh[U]+u0 Sinh[U])^2)/(4 (-1+u0^2) ((-1+u0^2) Cosh[U]^2-2 u0 Cosh[U] Sinh[U]+(u0^4+y0^2+u0^2 (-1+2 y0)) Sinh[U]^2+u0 (u0^2+y0) Sinh[2 U])) ((-1+u0^2) ((-1+u0^2) Cosh[U]^2-2 u0 Cosh[U] Sinh[U]+(u0^4+y0^2+u0^2 (-1+2 y0)) Sinh[U]^2+u0 (u0^2+y0) Sinh[2 U]))/((Cosh[U]+u0 Sinh[U])^2 (Cosh[U] (Cosh[V]+u0 Sinh[V])+Sinh[U] (u0 Cosh[V]+(u0^2+y0) Sinh[V]))^2)//Simplify*)


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


(* ::Section::Closed:: *)
(*Rindler maps: properties*)


(* ::Input:: *)
(*{\[Xi]u,\[Xi]v}*)


(* ::Input:: *)
(*\[Xi]==2\[Pi] (\[Xi]u-\[Xi]v)/2//Simplify*)


(* ::Subsection::Closed:: *)
(*Cutoff*)


(* ::Item:: *)
(*The horizon is at Y=1:*)


(* ::Input:: *)
(*rindlerLength*)
(*rindlerMetric[[;;2,;;2]]//Det//Simplify*)


(* ::Subsubsection::Closed:: *)
(*Map of coordinates*)


(* ::Input:: *)
(*flowCutoff*)


(* ::Item:: *)
(*The interval and the RT surface resides on the Cauchy slice u/Subscript[l, u]=v/Subscript[l, v], which corresponds to U=V:*)


(* ::Input:: *)
(*0==(Tanh[U]+Y Tanh[V])/(1+Y Tanh[U]Tanh[V])-(Tanh[V]+Y Tanh[U])/(1+Y Tanh[U]Tanh[V])//FullSimplify*)


(* ::Item:: *)
(*Then let us look at the horizon. We have:*)


(* ::Input:: *)
(*flowCutoff/.Y->1//Simplify*)


(* ::Input:: *)
(*Framed[(U+V)/2==1/2 ArcTanh[(2 u)/lu]==1/2 ArcTanh[(2 v)/lv]]//hideShow*)


(* ::Item:: *)
(*Near the RT surface but away from the horizon, we have Y!=1 yet u/Subscript[l, u]=v/Subscript[l, v] should still hold. This leads us to *)


(* ::Input:: *)
(*Tanh[U-V]//TrigExpand*)


(* ::Input:: *)
(*flowCutoff/.Y->0*)


(* ::Input:: *)
(*Framed[{U==ArcTanh[(2u)/lu],V==ArcTanh[(2v)/lv]}]//hideShow*)


(* ::Input:: *)
(*ArcTanh[x]//TrigToExp//Simplify*)


(* ::Input:: *)
(*ArcTanh[x]==1/2 Log[(1+x)/(1-x)]//Framed//hideShow*)


(* ::Subsubsection::Closed:: *)
(*Inverting the map (incomplete)*)


(* ::Input:: *)
(*Thread[yCoord==(yCoord/.flowCutoff)]*)
(*Solve[%,{U,V,Y}]//Simplify*)
(*{U,V}/.%*)


(* ::Subsubsection::Closed:: *)
(*Extension to the glue-on side*)


(* ::Input:: *)
(*flowCutoff*)


(* ::Item:: *)
(*Try extending this to negative values of y and Y. *)


(* ::Input:: *)
(*yCoord/.flowCutoff/.sameL/.{l->2,Y->-.9}//Simplify;*)
(*ParametricPlot3D[*)
(*Evaluate[%],{U,-5,5},{V,-5,5},*)
(*AxesLabel->{u,v,y},PlotRange->All,Mesh->None,*)
(*MaxRecursion->4*)
(*]*)


(* ::Item:: *)
(*This is not working as expected. We want to explore the complement!*)


(* ::Item:: *)
(*Check its value along the RT surface:*)


(* ::Input:: *)
(*y+u v-(lu lv)/4/.flowCutoff//Simplify*)
(*%/.Y->-1//FullSimplify*)
(**)
(*y+u v+(lu lv)/4/.flowCutoff/.Y->-1//Simplify*)


(* ::Subsection::Closed:: *)
(*Complement*)


(* ::Item:: *)
(*\[Xi]=2\[Pi] \!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(U\)]\(-*)
(*\*SubscriptBox[\(\[PartialD]\), \(V\)]\)\)/2=\!\(2  \[Pi]\ *)
(*\*SubscriptBox[\(\[PartialD]\), \(\[Tau]\)]\), so \[Tau]=U-V, or U,V=(\[Rho]\[PlusMinus]\[Tau])/2. Note the 1/2 factor. *)


(* ::Item:: *)
(*We can zoom in to one of the endpoints:*)


(* ::Input:: *)
(*flowCutoff/.Y->0*)


(* ::Input:: *)
(*yCoord/.flowCutoff/.{U->ArcTanh[x0]+U,V->ArcTanh[x0]+V}//TrigExpand//Simplify;*)
(*Series[%/.x0->-1+\[Epsilon],{\[Epsilon],0,1}]//FullSimplify*)
(*Normal[%]/.Y->0*)
(**)
(*Dt[u]Dt[v]/.Thread[{u,v}->%[[1;;2]]]/.constParams/.Dt[\[Epsilon]]->0//FullSimplify*)
(**)
(*(lu lv)/4 \[Epsilon]^2 Dt[E^(2U)]Dt[E^(2V)]/.constParams//FullSimplify*)


(* ::Item:: *)
(*On the other hand, we have:*)


(* ::Input:: *)
(*uv2xt/.{x->\[Rho] Cosh[\[Tau]],t->\[Rho] Sinh[\[Tau]]}//FullSimplify*)
(**)
(*Dt[u]Dt[v]/.%//Simplify*)
(**)
(*%/.{\[Tau]->U-V,\[Rho]->Exp[U+V]}//Simplify*)


(* ::Item:: *)
(*We confirm that the Rindler time \[Tau] corresponds to U-V. *)


(* ::Item:: *)
(*Adding I \[Pi] phase to \[Tau] while keeping \[Rho] invariant, amounts to adding \[PlusMinus]I \[Pi]/2 phases to U,V. *)


(* ::Input:: *)
(*{Tanh[X+I \[Pi]/2],Tanh[X-I \[Pi]/2],Sech[X+I \[Pi]/2],Sech[X-I \[Pi]/2]}*)


(* ::Item:: *)
(*This gives us the Rindler map for the complement:*)


(* ::Input:: *)
(*flowPhased*)


(* ::Input:: *)
(*flowCutoff/.{U->U+I \[Pi]/2,V->V-I \[Pi]/2}//FullSimplify*)
(*%==flowPhased//FullSimplify*)


(* ::Item:: *)
(*We note that this solution can also be obtained by a simple replacement of Y->1/Y:*)


(* ::Input:: *)
(*yCoord/.flowCutoff/.Y->1/Y//Simplify*)
(*yCoord/.flowPhased*)
(*Thread[%==%%]//Simplify*)


(* ::Input:: *)
(*rindlerLength/.Y->1/Y//Simplify*)
(*%==rindlerLength//Simplify*)


(* ::Item:: *)
(*Note that it lives in the same family of solutions as the original map, as the initial point is Subscript[(u,v), 0]=(0,0), but now we have Subscript[y, 0]\[Proportional]1/Y instead of Subscript[y, 0]\[Proportional]Y:*)


(* ::Input:: *)
(*Series[yCoord/.flowPhased//Evaluate,{U,0,1},{V,0,1}]*)
(*Normal[%]//Simplify*)
(**)
(*%/.{U->0,V->0}*)
(*yCoord/.flowCutoff/.{U->0,V->0}*)


(* ::Input:: *)
(*yCoord/.flowPhased/.sameL//Simplify;*)
(*flowCutoffSimp[Y_,l_:2]=%//MapAt[zFunction,-1];*)
(**)
(*Show[*)
(*ParametricPlot3D[*)
(*Evaluate[flowCutoffSimp/@{.99,-.99}],*)
(*{U,-10,10},{V,-10,10},*)
(*AxesLabel->{u,v,y},PlotRange->{{-3,3},{-3,3},{-3,3}},Mesh->None,*)
(*MaxRecursion->3*)
(*],*)
(*ParametricPlot3D[{*)
(*{*)
(*{u0,-u0,-1+u0^2},*)
(*{u0,u0,1-u0^2}*)
(*}//MapAt[zFunction,{All,-1}],*)
(*{*)
(*{u0,-u0,1+Abs[u0]},*)
(*{u0,u0,-1-Abs[u0]},*)
(*{u0,-u0,\!\(\**)
(*TagBox[GridBox[{*)
(*{"\[Piecewise]", GridBox[{*)
(*{*)
(*RowBox[{*)
(*RowBox[{"-", "1"}], "+", *)
(*RowBox[{"Abs", "[", "u0", "]"}]}], *)
(*RowBox[{*)
(*RowBox[{"Abs", "[", "u0", "]"}], ">", "1"}]},*)
(*{"Indeterminate", "True"}*)
(*},*)
(*AllowedDimensions->{2, Automatic},*)
(*Editable->True,*)
(*GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},*)
(*GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}},*)
(*GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}},*)
(*Selectable->True]}*)
(*},*)
(*GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},*)
(*GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}},*)
(*GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}}],*)
(*"Piecewise",*)
(*DeleteWithContents->True,*)
(*Editable->False,*)
(*SelectWithContents->True,*)
(*Selectable->False,*)
(*StripWrapperBoxes->True]\)},*)
(*{u0,u0,\!\(\**)
(*TagBox[GridBox[{*)
(*{"\[Piecewise]", GridBox[{*)
(*{*)
(*RowBox[{"1", "-", *)
(*RowBox[{"Abs", "[", "u0", "]"}]}], *)
(*RowBox[{*)
(*RowBox[{"Abs", "[", "u0", "]"}], ">", "1"}]},*)
(*{"Indeterminate", "True"}*)
(*},*)
(*AllowedDimensions->{2, Automatic},*)
(*Editable->True,*)
(*GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},*)
(*GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}},*)
(*GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}},*)
(*Selectable->True]}*)
(*},*)
(*GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},*)
(*GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}},*)
(*GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}}],*)
(*"Piecewise",*)
(*DeleteWithContents->True,*)
(*Editable->False,*)
(*SelectWithContents->True,*)
(*Selectable->False,*)
(*StripWrapperBoxes->True]\)}*)
(*}*)
(*}//Apply[Join]//Evaluate,*)
(*{u0,-3,3},*)
(*PlotStyle->(Directive[#,Thickness[.008],Dashing[{.02,.01}]]&/@{RGBColor[0.922526, 0.385626, 0.209179],RGBColor[0.922526, 0.385626, 0.209179],RGBColor[0.880722, 0.611041, 0.142051],RGBColor[0.368417, 0.506779, 0.709798],RGBColor[0.880722, 0.611041, 0.142051],RGBColor[0.368417, 0.506779, 0.709798]}//Evaluate),*)
(*MaxRecursion->4*)
(*]*)
(*]*)


(* ::Input:: *)
(*yCoord/.flowCutoff/.sameL//Simplify;*)
(*flowCutoffSimp[Y_,l_:2]=%//MapAt[zFunction,-1];*)
(**)
(*Module[{Yvalues={1.01,.99,-.99,-1.01}},*)
(*ParametricPlot3D[*)
(*Evaluate[flowCutoffSimp/@Yvalues],*)
(*{U,-10,10},{V,-10,10},*)
(*AxesLabel->{SuperPlus[w],SuperMinus[w],z},PlotRange->{{-2,2},{-2,2},{-2,2}},Mesh->None,*)
(*MaxRecursion->4*)
(*]*)
(*]*)


(* ::Item:: *)
(*This is nice, but it is not quite the glue-on modular flow. *)


(* ::Item:: *)
(*In particular, the horizon does not coincide with the glue-on RT surface:*)


(* ::Input:: *)
(*y+u v-(lu lv)/4/.flowPhased//Simplify*)
(*%/.Y->-1//FullSimplify*)
(**)
(*y+u v+(lu lv)/4/.flowPhased/.Y->-1//Simplify*)


(* ::Item:: *)
(*Actually, its fixed points agree with the cutoff modular flow:*)


(* ::Input:: *)
(*y+u v-(lu lv)/4/.flowCutoff//Simplify*)
(*%/.Y->-1//FullSimplify*)
(**)
(*y+u v+(lu lv)/4/.flowCutoff/.Y->-1//Simplify*)


(* ::Item:: *)
(*It has the correct asymptotics along infinity, but is incorrect along the horizon:*)


(* ::Input:: *)
(*flowPhased/.Y->0*)
(*flowPhased/.Y->-1//FullSimplify*)
(*flowPhased/.Y->+1//FullSimplify*)


(* ::Input:: *)
(*Series[yCoord/.flowPhased,Y->0]//Simplify*)
(**)
(*Normal[%]/.{U->ArcCoth[u/(lu/2)],V->ArcCoth[v/(lv/2)]}//Simplify*)
(**)
(*(Last[%]/Y)/.{u->lu/2+u,v->lv/2+v}//Simplify*)
(*%//ExpandAll*)


(* ::Subsection::Closed:: *)
(*Glue-on*)


(* ::Input:: *)
(*flowGlueon*)
(*%/.{U->0,V->0}//Simplify*)


(* ::Item:: *)
(*We see that the resulting metric is not quite Fefferman-Graham:*)


(* ::Input::Initialization:: *)
glueonRindlerLength=(Dt[Y]/(2Y)-Coth[x0](Dt[U]+Dt[V]))^2-(Csch[x0]^2 (Dt[V]+Y Dt[U]) (Dt[U]+Y Dt[V]))/Y;
glueonRindlerMetric=lengthToMetric[%,{U,V,Y}]//FullSimplify


(* ::Input:: *)
(*glueonRindlerLength==yLength/.flowGlueon/.constParams/.Dt[x0]->0//FullSimplify*)


(* ::Item:: *)
(*The horizon is located at Y=1:*)


(* ::Input:: *)
(*glueonRindlerMetric//Det//Simplify*)


(* ::Subsubsection::Closed:: *)
(*Map of coordinates*)


(* ::Input:: *)
(*flowGlueon*)


(* ::Input:: *)
(*jacobianFromLists[up_List,down_List]:=Outer[\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(down[[#2]]\)]\(up[[#1]]\)\)&,Range[Length[up]],Range[Length[down]]];*)
(*jacobianFromLists[yCoord/.flowGlueon,rindlerCoord]//Det//Simplify*)
(**)
(*%/.sameL/.l->1/.{U->3,V->3,Y->.5`50,x0->2}*)


(* ::Item:: *)
(*We see that the map flips orientation. *)


(* ::Item:: *)
(*The interval and the RT surface resides on the Cauchy slice u/Subscript[l, u]=v/Subscript[l, v], which corresponds to U=V:*)


(* ::Input:: *)
(*u/lu-v/lv/.flowGlueon//FullSimplify*)


(* ::Item:: *)
(*Then let us look at the "horizon". We have:*)


(* ::Input:: *)
(*flowGlueon/.Y->1//Simplify*)


(* ::Input:: *)
(*Framed[(U+V+x0)/2==1/2 ArcCoth[(2 u)/lu]==1/2 ArcCoth[(2 u)/lu]]//hideShow*)


(* ::Input:: *)
(*flowGlueon/.Y->0*)


(* ::Input:: *)
(*Framed[{U==ArcCoth[(2u)/lu],V==ArcCoth[(2v)/lv]}]//hideShow*)


(* ::Input:: *)
(*ArcCoth[x]//TrigToExp//Simplify*)


(* ::Input:: *)
(*ArcCoth[x]==1/2 Log[(x+1)/(x-1)]//Framed//hideShow*)


(* ::Subsubsection::Closed:: *)
(*Analytic continuation back to cutoff*)


(* ::Input:: *)
(*glueonRindlerLength*)


(* ::Input:: *)
(*rindlerLength*)


(* ::Item:: *)
(*A way to kill the cross term is to set:*)


(* ::Input:: *)
(*Solve[Coth[x0]==0]*)


(* ::Input:: *)
(*{Coth[x0],Csch[x0]^2}/.x0->I \[Pi]/2+I \[Pi](-1)*)


(* ::Input:: *)
(*glueonRindlerLength/.x0->+I \[Pi]/2//FullSimplify*)
(*%==rindlerLength//Simplify*)


(* ::Item:: *)
(*The metric becomes Fefferman-Graham!*)


(* ::Item:: *)
(*What happens to the coordinate map?*)


(* ::Input:: *)
(*flowGlueon/.{{x0->I \[Pi]/2},{x0->-I \[Pi]/2}}//FullSimplify*)
(*Union[%]//Flatten*)
(**)
(*yCoord/.%//FullSimplify*)
(*%==(yCoord/.flowCutoff)//Simplify*)


(* ::Item:: *)
(*Fascinating! It goes back to the cutoff Rindler map!*)


(* ::Subsubsection::Closed:: *)
(*Attempt to remove the cross terms: Fefferman-Graham with higher modes*)


(* ::Item:: *)
(*The attempt to remove the cross term spoils the Killing vectors Subscript[\[PartialD], U,V]. *)


(* ::Input:: *)
(*Dt[1/2 Log[Y]-Coth[x0]U]^2/.Dt[x0]->0//Expand*)


(* ::Input:: *)
(*Solve[1/2 Log[Yx]==1/2 Log[Y]-Coth[x0]U,Yx]*)


(* ::Input:: *)
(*yLength/.flowGlueon/.Y->(Y Exp[2Coth[x0](U+V)])/.constParams/.Dt[x0]->0//FullSimplify*)
(**)
(*%/.Dt[Y]->0*)
(**)
(*%/.{U->-Log[-2U Cosh[x0]]/(2Coth[x0]),V->-Log[+2V Cosh[x0]]/(2Coth[x0])}/.Dt[x0]->0//FullSimplify*)
(*%==1/Y (Dt[U]-Y (1/(2V Cosh[x0]))^2 Dt[V])(Dt[V]-Y (1/(2U Cosh[x0]))^2 Dt[U])//Simplify*)


(* ::Item:: *)
(*The metric is now in the form of Fefferman-Graham.*)


(* ::Item:: *)
(*In summary, we have:*)


(* ::Input:: *)
(*Y->(Y Exp[2Coth[x0](U+V)])/.{U->-Log[-2U Cosh[x0]]/(2Coth[x0]),V->-Log[+2V Cosh[x0]]/(2Coth[x0])}//Simplify*)


(* ::Input::Initialization:: *)
glueonRindlerBanadosLength=(Dt[Y]/(2Y))^2+1/Y (Dt[U]-Y (1/(2V Cosh[x0]))^2 Dt[V])(Dt[V]-Y (1/(2U Cosh[x0]))^2 Dt[U]);


(* ::Input:: *)
(*yLength/.flowGlueon/.{*)
(*U->-1/2 Tanh[x0]Log[-2U Cosh[x0]],*)
(*V->-1/2 Tanh[x0]Log[+2V Cosh[x0]],*)
(*Y->-(Y/(4 U V Cosh[x0]^2))*)
(*}/.constParams/.Dt[x0]->0//FullSimplify*)
(**)
(*%==glueonRindlerBanadosLength//Simplify*)


(* ::Input:: *)
(*{*)
(*U->-1/2 Sech[x0]Exp[-2U Coth[x0]],*)
(*V->+1/2 Sech[x0]Exp[-2V Coth[x0]]*)
(*};*)
(**)
(*Y->-4Y U V Cosh[x0]^2/.%//Simplify*)
(**)
(*glueonRindlerBanadosLength/.%%/.%/.constParams/.Dt[x0]->0//FullSimplify;*)
(*%==glueonRindlerLength//Simplify*)


(* ::Item:: *)
(*The metric is Fefferman-Graham, but with higher modes:*)


(* ::Input:: *)
(*glueonRindlerBanadosLength*)


(* ::Input:: *)
(*lengthToMetric[glueonRindlerBanadosLength,rindlerCoord]//Det//Simplify*)


(* ::Subsubsection::Closed:: *)
(*Relation with the complement map [WIP]*)


(* ::Input:: *)
(*flowGlueon/.Y->0*)


(* ::Input:: *)
(*flowPhased/.{U->x0+U,V->x0+V}*)
(*%/.Y->0*)


(* ::Input:: *)
(*yLength/.(flowPhased/.{U->x0+U,V->x0+V})/.constParams/.Dt[x0]->0//Simplify*)


(* ::Item:: *)
(*We should be able to relate these two in the overlapping region, since the solution should be unique. *)


(* ::Input:: *)
(*yCoord/.flowPhased/.{U->x,V->x}/.Y->Yx//FullSimplify*)
(*%/.Yx->0*)
(*yCoord/.flowGlueon/.{U->0,V->0}//FullSimplify*)
(*%/.Y->0*)
(**)
(*Thread[%%==%%%%]//FullSimplify[#,{lu>0,lv>0}]&//Union*)
(**)
(*Solve[%[[1]],Yx]*)
(*%%[[2]]/.First[%]//FullSimplify*)


(* ::Item:: *)
(*We know that when Y=0, we have Subscript[Y, x]=0, and x=Subscript[x, 0]. *)


(* ::Input:: *)
(*Solve[(lu lv Yx Csch[x]^4)/(4 (1+Yx Coth[x]^2)^2)==-(1/4) lu lv Y Csch[x0]^2,#]&/@{Y,Yx}//Simplify*)


(* ::Item:: *)
(*Something familiar happened when we try to normalize the glue-on metric...*)


(* ::Input:: *)
(*(lu (Coth[x0]+Yx Coth[x0]))/(2 (1+Yx Coth[x0]^2))/.Yx->((-2 Y Coth[x0]^2-Csch[x0]^2+Sqrt[1+2 Y+2 Y Cosh[2 x0]] Csch[x0]^2) Tanh[x0]^4)/(2 Y)//FullSimplify*)
(*%/.Y->1//FullSimplify*)


(* ::Item:: *)
(*Hey! That's interesting!*)


(* ::Input:: *)
(*Yx*)


(* ::Input:: *)
(*{x0->ArcCoth[((1+Yx) Coth[x])/(1+Yx Coth[x]^2)]};*)
(**)
(*Solve[-(1/4) lu lv Y Csch[x0]^2==(lu lv Yx)/(-1+Yx+(1+Yx) Cosh[2 x])^2,Y]*)
(*First[%]/.%%//Simplify*)
(**)
(*Join[%,%%%]/.Yx->Y/.x->x0//FullSimplify*)


(* ::Input::Initialization:: *)
glue2phased={Y->(2Y)/((Y^2+1)+(Y^2-1)Cosh[2x0]),x0->ArcCoth[((1+Y)Coth[x0])/(1+Y Coth[x0]^2)]};


(* ::Input:: *)
(*{Y,x0}/.{{Y->(2 Y)/(1+Y^2+(-1+Y^2) Cosh[2 x0]),x0->ArcCoth[((1+Y) Coth[x0])/(1+Y Coth[x0]^2)]},glue2phased}//Simplify*)
(*%[[1]]==%[[2]]*)


(* ::Input:: *)
(*flowGlueon/.glue2phased/.{U->0,V->0}//FullSimplify*)
(*flowPhased/.{U->x0,V->x0}//FullSimplify*)
(**)
(*%==%%*)


(* ::Input:: *)
(*yLength/.(flowGlueon/.glue2phased)/.constParams/.Dt[x0]->0//Simplify*)


(* ::Input:: *)
(*glueonRindlerLength/.Y->(2Y)/((Y^2+1)+(Y^2-1)Cosh[2x0])*)
(**)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*flowGlueon/.glue2phased//timedSimplify*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*y+u v-(lu lv)/4/.(flowPhased/.{U->x0+U,V->x0+V})//Simplify*)
(*%/.Y->-1*)


(* ::Subsubsection::Closed:: *)
(*Visualizations*)


(* ::Input:: *)
(*flowGlueon*)


(* ::Input:: *)
(*ParametricPlot3D[*)
(*Evaluate[*)
(**)
(*yCoord/.#/.sameL/.{x0->1,l->2,Y->+.9}&/@{*)
(*flowGlueon,flowCutoff*)
(*}*)
(**)
(*],{U,-5,5},{V,-5,5},*)
(*AxesLabel->{u,v,y},PlotRange->(*All*){{-4,4},{-4,4},{-5,2}},Mesh->None,*)
(*MaxRecursion->5*)
(*]*)


(* ::Item:: *)
(*Let's take a closer look at the behavior of the map:*)


(* ::Input:: *)
(*yCoord/.flowCutoff/.sameL/.{l->2,Y->+.9}//Simplify;*)
(*(*yCoord/.flowCutoff/.sameL/.{l\[Rule]2,Y\[Rule]-.9}//Simplify;*)*)
(*yCoord/.flowGlueon/.x0->2/.sameL/.{l->2,Y->+.9}//Simplify;*)
(*ParametricPlot3D[*)
(*Evaluate[{%,%%(*,%%%*)}/.{U->(x+t)/2,V->(x-t)/2}],{x,-2.5,1.5},{t,-3,5},*)
(*AxesLabel->{u,v,y},PlotRange->(*All*){{-4,4},{-4,4},{-5,2}},Mesh->None,*)
(*MaxRecursion->5*)
(*]*)


(* ::Input:: *)
(*\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(U\)]\((u /. flowGlueon // Evaluate)\)\)//Simplify*)
(**)
(*%==lu/2 (1-(u/(lu/2))^2)/.flowGlueon//Simplify*)


(* ::Item:: *)
(*The image goes to \[Infinity] as:*)


(* ::Input:: *)
(*(-Y Sinh[U] Sinh[V]+Sinh[U+x0] Sinh[V+x0])/.Y->(Sinh[U+x0]Sinh[V+x0])/(Sinh[U]Sinh[V])*)


(* ::Item:: *)
(*For example,*)


(* ::Input:: *)
(*NSolve[1.5==(Sinh[U+x0]Sinh[V+x0])/(Sinh[U]Sinh[V])/.V->U/.x0->2//Evaluate,U,Reals]//Simplify*)


(* ::Item:: *)
(*One can check this in the graph above.*)


(* ::Subsubsection::Closed:: *)
(*Attempt to simplify by rescaling Y [bad]*)


(* ::Item:: *)
(*Attempting to rescale Y:*)


(* ::Input:: *)
(*Solve[(1-((-1+Y)^2 Csch[x0]^2)/(2 Y)/.Y->Y0)==(1+Y^2)/(2 Y)//Evaluate,Y0]//FullSimplify*)
(**)
(*Series[Y0/.%,Y->0]*)
(*Series[Y0/.%%,Y->-1]//Simplify*)
(**)


(* ::Item:: *)
(*It seems that the first solution is the one. *)


(* ::Input:: *)
(*yLength/.flowGlueon/.Y->((-(-1+Y)^2+2 Y Csch[x0]^2+Sqrt[(-1+Y)^4-4 (-1+Y)^2 Y Csch[x0]^2]) Sinh[x0]^2)/(2 Y)/.constParams/.Dt[x0]->0;*)
(*lengthToMetric[%,{U,V,Y}]//timedSimplify*)
(**)
(*(*%\[Equal]rindlerLength//Simplify*)*)


(* ::Item:: *)
(*This is not ideal!*)


(* ::Subsubsection::Closed:: *)
(*Attempt to simplify by sending Subscript[x, 0]->\[Infinity] [singular]*)


(* ::Item:: *)
(*How about we send Subscript[x, 0]->\[Infinity]?*)


(* ::Input:: *)
(*Limit[glueonRindlerMetric,x0->\[Infinity]]//Simplify*)


(* ::Item:: *)
(*The metric is wonderful, but the map degenerates:*)


(* ::Input:: *)
(*yCoord/.flowGlueon/.x0->ArcCoth[x0]//TrigExpand//Simplify*)
(**)
(*Series[%,x0->1]//FullSimplify//ExpToTrig//FullSimplify*)


(* ::Item:: *)
(*Maybe we re-run the simplification and set coth Subscript[x, 0]->1 early on?*)


(* ::Subsubsection::Closed:: *)
(*Attempt to simplify with coth Subscript[x, 0]->1 [not so good]*)


(* ::Item:: *)
(*Recall that this is the u component:*)


(* ::Input:: *)
(*u/.flowGeneral*)


(* ::Item:: *)
(*This is the result if we keep Subscript[u, 0],Subscript[y, 0] unfixed.*)


(* ::Item:: *)
(*We can also choose some special Subscript[u, 0],Subscript[y, 0].*)


(* ::Item:: *)
(*Setting Subscript[u, 0]=+1 seems convenient, although this point is somewhat singular (at the right tip of the diamond):*)


(* ::Input:: *)
(*(y0 Coth[U]+(Coth[U]+1)(Coth[V]+1))/(y0+(Coth[U]+1)(Coth[V]+1))==(u0+Coth[V]+Coth[U] (u0^2+y0)+u0 Coth[U]Coth[V])/(u0^2+y0+u0(Coth[V]+Coth[U])+Coth[U]Coth[V])/.u0->1//Simplify*)


(* ::Input:: *)
(*(y0 Coth[U]+(Coth[U]+1)(Coth[V]+1))/(y0+(Coth[U]+1)(Coth[V]+1))//Framed//hideShow*)


(* ::Item:: *)
(*One can no longer place the RT surface at Y=1 by setting Subscript[y, 0]=(1-Subscript[u, 0]^2)Y, as:*)


(* ::Input:: *)
(*(1-u0^2)Y/.u0->1*)


(* ::Item:: *)
(*This is inconvenient. Again this is due to the fact that Subscript[u, 0]=1 is at the tip of the diamond.*)


(* ::Item:: *)
(*Let us proceed in any case. Recall that this is the y component:*)


(* ::Input:: *)
(*y/.flowGeneral*)
(*%/.u0->1//Simplify//Framed*)


(* ::Input::Initialization:: *)
flowRight=Thread[{u,v,y}->{lu/2 (Y Coth[U]+(Coth[U]+1)(Coth[V]+1))/(Y+(Coth[U]+1)(Coth[V]+1)),lv/2 (Y Coth[V]+(Coth[U]+1)(Coth[V]+1))/(Y+(Coth[U]+1)(Coth[V]+1)),Y (lu lv)/4 ((Csch[U]Csch[V])/(Y+(Coth[U]+1)(Coth[V]+1)))^2}];


(* ::Input:: *)
(*yLength/.flowRight/.constParams//FullSimplify//Expand*)
(*lengthToMetric[%,rindlerCoord]//FullSimplify//Apart*)


(* ::Item:: *)
(*This is closed to, but not exactly the same as the limit of the glue-on Rindler metric:*)


(* ::Input:: *)
(*Limit[glueonRindlerLength,x0->\[Infinity]]//FullSimplify*)
(*Limit[glueonRindlerMetric,x0->\[Infinity]]//Simplify*)


(* ::Item:: *)
(*This should be related with the flow of the complement.*)


(* ::Input:: *)
(*flowPhased*)
(*Series[yCoord/.flowPhased,{U,0,1},{V,0,1}]*)
(*Normal[%]//Simplify*)


(* ::Input:: *)
(*Series[yCoord/.flowRight/.Y->1/Y,{U,0,1},{V,0,1}]*)
(*Normal[%]//FullSimplify*)


(* ::Item:: *)
(*This probably isn't the one we are interested in, as it probably never touches the asymptotic boundary.*)


(* ::Section::Closed:: *)
(*Rindler maps: orthogonality*)


(* ::Input:: *)
(*initialCurvesPoincare*)


(* ::Item:: *)
(*We would like to impose that \!\(\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(Y\)]\(\(\[Perpendicular]\)*)
(*\*SubscriptBox[\(\[PartialD]\), \(U\)]\)\), *)
(*\*SubscriptBox[\(\[PartialD]\), \(V\)]\), which will then ensure that the metric components Subscript[g, Y U]=Subscript[g, Y V]=0. *)


(* ::Input:: *)
(*{\[Xi]u,\[Xi]v} . lengthToMetric[yLength,yCoord] . {Subscript[u, y],Subscript[v, y],Subscript[y, y]}//Simplify*)
(**)
(*Thread[%=={0,0}]//Apply[And]//Solve[#,{Subscript[u, y],Subscript[v, y]}]&//Simplify*)
(**)
(*First[%]/.Subscript[y, y]->((lu/2)^2-u^2)((lv/2)^2-v^2)-y^2//FullSimplify*)


(* ::Item:: *)
(*... where we've noted that:*)


(* ::Input:: *)
(*-lu^2 (lv^2-4 v^2)+4 (lv^2 u^2-4 u^2 v^2+4 y^2)//FullSimplify*)


(* ::Item:: *)
(*Therefore, we have a nice choice:*)


(* ::Input::Initialization:: *)
\[Xi]y={v (lu/2)^2-u(u v+y),u (lv/2)^2-v(u v+y),((lu/2)^2-u^2)((lv/2)^2-v^2)-y^2}/((lu lv)/4);


(* ::Input:: *)
(*{\[Xi]u,\[Xi]v} . lengthToMetric[yLength,yCoord] . \[Xi]y//Simplify*)


(* ::Input:: *)
(*\[Xi]y . lengthToMetric[yLength,yCoord] . \[Xi]y//ExpandAll//FullSimplify*)


(* ::Item:: *)
(*Note that Subscript[\[PartialD], Y] is fixed up to a overall rescaling.*)


(* ::Item:: *)
(*It may be time-like / space-like / null, depending on the causal regime!*)


(* ::Subsection::Closed:: *)
(*General discussions*)


(* ::Item:: *)
(*Both Subscript[u, 0],Subscript[y, 0] can depend non-trivially on the radial Y. *)


(* ::Item:: *)
(*The solution with general Y dependence is given by:*)


(* ::Input:: *)
(*flowGeneralY*)


(* ::Item:: *)
(*The U,V dependence guarantees that it is the integral curve of \[Xi]:*)


(* ::Input:: *)
(*yCoord/.flowGeneralY*)


(* ::Input:: *)
(*\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(U\)]\((yCoord /. flowGeneralY)\)\)//Simplify;*)
(*%==(\[Xi]u/.flowGeneralY)//Simplify*)


(* ::Item:: *)
(*We then fix the direction of \!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(Y\)]\(\(//\)*)
(*\*SubscriptBox[\(\[Xi]\), \((Y)\)]\)\):*)


(* ::Input:: *)
(*\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(Y\)]\((yCoord /. flowGeneralY)\)\)//Simplify*)
(*\[Xi]y/.flowGeneralY//Simplify;*)
(**)
(*(**)
(*%%/%//Simplify;*)
(*%\[LeftDoubleBracket]1\[RightDoubleBracket]\[Equal]%\[LeftDoubleBracket]2\[RightDoubleBracket]//Simplify*)
(**)*)
(**)
(*{%,%%}[[;;2,;;2]]//Det//Simplify*)


(* ::Item:: *)
(*A natural choice would be Subscript[y, 0]=1-Subscript[u, 0]^2, however this is bad since:*)


(* ::Input:: *)
(*\[Xi]y/.flowGeneralY/.y0->(1-u0[#]^2&)//Simplify*)


(* ::Item:: *)
(*It is the fixed point of the flow, so there will be no flow if we start from that. *)


(* ::Item:: *)
(*The only other choice is thus:*)


(* ::Input:: *)
(*((-1+u0[Y]^2-y0[Y]) Derivative[1][u0][Y]+u0[Y] Derivative[1][y0][Y])==0;*)


(* ::Item:: *)
(*One solution is Subscript[u, 0]\[Congruent]0 with arbitrary Subscript[y, 0]. This is the case for the original map & the complement map.*)


(* ::Item:: *)
(*The remaining possibility is then given by:*)


(* ::Input:: *)
(*DSolve[u y'[u]==y[u]-(u^2-1),y[u],u]//Simplify*)


(* ::Input:: *)
(*((-1+u0[Y]^2-y0[Y]) Derivative[1][u0][Y]+u0[Y] Derivative[1][y0][Y])/.y0->(-1-u0[#]^2+Subscript[\[ConstantC], 0]u0[#]&)//Simplify*)


(* ::Item:: *)
(*That's surprisingly simple, yet somewhat unexpected, as the geodesic is y=+1-u^2. *)


(* ::Item:: *)
(*The equation y=-1-u^2 does remind us of the "negative horizon" of the complement Rindler map.*)


(* ::Item:: *)
(*In any case, let us check that it is indeed correct:*)


(* ::Input:: *)
(*\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(Y\)]\((\(yCoord /. flowGeneralY\) /. y0 -> \((\(-1\) - *)
(*\*SuperscriptBox[\(u0[#]\), \(2\)] + *)
(*\*SubscriptBox[\(\[ConstantC]\), \(0\)] u0[#] &)\))\)\)//Simplify*)
(*\[Xi]y/.flowGeneralY/.y0->(-1-u0[#]^2+Subscript[\[ConstantC], 0]u0[#]&)//Simplify*)
(**)
(*%%/%//Simplify*)
(*Union[%]*)


(* ::Item:: *)
(*Intersections with the asymptotic boundary & the HRT surface:*)


(* ::Input:: *)
(*Solve[-1-u^2+x u==0,u]*)


(* ::Input:: *)
(*Solve[-1-u^2+x u==1-u^2,u]*)


(* ::Item:: *)
(*The family of possible initial value curves,*)


(* ::Item:: *)
(*... also the Y lines in the Rindler patch:*)


(* ::Subsection::Closed:: *)
(*Visuals*)


(* ::Input:: *)
(*initialCurves=Module[{cList={0,0.5,1,1.5,2,3,4,5},uRange={-1.5,5}},*)
(*Show[*)
(*Plot[*)
(*1-u^2,*)
(*{u,Splice[uRange]},*)
(*Filling->Axis,*)
(*PlotRange->{-16,3},*)
(*PlotStyle->Directive[Thickness[.005],Dotted],*)
(*AspectRatio->1.2*)
(*],*)
(*Plot[*)
(*(-1-u^2+# u)&/@cList//Evaluate,*)
(*{u,Splice[uRange]},*)
(*PlotLegends->(HoldForm[\[Alpha]==#]&/@cList//Evaluate)*)
(*],*)
(*ListPlot[Join@@{*)
(*{2/#,1-(2/#)^2}&/@DeleteCases[cList,_?PossibleZeroQ]//DeleteCases[_?(#[[2]]>0&)],*)
(*{1/2 (#+Sqrt[#^2-4]),0}&/@cList*)
(*}//Evaluate,*)
(*PlotStyle->PointSize[.016]*)
(*]*)
(*]*)
(*]*)


(* ::Input:: *)
(*exportPlot[initialCurves,"../img/"]*)


(* ::Input:: *)
(*z2y*)


(* ::Input::Initialization:: *)
initialCurvesPoincare=Module[{cList={0,0.5,1,1.5,2,2.3,3,4,5},uRange={-1.5,5}},
Show[
Plot[
z/.z2y/.y->1-u^2//Evaluate,
{u,Splice[uRange]},
Filling->Axis,
PlotRange->{-5,2.5},
PlotStyle->Directive[Thickness[.005],Dotted],
AspectRatio->Automatic
],
Plot[
(z/.z2y/.y->-1-u^2+# u)&/@cList//Evaluate,
{u,Splice[uRange]},
PlotLegends->(HoldForm[\[Alpha]==#]&/@cList//Evaluate)
],
ListPlot[Join@@{
{2/#,z/.z2y/.y->1-(2/#)^2}&/@DeleteCases[cList,_?PossibleZeroQ]//DeleteCases[_?(#[[2]]>0&)],
{1/2 (#+Sqrt[#^2-4]),0}&/@cList
}//Evaluate,
PlotStyle->PointSize[.016]
]
]
]


(* ::Input:: *)
(*exportPlot[initialCurvesPoincare,"../img/"]*)


(* ::Subsection::Closed:: *)
(*Cutoff: some checks*)


(* ::Item:: *)
(*Let's first check this for the known result: the original Rindler map:*)


(* ::Input:: *)
(*\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(Y\)]\((yCoord /. flowCutoff)\)\)//Simplify*)
(*\[Xi]y/.flowCutoff//Simplify*)
(**)
(*%%/%//Simplify*)


(* ::Input:: *)
(*\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(Y\)]\((yCoord /. flowCutoff)\)\)//Simplify;*)
(*% . lengthToMetric[yLength,yCoord] . %/.flowCutoff//Simplify*)
(*\[Xi]y . lengthToMetric[yLength,yCoord] . \[Xi]y/.flowCutoff//Simplify*)


(* ::Item:: *)
(*This is exactly the same for the complement map, as it comes from the analytic continuation of U,V. *)


(* ::Item:: *)
(*A more general result is given by:*)


(* ::Input:: *)
(*flowCutoff/.Y->y0[Y]*)
(**)
(*\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(Y\)]\((yCoord /. %)\)\)//Simplify*)
(*\[Xi]y/.%%//Simplify*)
(**)
(*%%/%//Simplify*)
(*Union[%]//Simplify*)


(* ::Item:: *)
(*For the original & the complement map, we have:*)


(* ::Input:: *)
(*-(Derivative[1][y0][Y]/(-1+y0[Y]^2))/.{{y0->(#&)},{y0->(1/#&)}}//Simplify*)


(* ::Subsection::Closed:: *)
(*Glue-on: some options*)


(* ::Item:: *)
(*Now let's look at the glue-on part. Recall the general solutions:*)


(* ::Input:: *)
(*initialCurvesPoincare*)


(* ::Item:: *)
(*Unfortunately, the coolest option Subscript[\[ConstantC], 0]=2 leads to a degenerate metric:*)


(* ::Input:: *)
(*flowGeneralY/.y0->(-(1-u0[#])^2&)//Simplify*)
(*yLength/.%/.constParams//FullSimplify//Expand*)


(* ::Item:: *)
(*This is understandable. The initial curve Subscript[y, 0]=-(1-Subscript[u, 0])^2 is actually null, and it falls inside a U,V integral surface.*)


(* ::Item:: *)
(*Namely, we have the situation that the Y line degenerates onto a constant Y surface. *)


(* ::Item:: *)
(*The symmetric choice Subscript[\[ConstantC], 0]=0 does not reach the boundary:*)


(* ::Input:: *)
(*flowGeneralY/.y0->(-1-u0[#]^2&)/.u0->(1/#&)//Simplify*)
(*yLength/.%/.constParams//FullSimplify*)
(*lengthToMetric[%,rindlerCoord]//Det//Simplify*)
(**)
(*%%%;*)
(*yCoord/.%;*)
(*Series[%,Y->\[Infinity]]//Normal//TrigExpand//Simplify*)
(*Series[%%,Y->0]//Normal//TrigExpand//Simplify*)


(* ::Subsubsection::Closed:: *)
(*A patch that covers the boundary but not the HRT surface [useless]*)


(* ::Item:: *)
(*Possible solutions that reach the boundary are those with Subscript[\[ConstantC], 0]>2, e.g. Subscript[\[ConstantC], 0]=x+1/x:*)


(* ::Input:: *)
(*-1-u0^2+(x+1/x)u0/.u0->x//Simplify*)


(* ::Item:: *)
(*The intersection is at Subscript[u, 0]=x. *)


(* ::Item:: *)
(*It does have the right asymptotics along the boundary:*)


(* ::Input:: *)
(*flowGeneralY/.y0->(-1-u0[#]^2+(x+1/x)u0[#]&)/.u0->(x+#&)/.x->Coth[x0]//Simplify*)
(*Series[yCoord/.%,Y->0]//Normal//TrigExpand//FullSimplify*)


(* ::Item:: *)
(*However, the horizon is no longer at Y=1, and everything is weird:*)


(* ::Input:: *)
(*flowGeneralY/.y0->(-1-u0[#]^2+(x+1/x)u0[#]&)/.u0->(x+#&)/.x->2//Simplify*)
(*yLength/.%/.constParams//Simplify//Expand*)
(*lengthToMetric[%,rindlerCoord]//Det//Factor*)


(* ::Item:: *)
(*Some constant Y curves in these coordinates:*)


(* ::Input:: *)
(*flowGeneralY/.y0->(-1-u0[#]^2+(x+1/x)u0[#]&)/.u0->(x+#&)/.x->2//Simplify*)
(*(Thread[{u,y}->(Normal[Series[{u,y}/.%,#]]/.{U->X/2,V->X/2}/.{lu->2,lv->2}//Simplify)])&/@{*)
(*Y->-3,Y->-2,Y->-1,Y->0,Y->1,Y->2,Y->3*)
(*}*)


(* ::Input:: *)
(*flowGeneralY/.y0->(-1-u0[#]^2+(x+1/x)u0[#]&)/.u0->(x+#&)/.x->2//Simplify*)
(*(Thread[{u,y}->(Normal[Series[{u,y}/.%,#]]/.{U->X/2,V->X/2}/.{lu->2,lv->2}//Simplify)])&/@{*)
(*Y->-3,Y->-2,Y->-1,Y->0,Y->1,Y->2,Y->3*)
(*};*)
(**)
(*ParametricPlot[*)
(*({u,z}/.z2y/.#)&/@%//Evaluate,{X,-5,5},PlotRange->{{-3,3},{-5,3}}*)
(*]*)


(* ::Item:: *)
(*This doesn't look useful. In fact it seems that it is precisely the complement map.*)


(* ::Subsubsection::Closed:: *)
(*A patch that follows Subscript[\[Xi], (Y)] proportionally [illuminating, but otherwise useless]*)


(* ::Item:: *)
(*Another possibility: say that the proportionality factor is the same as the one for the original interval:*)


(* ::Input:: *)
(*Derivative[1][u0][Y]/(u0[Y] (2-Subscript[\[ConstantC], 0] u0[Y]))==1/(1-Y^2);*)


(* ::Input:: *)
(*DSolve[(u0[Y] (2-Subscript[\[ConstantC], 0] u0[Y]))/(1-Y^2)==Derivative[1][u0][Y],u0[Y],Y]//Simplify*)


(* ::Item:: *)
(*A natural choice would be Subscript[\[ConstantC], 0]=0,*)


(* ::Item:: *)
(*... while a convenient choice would be E^(2 C[1])=2.*)


(* ::Item:: *)
(*We thus have:*)


(* ::Input:: *)
(*flowGeneralY/.y0->(-1-u0[#]^2&)/.u0->((1+#)/(1-#)&)//Simplify*)
(*yLength/.%/.constParams//FullSimplify//Expand*)
(*lengthToMetric[%,rindlerCoord]//Det//Simplify*)
(**)
(*%%%;*)
(*yCoord/.%;*)
(*Series[%,Y->1]//Normal//TrigExpand//Simplify*)
(*Series[%%,Y->0]//Normal//TrigExpand//Simplify*)
(*Series[%%%,Y->\[Infinity]]//Normal//TrigExpand//Simplify*)


(* ::Item:: *)
(*This is a nice result, but still it cannot reach the asymptotic boundary. *)


(* ::Item:: *)
(*Can this be cured by some coordinate map?*)


(* ::Input:: *)
(*\[Integral]\[DifferentialD]Y/(1+Y^2)*)


(* ::Input:: *)
(*\[Integral]\[DifferentialD]Y/Sqrt[Y]*)


(* ::Input:: *)
(*Dt[U]^2+(4 Y Dt[U] Dt[V])/(1+Y^2)+Dt[V]^2-Dt[Y]^2/(1+Y^2)^2/.{*)
(*{Y->Tan[Y]},*)
(*{Y->Tan[1/2 Log[Y]]},*)
(*{Y->Tan[1/2 ArcSin[Y]]}*)
(*}//FullSimplify//Column*)


(* ::Input:: *)
(*Tan[x/2]==Sin[x]/(1+Cos[x])==(1-Cos[x])/Sin[x]//Simplify*)
(*Sin[x]/(1+Cos[x])/.x->ArcSin[Y]//Simplify*)
(*(1-Cos[x])/Sin[x]/.x->ArcSin[Y]//Simplify*)
(*%/%%//Simplify*)


(* ::Input:: *)
(*(1+Y)/(1-Y)/.Y->(1-Sqrt[1-Y^2])/Y//FullSimplify*)
(*(1+Y)/(1-Y)/.Y->Tan[Y]//FullSimplify*)


(* ::Item:: *)
(*We've hence found some nice reparametrizations of Y. See the following discussions.*)


(* ::Subsubsection::Closed:: *)
(*Maximal extensions [nice, but feels weird]*)


(* ::Input:: *)
(*flowGeneralY/.y0->(-1-u0[#]^2&)/.u0->(1/#&)//Simplify*)
(*yLength/.%/.constParams//FullSimplify*)
(*lengthToMetric[%,rindlerCoord]//Det//Simplify*)
(**)
(*%%%;*)
(*yCoord/.%;*)
(*Series[%,Y->\[Infinity]]//Normal//TrigExpand//Simplify*)
(*Series[%%,Y->0]//Normal//TrigExpand//Simplify*)


(* ::Input:: *)
(*flowGeneralY/.y0->(-1-u0[#]^2&)/.u0->(Cot[#]&)//Simplify*)
(*yLength/.%/.constParams//FullSimplify*)
(*lengthToMetric[%,rindlerCoord]//Det//Simplify*)
(**)
(*%%%;*)
(*yCoord/.%;*)
(*Series[%,Y->\[Pi]/2]//Normal//TrigExpand//Simplify*)
(*Series[%%,Y->0]//Normal//TrigExpand//Simplify*)


(* ::Item:: *)
(*Some constant Y curves in these coordinates:*)


(* ::Input:: *)
(*flowGeneralY/.y0->(-1-u0[#]^2&)/.u0->(Cot[#]&)//Simplify;*)
(*Module[{Yvalues={*)
(*Y->-\[Pi]/2+1/4,*)
(*Y->-\[Pi]/4,*)
(*Y->0,*)
(*Y->+\[Pi]/4,*)
(*Y->\[Pi]/2-1/4*)
(*},Ycurves},*)
(**)
(*Ycurves=Thread[{u,y}->(Normal[Series[{u,y}/.%,#]]/.{U->X/2,V->X/2}/.{lu->2,lv->2}//Simplify)]&/@Yvalues;*)
(**)
(*Show[*)
(*ParametricPlot[*)
(*(({u,z}/.z2y/.#)&/@Ycurves)//Evaluate,*)
(*{X,-5,5},*)
(*PlotRange->{{-2,2},{-3,1}},*)
(*PlotLegends->(Yvalues/.Y->OverTilde[\[Rho]]//Map[HoldForm]//Evaluate)*)
(*],*)
(*Plot[{*)
(*z/.z2y/.y->-1-X^2,*)
(*-1-X,-1+X*)
(*},{X,-5,5},PlotStyle->Dotted]*)
(*]*)
(*]*)
(**)
(*Limit[y+u v-(lu lv)/4/.%%,Y->0]//Simplify*)


(* ::Input:: *)
(*flowGeneralY/.y0->(-1-u0[#]^2&)/.u0->(1/#&)//Simplify;*)
(*Module[{Yvalues={*)
(*Y->-4,Y->-2,Y->-1,*)
(*Y->0,*)
(*Y->1,Y->2,Y->4*)
(*},Ycurves},*)
(**)
(*Ycurves=Thread[{u,y}->(Normal[Series[{u,y}/.%,#]]/.{U->X/2,V->X/2}/.{lu->2,lv->2}//Simplify)]&/@Yvalues;*)
(**)
(*Show[*)
(*ParametricPlot[*)
(*(({u,z}/.z2y/.#)&/@Ycurves)//Evaluate,*)
(*{X,-5,5},*)
(*PlotRange->{{-2,2},{-3,1}},*)
(*PlotLegends->(Yvalues/.Y->OverTilde[\[Rho]]//Map[HoldForm]//Evaluate)*)
(*],*)
(*Plot[{*)
(*z/.z2y/.y->-1-X^2,*)
(*-1-X,-1+X*)
(*},{X,-5,5},PlotStyle->Dotted]*)
(*]*)
(*]*)
(**)
(*y+u v-(lu lv)/4/.%%/.Y->0//Simplify*)


(* ::Section::Closed:: *)
(*AdS-Schwarzschild*)


(* ::Input::Initialization:: *)
karlLength=Dt[Y]^2/(4 (Y^2-1))+Dt[U]^2+2 Y Dt[U] Dt[V]+Dt[V]^2;


(* ::Input:: *)
(*stitchedInitialCurve*)


(* ::Input:: *)
(*flowStitched*)


(* ::Subsection::Closed:: *)
(*Glue-on: near HRT*)


(* ::Item:: *)
(*We start with a simple yet maximally extended parametrization of Subscript[u, 0]=Y:*)


(* ::Input:: *)
(*flowGeneralY/.y0->(-1-u0[#]^2&)/.u0->(#&)//Simplify*)
(*yLength/.%/.constParams//FullSimplify*)


(* ::Item:: *)
(*... and then set the coefficient of \[DifferentialD]U \[DifferentialD]V to 2Y:*)


(* ::Input:: *)
(*Solve[((Y^2-1)/(Y^2+1)/.Y^2->Y2)==Y,Y2]//Simplify*)


(* ::Input:: *)
(*flowGeneralY/.y0->(-1-u0[#]^2&)/.u0->(Sqrt[(1+#)/(1-#)]&)//FullSimplify*)
(*yLength/.%/.constParams//Simplify*)
(*%==karlLength*)
(*lengthToMetric[%%,rindlerCoord]//Det//Simplify*)
(**)
(*%%%%;*)
(*yCoord/.%;*)
(*Series[%,Y->1]//Normal//TrigExpand//Simplify*)
(*Series[%%,Y->-1]//Normal//TrigExpand//Simplify*)


(* ::Item:: *)
(*The metric is precisely AdS Schwarzschild!*)


(* ::Item:: *)
(*The other sign:*)


(* ::Input:: *)
(*flowGeneralY/.y0->(-1-u0[#]^2&)/.u0->(-Sqrt[((1+#)/(1-#))]&)//FullSimplify*)
(*yLength/.%/.constParams//Simplify*)
(*%==karlLength*)
(**)
(*%%%;*)
(*yCoord/.%;*)
(*Series[%,Y->1]//Normal//TrigExpand//Simplify*)
(*Series[%%,Y->-1]//Normal//TrigExpand//Simplify*)


(* ::Item:: *)
(*Visualizations:*)


(* ::Input:: *)
(*flowGeneralY/.y0->(-1-u0[#]^2&)/.u0->(-Sqrt[((1+#)/(1-#))]&)//Simplify;*)
(*Module[{Yvalues={*)
(*Y->-.95,Y->-.5,*)
(*Y->0,*)
(*Y->.5,Y->.9,Y->.9999*)
(*},Ycurves},*)
(**)
(*Ycurves=Thread[{u,y}->(Normal[Series[{u,y}/.%,#]]/.{U->X/2,V->X/2}/.{lu->2,lv->2}//Simplify)]&/@Yvalues;*)
(**)
(*Show[*)
(*ParametricPlot[*)
(*(({u,z}/.z2y/.#)&/@Ycurves)//Evaluate,*)
(*{X,-5,5},*)
(*PlotRange->{{-2,2},{-3,1}},*)
(*PlotLegends->(Yvalues/.Y->OverTilde[\[Rho]]//Map[HoldForm]//Evaluate)*)
(*],*)
(*Plot[{*)
(*z/.z2y/.y->-1-X^2,*)
(*-1-X,-1+X*)
(*},{X,-5,5},PlotStyle->Dotted]*)
(*]*)
(*]*)
(**)
(*Limit[y+u v-(lu lv)/4/.%%,Y->1]*)


(* ::Subsection::Closed:: *)
(*Glue-on & cutoff: near infinity*)


(* ::Item:: *)
(*One then hope to stitch this with the usual Rindler map map on the right-hand side.*)


(* ::Input:: *)
(*flowGeneralY/.y0->(#&)/.u0->(0&)//Simplify*)
(*%==flowCutoff//FullSimplify*)
(**)
(*yLength/.%%/.constParams//Simplify*)


(* ::Item:: *)
(*We again try to obtain AdS-Schwarzschild:*)


(* ::Input:: *)
(*Solve[((1+Y^2)/Y/.Y->Yx)==2Y,Yx]*)
(*Plot[{*)
(*Yx/.%,*)
(*2Y*)
(*}//Evaluate,{Y,-3,3},*)
(*AspectRatio->Automatic,PlotRange->{-6,6},*)
(*PlotStyle->{Automatic,Automatic,Dashed},*)
(*PlotLegends->"Expressions"]*)


(* ::Item:: *)
(*Y<-1 and Subscript[Y, x]<-1, so we take the Y-Sqrt[Y^2-1] solution.*)


(* ::Item:: *)
(*Alternatively, we can start from the complement map Subscript[y, 0]=1/Y.*)


(* ::Item:: *)
(*In this case Y<-1 and -1<Subscript[Y, x]<0, then we should take the Y+Sqrt[Y^2-1] solution.*)


(* ::Input:: *)
(*(Y-Sqrt[Y^2-1])/((Y+Sqrt[Y^2-1])(Y-Sqrt[Y^2-1])//Simplify)*)


(* ::Item:: *)
(*The end result is the same, as it should be. *)


(* ::Input:: *)
(*flowGeneralY/.y0->(#-Sqrt[#^2-1]&)/.u0->(0&)//FullSimplify*)
(*yLength/.%/.constParams//Simplify*)
(*%==karlLength*)


(* ::Item:: *)
(*Nice! Indeed we have AdS-Schwarzschild!*)


(* ::Input:: *)
(*flowGeneralY/.y0->(#-Sqrt[#^2-1]&)/.u0->(0&)//Simplify;*)
(*Module[{Yvalues={*)
(*Y->-1.1,Y->-3,Y->-5,Y->-7,Y->-10,Y->-100,Y->-1000,Y->-10000*)
(*},Ycurves},*)
(**)
(*Ycurves=Thread[{u,y}->(Normal[Series[{u,y}/.%,#]]/.{U->X/2,V->X/2}/.{lu->2,lv->2}//Simplify)]&/@Yvalues;*)
(**)
(*Show[*)
(*ParametricPlot[*)
(*(({u,z}/.z2y/.#)&/@Ycurves)//Evaluate,*)
(*{X,-5,5},*)
(*PlotRange->{{-6,6},{-4,1}},*)
(*PlotLegends->(Yvalues/.Y->OverTilde[\[Rho]]//Map[HoldForm]//Evaluate)*)
(*],*)
(*Plot[{*)
(*-1-X,-1+X*)
(*},{X,-5,5},PlotStyle->Dotted]*)
(*]*)
(*]*)
(**)
(*Limit[y+u v-(lu lv)/4/.%%,Y->1]*)


(* ::Subsection::Closed:: *)
(*Glue-on: stitching*)


(* ::Item:: *)
(*In summary, we have:*)


(* ::Input:: *)
(*flowGeneralY/.y0->(#-Sqrt[#^2-1]&)/.u0->(0&)//FullSimplify*)
(*Series[coord/.%,Y->-\[Infinity]]*)
(*Series[coord/.%%,Y->-1]//FullSimplify*)
(*flowGeneralY/.y0->(-1-u0[#]^2&)/.u0->(-Sqrt[((1+#)/(1-#))]&)//FullSimplify*)
(*Series[coord/.%,Y->1]*)
(*Series[coord/.%%,Y->-1]*)


(* ::Item:: *)
(*The asymptotics are perfect!*)


(* ::Input:: *)
(*-1-(1+Y)/(1-Y)//Simplify*)


(* ::Input::Initialization:: *)
\[Piecewise]{
 {{0,Y-Sqrt[Y^2-1]}, Y<-1},
 {{-Sqrt[((1+Y)/(1-Y))],-(2/(1-Y))}, -1<Y<1}
}/.Piecewise->List//First

{%[[All,1,#]],%[[All,2]]}&/@{1,2}//Map[Transpose]
%//Map[Piecewise[#,Indeterminate]&]

stitchedInitialCurve=Function[Y,Evaluate[%]];


(* ::Item:: *)
(*The map is continuous at Y=-1.*)


(* ::Input:: *)
(*Limit[stitchedInitialCurve[Y],Y->-1,Direction->#]&/@{"FromAbove","FromBelow"}*)


(* ::Item:: *)
(*... but clearly not smooth. As a parametrized curve, the first derivative already diverges:*)


(* ::Input:: *)
(*stitchedInitialCurve'[Y]//FullSimplify*)
(*Limit[stitchedInitialCurve'[Y],Y->-1,Direction->#]&/@{"FromAbove","FromBelow"}*)


(* ::Input:: *)
(*ParametricPlot[*)
(*{u,z}/.z2y/.Thread[*)
(*{u,y}->stitchedInitialCurve[Y]*)
(*]//Simplify,*)
(*{Y,-4.5,.9},*)
(*PlotRange->{Automatic,{-2.5,Automatic}},*)
(*AxesOrigin->{0,0},*)
(*AspectRatio->Automatic,*)
(*Mesh->80,MeshStyle->Directive[Purple,PointSize[.015]]*)
(*]*)


(* ::Input::Initialization:: *)
flowStitched=flowGeneralY/.Thread[{u0[Y],y0[Y]}->stitchedInitialCurve[Y]]//Simplify


(* ::Input:: *)
(*Series[yCoord/.flowStitched/.sameL/.l->2/.{U->X,V->X},{X,0,0}]//Normal//Simplify*)
(**)
(*Thread[yCoord->%];*)
(*(u==v)/.%*)
(*({u,y}/.%%)==stitchedInitialCurve[Y]//Thread//FullSimplify*)


(* ::Subsection::Closed:: *)
(*Near horizon: local Rindler coordinates*)


(* ::Input:: *)
(*karlLength*)


(* ::Item:: *)
(*We try to "renormalize" \[DifferentialD]Y^2/(4(Y^2-1))->\[PlusMinus]\[DifferentialD]R^2.*)


(* ::Item:: *)
(*Note the \[PlusMinus] sign. *)


(* ::Input:: *)
(*karlLength/.Thread[Dt/@{U,V,Y}->{0,0,1}]*)


(* ::Input:: *)
(*Rx[Y]/.Assuming[Y>1,*)
(*DSolve[Rx'[Y]^2==1/(4Abs[Y^2-1])&&Rx[1]==0//Evaluate,Rx[Y],Y]//ExpToTrig//FullSimplify[#,Y>1]&*)
(*]*)


(* ::Input:: *)
(*Rx[Y]/.Assuming[#[[1]],*)
(*DSolve[Rx'[Y]^2==1/(4Abs[Y^2-1])&&Rx[#[[2]]]==0//Evaluate,Rx[Y],Y]//FullSimplify*)
(*]&/@{*)
(*{Y>1,1},*)
(*{-1<Y<1,1},*)
(*{-1<Y<1,-1},*)
(*{Y<-1,-1}*)
(*}*)
(**)
(*%//Map[Function[Y,#]&,#,{2}]&//Map[InverseFunction,#,{2}]&//Map[#[R]&,#,{2}]&//FullSimplify[#,R\[Element]Reals]&*)


(* ::Item:: *)
(*Nice! Check results:*)


(* ::Input:: *)
(*(karlLength/.Y->#)&/@{Cosh[2R],-Cosh[2R],Cos[2R],-Cos[2R]}//Column*)
(*%/.UV2XT//Simplify*)
(**)
(*lengthToMetric[#,{R,T,X}]&/@Normal[%]//Column*)
(*Series[Normal[%],{R,0,3}]//Simplify//Column*)
(**)
(*Outer[#1 . #2 . #1&,{Dt/@{R,T,X}},Normal[%%],1]//Flatten//Column*)
(*Outer[#1 . #2 . #1&,{Dt/@{R,T,X}},Normal[%%],1]//Flatten//Column*)
(*%%==%%%%%*)


(* ::Item:: *)
(*In particular, the norm of \!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\[Tau]\)]\(\(=\)\(\[Xi]/\((2  \[Pi])\)\)\)\) is given by:*)


(* ::Input:: *)
(*karlLength/.UV2XT;*)
(*lengthToMetric[%,{T,X,Y}][[1,1]]*)
(**)
(*(%/.Y->#)&/@{Cosh[2R],-Cosh[2R],Cos[2R],-Cos[2R]}//Simplify*)


(* ::Input:: *)
(*{Cos[2R],Sin[R],Cos[R]}/.R->I R*)


(* ::Subsection::Closed:: *)
(*Details on the flow of coordinates*)


(* ::Input:: *)
(*flowStitched/.sameL/.l->2/.UV2XT/.T->0//Simplify*)
(*{u,v}/.%//Simplify//Union*)
(**)
(*{%,z}/.z2y/.%%;*)
(*Show[*)
(*ParametricPlot[*)
(*(%/.X->#)&/@{.6,.7,.8}//Evaluate,*)
(*{Y,-10^3,1},*)
(*MaxRecursion->10,AxesOrigin->{0,0,0},*)
(*ImageSize->400,AspectRatio->Automatic,PlotRange->{{-5,5},{-5,0}},PlotStyle->Thickness[.003]*)
(*],*)
(*Plot[{*)
(*1-Abs[x],-1-Abs[x],-Sqrt[x^2-1]*)
(*},{x,-6,6},*)
(*PlotStyle->Evaluate[Directive[Pink,#]&/@{Dotted,Dotted,Thickness[.002]}]*)
(*]*)
(*]*)


(* ::Item:: *)
(*The flow of coordinates is kind of weird: it tends to \[Infinity] and then circles back!*)


(* ::Input:: *)
(*flowStitched/.sameL/.l->2/.UV2XT/.T->0//Simplify*)
(*{u,v}/.%//Simplify//Union*)
(**)
(*{u,z};*)
(*%/.z2y/.%%%;*)
(*Plot[*)
(*%/.X->2//Simplify//Evaluate,*)
(*{Y,-2,1},*)
(*PlotRange->{-3,3},MaxRecursion->10,*)
(*ExclusionsStyle->Dashed,PlotLegends->%%*)
(*]*)


(* ::Item:: *)
(*It is somewhat singular at Y=-1.*)


(* ::Input:: *)
(*yCoord/.flowStitched//Simplify*)
(**)
(*Series[%,Y->-1]//Simplify;*)
(*%/.UV2XT//Simplify*)


(* ::Subsubsection::Closed:: *)
(*Regularize the Y=-1 singularity*)


(* ::Item:: *)
(*Any constant T slice will flow to a singular point (of dim 0) at Y=-1.*)


(* ::Input:: *)
(*flowStitched/.sameL/.l->2/.UV2XT//Simplify*)
(*ParametricPlot3D[*)
(*{x,z,t}/.xt2uv/.z2y/.%/.T->.3//Evaluate,{X,.2,10},{Y,-10,1},*)
(*PlotRange->{{-3,3},{-5,0},{-3,3}},MaxRecursion->5,*)
(*AxesLabel->{x,z,t},Mesh->None]*)


(* ::Item:: *)
(*In order to resolve the Y=-1 point one has to perturb the slice such that it is T dependent.*)


(* ::Item:: *)
(*For example, take T=k X, where k is small:*)


(* ::Input:: *)
(*flowStitched/.sameL/.l->2/.UV2XT//Simplify;*)
(*%/.T->k X//Simplify*)
(**)
(*ParametricPlot3D[*)
(*{x,z,t}/.xt2uv/.z2y/.%/.k->.3//Evaluate,{X,.2,10},{Y,-10^1,1},*)
(*PlotRange->{{-3,3},{-5,0},{-3,3}},MaxRecursion->5,*)
(*AxesLabel->{x,z,t},Mesh->None]*)


(* ::Input:: *)
(*Series[yCoord/.flowStitched//Simplify,Y->-1]/.UV2XT//FullSimplify[#,{Y!=-1}]&*)
(*Thread[yCoord->Normal[%]/.sameL/.l->2]*)
(**)
(*{x,z,t};*)
(*%/.xt2uv/.z2y/.%%;*)
(*ParametricPlot3D[%,{T,-3,3},AxesLabel->%%]*)


(* ::Section::Closed:: *)
(*Orthogonal bases along the HRT*)


(* ::Input:: *)
(*yLength*)


(* ::Input:: *)
(*norm[lower[\[Xi]]]//FullSimplify//Factor//FullSimplify*)


(* ::Item:: *)
(*\[Del]\[Xi] is antisymmetrized automatically:*)


(* ::Input:: *)
(*covD[lower[\[Xi]]]//Simplify*)
(*antisymmetricQ[%]*)


(* ::Item:: *)
(*The reason is that \[Xi] is Killing, so the symmetric part vanishes. *)


(* ::Item::Initialization:: *)
(*Define r=-(-1)^s n/(2\[Pi]) \[Xi]\[CenterDot]\[Del]\[Xi] where n->1 is the replica number, and s is the signature:*)


(* ::Input::Initialization:: *)
\[Xi]d\[Xi]=raise[contract[1/(2\[Pi]) lower[\[Xi]]**covD[lower[\[Xi]]],{1,2}]]//FullSimplify


(* ::Subsection::Closed:: *)
(*Visualizations*)


(* ::Input:: *)
(*Module[{s=1.6,pts},*)
(*pts=Subdivide[-s,s,Round[10s]];*)
(*Show[*)
(*StreamPlot[*)
(*{ \[Xi][[{v,u}]],\[Xi]d\[Xi][[{v,u}]] }/.sameL/.uvExchange/.{y->0,l->2}//Evaluate,*)
(*{u,-s,s},{v,-s,s},*)
(*StreamPoints->Evaluate[( *)
(*Function[specialpt,*)
(*Splice[*)
(*Splice[{{#,specialpt},{specialpt,#}}]&/@pts*)
(*]*)
(*]/@{s,-s}*)
(*)~Join~( *)
(*Splice[{{#,#},{#,-#}}]&/@pts*)
(*)],*)
(*StreamStyle->(Directive[#,Opacity[.8]]&/@colorsDefault[[;;2]]//Evaluate),*)
(*(*StreamScale\[Rule]None,*)*)
(*(*PlotRangePadding\[Rule]None,*)*)
(*FrameLabel->{v,u},PlotLegends->{HoldForm[\[Xi]],HoldForm[r]}*)
(*],*)
(*StreamPlot[*)
(*\[Xi]d\[Xi][[{v,u}]]/.sameL/.uvExchange/.{y->0,l->2}//Evaluate,*)
(*{u,-s,s},{v,-s,s},*)
(*StreamPoints->Evaluate[*)
(*Function[specialpt,*)
(*Splice[*)
(*Splice[{{#,specialpt},{specialpt,#}}]&/@pts[[2;;-2]]*)
(*]*)
(*]/@{0}*)
(*],*)
(*StreamStyle->Directive[colorsDefault[[2]],Opacity[.8]]*)
(**)
(*(*VectorPoints\[Rule]{"Mesh",12},VectorMarkers\[Rule]Placed["Drop","Start"],*)
(*VectorScale\[Rule]{.07,Automatic,Function[{x,y,vx,vy,s},Log[1+s]]},*)
(*VectorColorFunction\[Rule]None,VectorStyle\[Rule]colorsDefault\[LeftDoubleBracket];;2\[RightDoubleBracket],*)*)
(*],ImageSize->Medium*)
(*]*)
(*]*)


(* ::Subsection::Closed:: *)
(*Norms and binormals*)


(* ::Item:: *)
(*Look at the norms:*)


(* ::Input:: *)
(*norm[lower[\[Xi]]]//Factor//FullSimplify*)
(*norm[lower[\[Xi]d\[Xi]]]//Factor//FullSimplify*)
(**)
(*norm[lower[\[Xi]d\[Xi]]]/norm[lower[\[Xi]]]//FullSimplify*)
(*norm[covD[lower[\[Xi]]]]//FullSimplify*)
(*%%/%*)
(**)
(*Out/@(-Range[2,5])/.sameL/.l->2/.uv2xt/.t->0//FullSimplify*)
(*Series[%/.y->1-x^2-\[Epsilon],{\[Epsilon],0,#}]&/@{1,2}//Simplify//Apply[Equal]*)


(* ::Item:: *)
(*We observe that r^2/\[Xi]^2=1/(8\[Pi]^2) (\[Del]\[Xi])^2\[TildeEqual]-1. *)


(* ::Item:: *)
(*We define the normalized r and \[Xi]:*)


(* ::Input::Initialization:: *)
{\[Xi]Hat,rHat}=2/\[Pi] {
(Sqrt[lu lv] Sqrt[Abs[y]])/Sqrt[Abs[((lu+2 u) (lv-2 v)-4 y) ((lu-2 u) (lv+2 v)-4 y)]] \[Xi],
((4lu lv Abs[y])/(\[Sqrt]Abs[((lu+2 u) (lv-2 v)-4 y) ((lu-2 u) (lv+2 v)-4 y) ((lu-2 u) (lv-2 v)+4 y) ((lu+2 u) (lv+2 v)+4 y)]))\[Xi]d\[Xi]
};


(* ::Item:: *)
(*Check:*)


(* ::Input:: *)
(*{\[Xi]Hat,rHat}/((#/Sqrt[Abs[norm[lower[#]]//Factor//FullSimplify]])&/@{\[Xi],\[Xi]d\[Xi]})//FullSimplify*)


(* ::Subsection::Closed:: *)
(*Replica variation*)


(* ::Item:: *)
(*What does Subscript[\[Delta], n]g\[Tilde]r\[TensorProduct]r/r^2 look like?*)


(* ::Input:: *)
(*lower[\[Xi]d\[Xi]**\[Xi]d\[Xi]/(norm[lower[\[Xi]d\[Xi]]]//Factor//FullSimplify)]//FullSimplify;*)
(*%//MatrixForm*)
(**)
(*%/.sameL/.l->2/.uv2xt;*)
(*%/.t->0//Simplify*)
(*%/.y->1-x^2//FullSimplify*)


(* ::Item:: *)
(*It is finite along the HRT. However, \[Del]Subscript[\[Delta], n]g diverges.*)


(* ::Input:: *)
(*covD[lower[\[Xi]d\[Xi]**\[Xi]d\[Xi]/(norm[lower[\[Xi]d\[Xi]]]//Factor//FullSimplify)]];*)
(**)
(*%/.sameL/.l->2/.uv2xt/.t->0//Simplify;*)
(*%/.y->1-x^2-\[Epsilon]//Simplify;*)
(**)
(*Series[%,{\[Epsilon],0,-1}]//Simplify*)
(*Normal[%]*)


(* ::Item:: *)
(*The divergence comes from \[PartialD](1/r^2).*)


(* ::Item:: *)
(*The Christoffel symbols do not contribute to the divergence.*)


(* ::Input:: *)
(*pD[lower[\[Xi]d\[Xi]**\[Xi]d\[Xi]/(norm[lower[\[Xi]d\[Xi]]]//Factor//FullSimplify)]];*)
(**)
(*%/.sameL/.l->2/.uv2xt/.t->0//Simplify;*)
(*%/.y->1-x^2-\[Epsilon]//Simplify;*)
(**)
(*Series[%,{\[Epsilon],0,-1}]//Simplify*)
(*Normal[%]*)


(* ::Item:: *)
(*Let's look at one component:*)


(* ::Input:: *)
(*lower[\[Xi]d\[Xi]**\[Xi]d\[Xi]/(norm[lower[\[Xi]d\[Xi]]]//Factor//FullSimplify)];*)
(**)
(*%/.sameL/.l->2/.uv2xt;*)
(*\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(t\)]%\)/.t->0//Simplify//MatrixForm*)
(*%/.y->1-x^2-\[Epsilon];*)
(**)
(*Series[%,{\[Epsilon],0,-1}]//Simplify*)


(* ::Item:: *)
(*More generally, in (t,x,y) coordinates we have:*)


(* ::Input:: *)
(*lower[\[Xi]d\[Xi]**\[Xi]d\[Xi]/(norm[lower[\[Xi]d\[Xi]]]//Factor//FullSimplify)];*)
(**)
(*%/.sameL/.l->2/.uv2xt;*)
(*Outer[\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(#1\)]#2\)&,{t,x,y},%]/.t->0//Simplify;*)
(*%/.y->1-x^2-\[Epsilon];*)
(**)
(*Series[%,{\[Epsilon],0,-1}]//Simplify//Map[MatrixForm]*)


(* ::Subsection::Closed:: *)
(*Limits*)


(* ::Item:: *)
(*1/(2\[Pi]) \[Del]\[Xi]=\[Xi]\[TensorProduct]r/\[Xi]^2 has a well-defined limit at the HRT surface:*)


(* ::Input:: *)
(*1/(2\[Pi]) raise[covD[lower[\[Xi]]]]//Simplify*)
(*(2antisymmetrize[\[Xi]**\[Xi]d\[Xi]])/norm[lower[\[Xi]]]//Simplify*)
(*%==%%*)
(**)
(*%%/.sameL/.l->2/.uv2xt//Simplify*)
(**)
(*%/.t->0*)
(*%/.y->1-x^2//Simplify*)
(**)
(*%%/.t->1/6 Sin[\[Pi] Sqrt[x^2+y]]//Simplify;*)
(*%/.y->1-x^2//Simplify*)


(* ::Item:: *)
(*Different approaching directions are realized by the function:*)


(* ::Input:: *)
(*Plot3D[1/3 Sin[\[Pi] Sqrt[x^2+z^2]],{x,-1,1},{z,0,1},RegionFunction->Function[{x,z},x^2+z^2<=1],*)
(*MaxRecursion->5,MeshStyle->Gray,BoxRatios->Automatic*)
(*]*)


(* ::Section:: *)
(*Entanglement wedge*)


(* ::Input::Initialization:: *)
norm[lower[\[Xi]]]//Factor//FullSimplify
%/.sameL/.l->2//FullSimplify
Solve[%==0,y]//FullSimplify

lightSheets=(y0==y/.%)/.y0->y/.uv2xt//FullSimplify//Reverse


(* ::Subsection::Closed:: *)
(*Bad visuals with low performance*)


(* ::Input:: *)
(*norm[lower[\[Xi]]]//Factor//FullSimplify*)
(*%/.uv2xt*)
(*%/.sameL/.l->2//FullSimplify*)
(**)
(*Solve[%==0,y]//Simplify*)
(*y/.%*)
(*z/.z2y/.%%//Simplify*)
(**)
(*Plot3D[%,{t,-3,3},{x,-3,3},AxesLabel->Automatic,BoxRatios->Automatic,Mesh->None,PlotStyle->Opacity[.3],MaxRecursion->5(*,ScalingFunctions\[Rule]{Function[y,Sqrt[Abs[y]] Sign[y]],Function[z,z^2Sign[z]]}*)*)
(*]*)


(* ::Input:: *)
(*norm[lower[\[Xi]]]//Factor//FullSimplify*)
(*%/.uv2xt*)
(*%/.sameL/.l->2//FullSimplify*)
(**)
(*t/.Solve[%==0,t]//Simplify*)
(**)
(*Plot3D[%/.y->z^2 Sign[z]//Evaluate,{x,-4,4},{z,-6,2},AxesLabel->Automatic,BoxRatios->Automatic,MaxRecursion->5,Mesh->None,PlotStyle->(Directive[Opacity[.5],#]&/@{RGBColor[1, 0.75, 0],RGBColor[0.560181, 0.691569, 0.194885],RGBColor[1, 0.75, 0],RGBColor[0.560181, 0.691569, 0.194885](*,,,*)}//Evaluate),BoundaryStyle->None,ImageSize->360(*,PlotLegends\[Rule]"Expressions"*)]*)


(* ::Subsection::Closed:: *)
(*Nice visuals: 3D light sheets*)


(* ::Input:: *)
(*ContourPlot3D[*)
(*{*)
(*lightSheets,z==0,z==-2*)
(*}/.y->z^2 Sign[z]//Flatten//Evaluate,*)
(*{x,-3,3},{z,-3,1.5},{t,-4,4},*)
(*AxesLabel->Automatic,BoxRatios->Automatic,*)
(*Mesh->None,*)
(*ContourStyle->{*)
(*Directive[Opacity[.7]],*)
(*Directive[Opacity[.7]],*)
(*Directive[Opacity[.3],White],*)
(*Directive[Opacity[.4],White]*)
(*},BoundaryStyle->None*)
(*]*)


(* ::Input:: *)
(*ContourPlot3D[*)
(*{*)
(*#[[1]],z==0*)
(*}/.y->z^2 Sign[z]//Evaluate,*)
(*{x,-3,3},{z,-3,1},{t,-4,4},*)
(*RegionFunction->Function[{x,z,t},#[[2]]],RegionBoundaryStyle->None,*)
(*AxesLabel->Automatic,BoxRatios->Automatic,*)
(*MeshFunctions->{Function[{x,z,t},z]},Mesh->11,(*MeshStyle\[Rule]Directive[Thickness[.0015]],*)*)
(*ContourStyle->{*)
(*Charting`ResolvePlotTheme[Automatic,ContourPlot3D]*)
(*//OptionValue[#,Method]&*)
(*//OptionValue[#,"DefaultPlotStyle"]&*)
(*//Extract[#[[3]]]*)
(*//Append[Opacity[.6]],*)
(*Directive[White,Opacity[.1],Lighting->"ThreePoint"]*)
(*},BoundaryStyle->None*)
(*]&/@{*)
(*{lightSheets[[1]],0<=t<=1||(t>=1&&z<=0),1},*)
(*{lightSheets[[2]],0>=t>=-1||(t<=-1&&z<=0),2}*)
(*}//Show*)


(* ::Item:: *)
(*Front view:*)


(* ::Item:: *)
(*Back view:*)


(* ::Item:: *)
(*Alternatively,*)


(* ::Input:: *)
(*ContourPlot3D[*)
(*{*)
(*#[[1]](*,z==0*)*)
(*}/.y->z^2 Sign[z]//Evaluate,*)
(*{x,-3,3},{z,-3,1},{t,-3,3},*)
(*RegionFunction->Function[{x,z,t},#[[2]]],RegionBoundaryStyle->None,*)
(*AxesLabel->Automatic,BoxRatios->Automatic,*)
(*MeshFunctions->{Function[{x,z,t},z]},Mesh->11,(*MeshStyle\[Rule]Directive[Thickness[.0015]],*)*)
(*ContourStyle->{*)
(*Charting`ResolvePlotTheme[Automatic,ContourPlot3D]*)
(*//OptionValue[#,Method]&*)
(*//OptionValue[#,"DefaultPlotStyle"]&*)
(*//Extract[#[[3]]]*)
(*//Append[Opacity[.6]](*,*)
(*Directive[White,Opacity[.1],Lighting->"ThreePoint"]*)*)
(*},BoundaryStyle->None*)
(*]&/@{*)
(*{lightSheets[[1]],(0<=t<=1&&z>=0)||(t<=0&&z<=0),1},*)
(*{lightSheets[[2]],(0>=t>=-1&&z>=0)||(t>=0&&z<=0),2}*)
(*}//Show*)


(* ::Item:: *)
(*Front view:*)


(* ::Item:: *)
(*Back view:*)


(* ::Subsection::Closed:: *)
(*z sections of the bulk Rindler decomposition*)


(* ::Item:: *)
(*z sections:*)


(* ::Input:: *)
(*norm[lower[\[Xi]]]//Factor//FullSimplify*)
(*%/.sameL/.l->2//FullSimplify*)
(*(y^2 %)/\[Pi]^2/.uv2xt*)
(**)
(*timelikeFlow=RegionPlot[*)
(*%<=0/.y->z^2 Sign[z]/.z->#//Evaluate,*)
(*{x,-3,3},{t,-3,3},*)
(*PlotLabel->HoldForm[z==#],*)
(*PlotPoints->5,MaxRecursion->5,*)
(*Mesh->None,Evaluate[*)
(*If[#==10.^-16||#==-.1,*)
(*FrameLabel->{x,t},*)
(*FrameLabel->{x,None}]//ReleaseHold*)
(*],*)
(*ImageSize->{Automatic,240},*)
(*PlotRangePadding->None,ImagePadding->{{Automatic,5}, {Automatic,5}},*)
(*PlotStyle->Lighter[RGBColor[0.368417, 0.506779, 0.709798],.7]*)
(*]&/@{*)
(*10.^-16,.2,.7,1,1.5,*)
(*-.1,-.3,-.5,-1,-2*)
(*}//Partition[#,5]&//Grid*)
(**)
(*exportPlot[timelikeFlow,"../img/"]*)


(* ::Section:: *)
(*Epilog*)


(* ::Input::Initialization:: *)
saveScript[]:=FrontEndExecute[FrontEndToken[
EvaluationNotebook[],"Save",{
NotebookDirectory[]<>FileBaseName[NotebookFileName[]]<>".wl",
"Script"
}]];

saveScript[]
