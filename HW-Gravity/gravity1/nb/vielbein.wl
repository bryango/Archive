#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Input::Initialization:: *)
SetDirectory[NotebookDirectory[]]


(* ::Input::Initialization:: *)
coord={t,r,\[Theta],\[Phi]};

metricSign=-1;
metric=DiagonalMatrix[{f[r],h[r],r^2,r^2 Sin[\[Theta]]^2}];

Sqrt[metric//Diagonal]//Simplify[#,{r>0,Sin[\[Theta]]>0}]&;
e=%*(\[DoubleStruckD]/@coord)


(* ::Input::Initialization:: *)
<<diffgeoM`


(* ::Item:: *)
(*We use `diffgeo` to compute the exterior derivatives, and to confirm our final result.*)


(* ::Subitem:: *)
(*`diffgeoM` is a customized version of `diffgeo` and can be obtained from:*)


(* ::Subsubitem:: *)
(*https://github.com/bryango/diffgeoM*)


(* ::Input::Initialization:: *)
de=extD/@e


(* ::Item:: *)
(*The connection form \!\(\*SubsuperscriptBox[\(\[Omega]\), \(\[Mu]\ b\), \(a\)]\) is denoted as `wab\[Mu]`. Note that we've moved the \[Mu] index to the last for better readability.*)


(* ::Input::Initialization:: *)
w=Table[

Sum[
ToExpression[
"w"
<>ToString[coord[[Min[a,b]]]]
<>ToString[coord[[Max[a,b]]]]
<>ToString[u]
]\[DoubleStruckD][u]Sign[b-a],
{u,coord}],

{a,dim},{b,dim}]


(* ::Item:: *)
(*Independent components in the connection form:*)


(* ::Input::Initialization:: *)
wvars=Table[

ToExpression[
"w"
<>ToString[coord[[Min[a,b]]]]
<>ToString[coord[[Max[a,b]]]]
<>ToString[u]
]Sign[b-a]HeavisideTheta[b-a],

{a,dim},{b,dim},{u,coord}]/.(0->Nothing)//Flatten


(* ::Item:: *)
(*Cartan's structure equations:*)


(* ::Input::Initialization:: *)
rhs=Function[a,
Sum[-w[[a,b]]\[Wedge]e[[b]],{b,dim}]
]/@Range[dim];

eqs=Thread[de==Simplify[rhs]]


(* ::Input:: *)
(*\[DoubleStruckD]@@#&/@Subsets[coord,{2}]*)


(* ::Item:: *)
(*Solving the structure equations by matching the \[DoubleStruckD][a,b] coefficients:*)


(* ::Input::Initialization:: *)
sol={};

coefeqs=Function[a,
(eqs[[a]]/.\[DoubleStruckD]@@#->1/.\[DoubleStruckD]->Function[x,0])&/@Subsets[coord,{2}]//Simplify
]/@Range[dim];

sol=Join[sol,{wtrt->Derivative[1][f][r]/(2Sqrt[f[r]] Sqrt[h[r]]),wt\[Theta]t->0,wt\[Phi]t->0,wtrr->0,wr\[Theta]r->0,wt\[Theta]\[Theta]->0,wr\[Theta]\[Theta]->-1/Sqrt[h[r]],w\[Theta]\[Phi]\[Theta]->0,wt\[Phi]\[Phi]->0,wr\[Phi]\[Phi] ->-Sin[\[Theta]]/Sqrt[h[r]],w\[Theta]\[Phi]\[Phi]->-Cos[\[Theta]]}];

coefeqs=coefeqs//.sol/.True->Nothing//Flatten
Length[coefeqs]


(* ::Item:: *)
(*There are 13 remaining equations involving multiple w components*)


(* ::Input::Initialization:: *)
(sol/.Rule->List//Transpose)[[1]]

unknowns=Complement[wvars,%]
Length[unknowns]


(* ::Item:: *)
(*There are precisely 13 unknown w components, so the solution is unique -- they are all 0's:*)


(* ::Input::Initialization:: *)
Solve[coefeqs,unknowns]

fullsol=Join[sol,First[%]]


(* ::Input::Initialization:: *)
wsol=w//.fullsol;
wsol//MatrixForm


(* ::Input::Initialization:: *)
dw=Map[extD,wsol,{2}]


(* ::Input::Initialization:: *)
w\[CapitalLambda]w=Table[
Sum[wsol[[a,c]]\[Wedge]wsol[[c,b]],{c,dim}],
{a,dim},{b,dim}
]


(* ::Item:: *)
(*Curvature form:*)


(* ::Input::Initialization:: *)
\[CapitalOmega]=dw+w\[CapitalLambda]w;
\[CapitalOmega]//MatrixForm


(* ::Item:: *)
(*The components are written in \[DifferentialD]x^\[Mu] basis. Convert to e^a basis:*)


(* ::Input::Initialization:: *)
eebasis=Function[{a,b},ToExpression["e"<>ToString[a]<>ToString[b]]]@@@Subsets[coord,{2}]

Wedge@@@Subsets[e,{2}]

eebasisValues=Thread[eebasis->%]

ruleTeebasis=Solve[(eebasis/.eebasisValues)==eebasis,\[DoubleStruckD]@@@Subsets[coord,{2}]]//Flatten


(* ::Item:: *)
(*Curvature form \[CapitalOmega] in  e^a basis:*)


(* ::Input::Initialization:: *)
omegaIeebasis=\[CapitalOmega]/.ruleTeebasis//Simplify;
%//MatrixForm


(* ::Input:: *)
(*omegaIeebasis[[1,2]][[1;;]]//Expand*)


(* ::Item:: *)
(*Compare with the Ricci tensor:*)


(* ::Input::Initialization:: *)
tRicci//Expand//Diagonal


(* ::Input:: *)
(*<<"Physica/MathUtils.wl";*)
(*saveScript[]*)
